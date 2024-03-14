use core::fmt;

use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::*;

#[derive(Debug, Clone)]
pub struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error: {}", self.message)
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

// TODO reorder internal functions
impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            peek_token: Token::Eof,
            errors: vec![],
        };
        // set current and peek tokens
        parser.next();
        parser.next();
        parser
    }

    fn parse_function_literal(&mut self) -> Expression {
        let function_token = self.current_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            // TODO add parse error
            panic!("function literal should contain a left parenthesis");
        }
        
        let arguments = self.parse_function_arguments();
        if !self.expect_peek(&Token::Lbrace) {
            // TODO add parse error
            panic!("function literal should contain a block statement after arguments");
        }
        let body = self.parse_block_statement();

        Expression::Function {
            token: function_token,
            arguments,
            body,
        }
    }

    fn parse_function_arguments(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            // TODO this could be a expect_peek
            self.next();
            return identifiers;
        }

        self.next();
        if let Token::Identifier(value) = &self.current_token {
            identifiers.push(Identifier {
                token: self.current_token.clone(),
                value: value.clone(),
            });
        }

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            if let Token::Identifier(value) = &self.current_token {
                identifiers.push(Identifier {
                    token: self.current_token.clone(),
                    value: value.clone(),
                });
            }
        }

        if !self.expect_peek(&Token::Rparen) {
            panic!("function arguments must end with right parenthesis");
        }

        identifiers
    }

    fn parse_if_expression(&mut self) -> Expression {
        let current_token = self.current_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            // TODO update this to be a parse error
            panic!("If expressions should be accompanied by a left parenthesis");
        }

        self.next();
        let condition = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(&Token::Rparen) {
            // TODO update this to be a parse error
            panic!("The if expression should end with a right parenthesis");
        }
        if !self.expect_peek(&Token::Lbrace) {
            // TODO update this to be a parse error
            panic!("The consequence should start with a left brace");
        }
        let consequence = self.parse_block_statement();

        let mut alternative: Option<BlockStatement> = None;
        // TODO add support for else if
        if self.peek_token_is(&Token::Else) {
            self.next();
            if !self.expect_peek(&Token::Lbrace) {
                panic!("Else block should start with left brace");
            }

            alternative = Some(self.parse_block_statement());
        }

        Expression::If {
            token: current_token,
            condition: Box::from(condition.unwrap()),
            consequence,
            alternative,
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let current_token = self.current_token.clone();
        self.next();

        let mut statements = Vec::new();
        while self.current_token != Token::Eof && !self.current_token_is(Token::Rbrace) {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next();
        }

        return BlockStatement {
            token: current_token,
            statements,
        };
    }

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next();

        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&Token::Rparen) {
            // TODO add error
            panic!("Missing right paren")
        }
        expression.unwrap()
    }

    fn parse_identifier(&mut self) -> Expression {
        let value = match &self.current_token {
            Token::Identifier(value) => value.clone(),
            _ => panic!("expected an identifier token"),
        };
        Expression::Identifier {
            token: self.current_token.clone(),
            value,
        }
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let value = match self.current_token {
            Token::Integer(value) => value,
            _ => panic!("expected integer token"),
        };
        Expression::Integer {
            token: self.current_token.clone(),
            value,
        }
    }

    fn parse_boolean(&mut self) -> Expression {
        Expression::Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(Token::True),
        }
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let current_token = self.current_token.clone();
        self.next();
        
        Expression::Prefix {
            token: current_token.clone(),
            operator: format!("{}", current_token),
            right: Box::from(self.parse_expression(Precedence::Prefix).unwrap_or_else(||
                panic!("prefix expressions should have right")
            )),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let current_token = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next();

        Expression::Infix {
            token: current_token.clone(),
            operator: format!("{}", current_token),
            left: Box::from(left),
            right: Box::from(self.parse_expression(precedence).unwrap()),
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut arguments = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            self.next();
            return arguments;
        }

        self.next();
        // TODO add error possibility
        arguments.push(self.parse_expression(Precedence::Lowest).unwrap());

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            arguments.push(self.parse_expression(Precedence::Lowest).unwrap());
        }

        if !self.expect_peek(&Token::Rparen) {
            // TODO parse error
            panic!()
        }

        arguments
    }

    fn parse_call_expression(&mut self, left: Expression) -> Expression {
        let s = Expression::Call {
            token: self.current_token.clone(),
            function: Box::new(left),
            arguments: self.parse_call_arguments(),
        };
        s
    }

    pub fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn next(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }


    fn peek_error(&mut self, expected_token: &Token) {
        self.errors.push(ParseError { 
            message: format!("expected next token to be {:?} but got {:?}", expected_token, self.peek_token).to_owned() 
        });
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        self.peek_token == *token
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if self.peek_token_is(token) {
            self.next();
            return true;
        }
        self.peek_error(token);
        false
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        self.current_token == token
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();

        // TODO understand the need of this clone
        let identifier = match self.peek_token.clone() {
            Token::Identifier(value) => {
                self.next();
                Identifier {
                    token: self.current_token.clone(),
                    value,
                }
            },
            _ => return None,
        };

        if !self.expect_peek(&Token::Assign) {
            return None;
        }
        self.next();

        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(Statement::Let { name: identifier, token, value })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        self.next();

        let value = self.parse_expression(Precedence::Lowest);
        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }
        Some(Statement::Return { token, value })
    }

    fn precedence_from_token(&self, token: &Token) -> Precedence {
        match token {
            Token::Lparen => Precedence::Call,
            Token::Asterisk | Token::Slash => Precedence::Product,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Gt | Token::Lt => Precedence::LessGreater,
            Token::Eq | Token::Neq => Precedence::Equals,
            _ => Precedence::Lowest
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.current_token)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        
        // prefix expressions
        let mut left_expression = match &self.current_token {
            Token::Function => self.parse_function_literal(),
            Token::If => self.parse_if_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Identifier(_) => self.parse_identifier(),
            Token::Integer(_) => self.parse_integer_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            _ => {
                // TODO add parse error here
                return None
            },
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            
            // infix expressions
            left_expression = match self.peek_token {
                Token::Lparen => {
                    self.next();
                    self.parse_call_expression(left_expression)
                },
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::Neq
                | Token::Lt
                | Token::Gt => {
                    self.next();
                    self.parse_infix_expression(left_expression)
                },
                _ => {
                    return Some(left_expression);
                }
            }
        }
        Some(left_expression)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expression_token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }
        Some(Statement::Expression {
            token: expression_token,
            expression,
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut statements = Vec::new();
        while self.current_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next();
        }
        Program(statements)
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    
    fn check_parse_errors(p: Parser) {
        let errors = p.get_errors();
        if errors.len() == 0 {
            return;
        }

        eprintln!("got {} parse errors", errors.len());
        for error in errors {
            eprintln!("{}", error.message);
        }
        panic!();
    }

    fn identifier(value: &str) -> Expression {
        Expression::Identifier {
            token: Token::Identifier(value.to_owned()),
            value: value.to_owned(),
        }
    }

    fn boolean(value: bool) -> Expression {
        Expression::Boolean {
            token: if value { Token::True } else { Token::False },
            value,
        }
    }

    fn integer(value: i64) -> Expression {
        Expression::Integer {
            token: Token::Integer(value),
            value,
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
            let y = true;
            let foobar = y;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Let {
                    token: Token::Let,
                    name: Identifier { token: Token::Identifier("x".to_owned()), value: "x".to_owned() },
                    value: Some(integer(5)),
                },
                Statement::Let {
                    token: Token::Let,
                    name: Identifier { token: Token::Identifier("y".to_owned()), value: "y".to_owned() },
                    value: Some(boolean(true)),
                },
                Statement::Let {
                    token: Token::Let,
                    name: Identifier { token: Token::Identifier("foobar".to_owned()), value: "foobar".to_owned() },
                    value: Some(identifier("y")),
                },
            ]),
            program
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
            return false;
            return xy;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Return {
                    token: Token::Return,
                    value: Some(integer(5)),
                },
                Statement::Return{
                    token: Token::Return,
                    value: Some(boolean(false)),
                },
                Statement::Return {
                    token: Token::Return,
                    value: Some(identifier("xy")),
                },
            ]),
            program
        );
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".as_bytes();
        let expected_value = "foobar";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Expression {
                    token: Token::Identifier(expected_value.to_owned()),
                    expression: Some(identifier(expected_value)),
                },
            ]),
            program
        );
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Expression {
                    token: Token::Integer(5),
                    expression: Some(integer(5)),
                },
            ]),
            program
        );
    }

    #[test]
    fn test_boolean_expression() {
        let input = "true; false;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Expression {
                    token: Token::True,
                    expression: Some(boolean(true)),
                },
                Statement::Expression {
                    token: Token::False,
                    expression: Some(boolean(false)),
                },
            ]),
            program
        );
    }

    #[test]
    fn test_prefix_expression() {
        let tests = vec![
            (
                "!5;",
                Program(vec![
                    Statement::Expression {
                        token: Token::Bang,
                        expression: Some(Expression::Prefix {
                            token: Token::Bang,
                            operator: "!".to_owned(),
                            right: Box::from(integer(5)),
                        })
                    },
                ]),
            ),
            (
                "-15;",
                Program(vec![
                    Statement::Expression {
                        token: Token::Minus,
                        expression: Some(Expression::Prefix {
                            token: Token::Minus,
                            operator: "-".to_owned(),
                            right: Box::from(integer(15)),
                        })
                    },
                ]),
            )
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);

            assert_eq!(expected, program);
        }
    }

    fn infix_template(left: i64, op: &str, op_token: Token, right: i64) -> Vec<Statement> {
        vec![Statement::Expression {
            token: Token::Integer(left),
            expression: Some(Expression::Infix {
                token: op_token,
                operator: op.to_owned(),
                left: Box::from(integer(left)),
                right: Box::from(integer(right)),
            })
        }]
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            ("5 + 5;", Program(infix_template(5, "+", Token::Plus, 5))),
            ("5 - 5;", Program(infix_template(5, "-", Token::Minus, 5))),
            ("5 * 5;", Program(infix_template(5, "*", Token::Asterisk, 5))),
            ("5 / 5;", Program(infix_template(5, "/", Token::Slash, 5))),
            ("5 > 5;", Program(infix_template(5, ">", Token::Gt, 5))),
            ("5 < 5;", Program(infix_template(5, "<", Token::Lt, 5))),
            ("5 == 5;", Program(infix_template(5, "==", Token::Eq, 5))),
            ("5 != 5;", Program(infix_template(5, "!=", Token::Neq, 5))),
        ]; 
        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);

            assert_eq!(expected, program);
        }
    }
 
    #[test]
    fn test_if_statement() {
        let input = "if (x < y) { x }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::If,
                expression: Some(Expression::If {
                    token: Token::If,
                    condition: Box::from(Expression::Infix {
                        token: Token::Lt,
                        operator: "<".to_owned(),
                        left: Box::from(identifier("x")),
                        right: Box::from(identifier("y")),
                    }),
                    consequence: BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![
                            Statement::Expression {
                                token: Token::Identifier("x".to_owned()), 
                                expression: Some(identifier("x")),
                            },
                        ],
                    },
                    alternative: None,
                })
           }]),
            program
        );
    }
 
    #[test]
    fn test_if_else_statement() {
        let input = "if (x < y) { x } else { y }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::If,
                expression: Some(Expression::If {
                    token: Token::If,
                    condition: Box::from(Expression::Infix {
                        token: Token::Lt,
                        operator: "<".to_owned(),
                        left: Box::from(identifier("x")),
                        right: Box::from(identifier("y")),
                    }),
                    consequence: BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![
                            Statement::Expression {
                                token: Token::Identifier("x".to_owned()), 
                                expression: Some(identifier("x")),
                            },
                        ],
                    },
                    alternative: Some(BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![
                            Statement::Expression {
                                token: Token::Identifier("y".to_owned()), 
                                expression: Some(identifier("y")),
                            },
                        ],
                    }),
                })
           }]),
            program
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Expression {
                    token: Token::Function,
                    expression: Some(Expression::Function {
                        token: Token::Function,
                        arguments: vec![
                            Identifier { token: Token::Identifier("x".to_owned()), value: "x".to_owned() },
                            Identifier { token: Token::Identifier("y".to_owned()), value: "y".to_owned() },
                        ],
                        body: BlockStatement {
                            token: Token::Lbrace,
                            statements: vec![
                                Statement::Expression {
                                    token: Token::Identifier("x".to_owned()),
                                    expression: Some(Expression::Infix {
                                        token: Token::Plus,
                                        operator: "+".to_owned(),
                                        left: Box::new(identifier("x")),
                                        right: Box::new(identifier("y")),
                                    })
                                }
                            ],
                        }
                    }),
                }
            ]),
            program
        );
    }

    #[test]
    fn test_function_call() {
        let input = "add(1, 2 * 3, 4 + 5)".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Expression {
                    token: Token::Identifier("add".to_owned()),
                    expression: Some(Expression::Call {
                        token: Token::Lparen,
                        function: Box::new(identifier("add")),
                        arguments: vec![
                            Expression::Integer { token: Token::Integer(1), value: 1 },
                            Expression::Infix {
                                token: Token::Asterisk,
                                operator: "*".to_owned(),
                                left: Box::new(integer(2)),
                                right: Box::new(integer(3)),
                            },
                            Expression::Infix {
                                token: Token::Plus,
                                operator: "+".to_owned(),
                                left: Box::new(integer(4)),
                                right: Box::new(integer(5)),
                            },
                        ],
                    }),
                }
            ]),
            program
        );
    }

    #[test]
    fn test_operator_precedence() {
        let tests = vec![
    		("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
        ];
        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);

            assert_eq!(format!("{}", program), expected);
        }
    }

    #[test]
    fn test_string() {
        let program = Program(vec![
            Statement::Let {
                token: Token::Let,
                name: Identifier {
                    token: Token::Identifier("my_var".to_owned()),
                    value: "my_var".to_owned(),
                },
                value: Some(identifier("another_var")),
        }]);
        assert_eq!(format!("{}", program), "let my_var = another_var;")
    }
}

