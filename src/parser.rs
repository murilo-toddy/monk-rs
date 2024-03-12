use core::fmt;
use std::mem::{discriminant, Discriminant};

use crate::lexer::Lexer;
use crate::token::Token;
use crate::ast::*;

#[derive(Debug, Clone)]
struct ParseError {
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error: {}", self.message)
    }
}

#[derive(Eq, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    LessGreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -x or !x
    Call = 7,        // func(x)
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

// TODO reorder internal functions
impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser {
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

    fn parse_function_literal(&mut self) -> Box<dyn Expression> {
        let function_token = self.current_token.clone();
        println!("{:?} {:?}", self.current_token, self.peek_token);
        if !self.expect_peek(&Token::Lparen) {
            // TODO add parse error
            panic!("function literal should contain a left parenthesis");
        }
        
        let arguments = self.parse_function_arguments();
        println!("{:?} {:?}", self.current_token, self.peek_token);
        if !self.expect_peek(&Token::Lbrace) {
            // TODO add parse error
            panic!("function literal should contain a block statement after arguments");
        }
    
        let body = self.parse_block_statement();
        return Box::from(FunctionLiteral {
            token: function_token,
            arguments,
            body,
        });
    }

    fn parse_function_arguments(&mut self) -> Vec<Identifier> {
        let mut identifiers = Vec::new();

        if self.peek_token_is(&Token::Rparen) {
            // TODO this could be a expect_peek
            self.next();
            return identifiers;
        }

        self.next();
        if let Token::Ident(value) = &self.current_token {
            identifiers.push(Identifier {
                token: self.current_token.clone(),
                value: value.clone(),
            });
        }

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            if let Token::Ident(value) = &self.current_token {
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

    fn parse_if_expression(&mut self) -> Box<dyn Expression> {
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

        return Box::from(IfExpression {
            token: current_token,
            condition: condition.unwrap(),
            consequence,
            alternative,
        });
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

    fn parse_grouped_expression(&mut self) -> Box<dyn Expression> {
        self.next();

        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&Token::Rparen) {
            // TODO come back to this, this is awful
            panic!("Missing right paren")
        }
        expression.unwrap()
    }

    fn parse_identifier(&mut self) -> Box<dyn Expression> {
        let value = match &self.current_token {
            Token::Ident(value) => value.clone(),
            _ => panic!("expected an identifier token"),
        };
        Box::new(Identifier {
            token: self.current_token.clone(),
            value,
        })
    }

    fn parse_integer_literal(&mut self) -> Box<dyn Expression> {
        let value = match self.current_token {
            Token::Integer(value) => value,
            _ => panic!("expected integer token"),
        };
        Box::new(IntegerLiteral {
            token: self.current_token.clone(),
            value,
        })
    }

    fn parse_boolean(&mut self) -> Box<dyn Expression> {
        Box::from(Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(Token::True),
        })
    }

    fn parse_prefix_expression(&mut self) -> Box<dyn Expression> {
        let current_token = self.current_token.clone();
        self.next();
        
        Box::from(PrefixExpression {
            token: current_token.clone(),
            operator: current_token.to_string(),
            right: self.parse_expression(Precedence::Prefix).unwrap_or_else(||
                panic!("prefix expressions should have right")
            ),
        })
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let current_token = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next();

        Box::from(InfixExpression {
            token: current_token.clone(),
            operator: current_token.to_string(),
            left,
            right: self.parse_expression(precedence).unwrap(),
        })
    }

    fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn next(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next();
    }


    fn token_types_match(&self, t1: &Token, t2: &Token) -> bool {
        std::mem::discriminant(t1) == std::mem::discriminant(t2)
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
//         if let Some(current_token) = &self.current_token {
//             if self.token_types_match(current_token, &token) {
//                 return true;
//             }
//         }
//         false
    }

    fn parse_let_statement(&mut self) -> Option<Box<LetStatement>> {
        let token = self.current_token.clone();

        // TODO understand the need of this clone
        let identifier = match self.peek_token.clone() {
            Token::Ident(value) => {
                self.next();
                Identifier {
                    token: self.current_token.clone(),
                    value,
                }
            },
            _ => return None,
        };
//         if !self.expect_peek(&Token::Ident(String::from(""))) {
//             return None;
//         }

//         let identifier = match self.current_token.take() {
//             Some(token) => {
//                 match token.clone() {
//                     Token::Ident(value) => Identifier { token, value },
//                     _ => panic!("branch cannot happen")
//                 }
//             },
//             _ => panic!("branch cannot happen")
//         };

        if !self.expect_peek(&Token::Assign) {
            return None;
        }
        
        // TODO skipping expressions until a semicolon is found
        while !self.current_token_is(Token::Semicolon) {
            self.next();
        }

        Some(Box::from(LetStatement {
            name: identifier,
            token,
            value: None,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<ReturnStatement>> {
        let token = self.current_token.clone();
        self.next();

        // TODO skipping expressions until a semicolon is found
        while !self.current_token_is(Token::Semicolon) {
            self.next();
        }
        Some(Box::from(ReturnStatement {
            token,
            value: None,
        }))
    }

    fn precedence_from_token(&self, token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Gt | Token::Lt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Asterisk | Token::Slash => Precedence::Product,
            _ => Precedence::Lowest
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.current_token)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<dyn Expression>> {
        
        // prefix expressions
        let mut left_expression = match &self.current_token {
            Token::Function => self.parse_function_literal(),
            Token::If => self.parse_if_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Ident(_) => self.parse_identifier(),
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

    fn parse_expression_statement(&mut self) -> Option<Box<ExpressionStatement>> {
        let expression_token = self.current_token.clone();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }
        Some(Box::from(ExpressionStatement {
            token: expression_token,
            expression,
        }))
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Token::Let => self.parse_let_statement().map(|s| s as Box<dyn Statement>),
            Token::Return => self.parse_return_statement().map(|s| s as Box<dyn Statement>),
            _ => self.parse_expression_statement().map(|s| s as Box<dyn Statement>),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        while self.current_token != Token::Eof {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next();
        }
        Program { statements }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;
    
    // TODO these tests are out of hand
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

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 3 {
            panic!("expected program to contain 3 elements but got {}", program.statements.len())
        }

        let expected_identifiers = vec![
            "x".to_string(),
            "y".to_string(),
            "foobar".to_string(),
        ];

        for (i, expected_identifier) in expected_identifiers.iter().enumerate() {
            match program.statements.get(i) {
                Some(statement) => {
                    if *statement.get_token() != Token::Let {
                        panic!("expected statement with token <Token::Let> but got <{:?}>", statement.get_token());
                    }
                    match statement.as_any().downcast_ref::<LetStatement>() {
                        None => panic!("expected LetStatement but got {:?}", statement),
                        Some(let_statement) => {
                            if let_statement.name.value != *expected_identifier {
                                panic!("expected identifier <{}> for let statement but got <{}>", *expected_identifier, let_statement.name.value)
                            }
                        },
                    }
                }
                None => panic!("index out of bounds fo statements"),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
return 10;
return 838383;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 3 {
            panic!("expected program to contain 3 elements but got {}", program.statements.len())
        }

        for statement in program.statements {
            if *statement.get_token() != Token::Return {
                panic!("expected statement with token <Token::Return> but got <{:?}>", statement.get_token());
            }
            if statement.as_any().downcast_ref::<ReturnStatement>().is_none() {
                panic!("expected ReturnStatement but got {:?}", statement);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 1 {
            panic!("expected program to contain 1 elements but got {}", program.statements.len())
        }

        let statement = program.statements.first().unwrap();
        let expression = &statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
            .expression;

        if let Some(expression) = expression {
            let identifier = expression
                .as_any()
                .downcast_ref::<Identifier>()
                .unwrap_or_else(|| panic!("expected Identifier"));

            assert_eq!(identifier.value, "foobar");
            match &identifier.token {
                Token::Ident(value) => assert_eq!(value, "foobar"),
                _ => panic!("identifier contains token with different type {:?}", identifier.token),
            }
        }
    }

    // TODO refactor test structure
    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 1 {
            panic!("expected program to contain 1 elements but got {}", program.statements.len())
        }

        let statement = program.statements.first().unwrap();
        let expression = &statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
            .expression;

        if let Some(expression) = expression {
            let integer = expression
                .as_any()
                .downcast_ref::<IntegerLiteral>()
                .unwrap_or_else(|| panic!("expected IntegerLiteral"));

            assert_eq!(integer.value, 5);
            match &integer.token {
                Token::Integer(value) => assert_eq!(*value, 5),
                _ => panic!("integer contains token with different type {:?}", integer .token),
            }
        }
    }

    #[test]
    fn test_boolean_expression() {
        let tests = [("true;", true), ("false;", false)];
        
        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser);

            if program.statements.len() != 1 {
                panic!("expected program to contain 1 elements but got {}", program.statements.len())
            }

            let statement = program.statements.first().unwrap();
            let expression = &statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
                .expression;

            if let Some(expression) = expression {
                let boolean = expression
                    .as_any()
                    .downcast_ref::<Boolean>()
                    .unwrap_or_else(|| panic!("expected Boolean"));

                match &boolean.token {
                    Token::True => assert_eq!(expected, true),
                    Token::False => assert_eq!(expected, false),
                    _ => panic!("boolean contains token with different type {:?}", boolean.token),
                }
                assert_eq!(boolean.value, expected);
            }
        }
    }

    #[test]
    fn test_prefix_expression() {
        let tests = vec![
            ("!5;", "!", 5),
            ("-15;", "-", 15),
        ];

        for (input, op, int) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser);

            if program.statements.len() != 1 {
                panic!("expected program to contain 1 elements but got {}", program.statements.len())
            }

            let statement = program.statements.first().unwrap();
            let expression = &statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
                .expression;

            if let Some(expression) = expression {
                let prefix = expression
                    .as_any()
                    .downcast_ref::<PrefixExpression>()
                    .unwrap_or_else(|| panic!("expected PrefixExpression"));

                assert_eq!(prefix.operator, op);
                test_integer_literal(&prefix.right, int);
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, left, op, right) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser);

            if program.statements.len() != 1 {
                panic!("expected program to contain 1 elements but got {}", program.statements.len())
            }

            let statement = program.statements.first().unwrap();
            let expression = &statement
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
                .expression;

            if let Some(expression) = expression {
                let infix = expression
                    .as_any()
                    .downcast_ref::<InfixExpression>()
                    .unwrap_or_else(|| panic!("expected InfixExpression"));

                test_integer_literal(&infix.left, left);
                assert_eq!(infix.operator, op);
                test_integer_literal(&infix.right, right);
            }
        }
    }

    fn test_integer_literal(exp: &Box<dyn Expression>, int: i64) {
        let int_exp = &exp
            .as_any()
            .downcast_ref::<IntegerLiteral>()
            .unwrap_or_else(|| panic!("expected IntegerLiteral"));
        
        assert_eq!(int_exp.value, int);
        assert_eq!(int_exp.token, Token::Integer(int));
    }

    #[test]
    fn test_if_statement() {
        let input = "if (x < y) { x }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        assert_eq!(program.statements.len(), 1,
            "expected program to contain 1 elements but got {}", program.statements.len());

        let statement = program.statements.first().unwrap();
        let expression = &statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
            .expression;

        if let Some(expression) = expression {
            let if_expression = expression
                .as_any()
                .downcast_ref::<IfExpression>()
                .unwrap_or_else(|| panic!("expected IfExpression"));

            let condition = if_expression 
                .condition
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap_or_else(|| panic!("expected InfixExpression"));

            check_identifier_equals(&condition.left, "x");
            assert_eq!(condition.operator, "<");
            check_identifier_equals(&condition.right, "y");
            
            assert_eq!(if_expression.consequence.statements.len(), 1, 
                       "consequence should have 1 statement but got {}", if_expression.consequence.statements.len());

            let consequence = if_expression 
                .consequence
                .statements
                .first()
                .unwrap()
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement"));

            check_identifier_equals(consequence.expression.as_ref().unwrap(), "x");

            assert!(if_expression.alternative.is_none(), "IfStatement should have no alternative");
        }
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (x < y) { x } else { y }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 1 {
            panic!("expected program to contain 1 elements but got {}", program.statements.len())
        }

        let statement = program.statements.first().unwrap();
        let expression = &statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
            .expression;

        if let Some(expression) = expression {
            let if_expression = expression
                .as_any()
                .downcast_ref::<IfExpression>()
                .unwrap_or_else(|| panic!("expected IfExpression"));

            let condition = if_expression 
                .condition
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap_or_else(|| panic!("expected InfixExpression"));

            check_identifier_equals(&condition.left, "x");
            assert_eq!(condition.operator, "<");
            check_identifier_equals(&condition.right, "y");
            
            assert_eq!(if_expression.consequence.statements.len(), 1, 
                       "consequence should have 1 statement but got {}", if_expression.consequence.statements.len());

            let consequence = if_expression 
                .consequence
                .statements
                .first()
                .unwrap()
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement"));
            check_identifier_equals(consequence.expression.as_ref().unwrap(), "x");

            assert!(if_expression.alternative.is_some());
            let alternative = if_expression 
                .alternative
                .as_ref()
                .unwrap()
                .statements
                .first()
                .unwrap()
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement"));
            check_identifier_equals(alternative.expression.as_ref().unwrap(), "y");
        }
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x, y) { x + y; }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(parser);

        if program.statements.len() != 1 {
            panic!("expected program to contain 1 elements but got {}", program.statements.len())
        }

        let statement = program.statements.first().unwrap();
        let expression = &statement
            .as_any()
            .downcast_ref::<ExpressionStatement>()
            .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", statement))
            .expression;

        if let Some(expression) = expression {
            let func = expression
                .as_any()
                .downcast_ref::<FunctionLiteral>()
                .unwrap_or_else(|| panic!("expected FunctionLiteral"));

            assert_eq!(func.arguments.len(), 2, "function must contain 2 arguments");
            let first_arg = Box::from(func.arguments[0].clone()) as Box<dyn Expression>;
            check_identifier_equals(&first_arg, "x");
            let second_arg = Box::from(func.arguments[1].clone()) as Box<dyn Expression>;
            check_identifier_equals(&second_arg, "y");
            
            assert_eq!(func.body.statements.len(), 1, 
                       "function body should have 1 statement but got {}", func.body.statements.len());

            let statement = func
                .body
                .statements
                .first()
                .unwrap()
                .as_any()
                .downcast_ref::<ExpressionStatement>()
                .unwrap_or_else(|| panic!("expected ExpressionStatement but got {:?}", func));


            let infix = statement
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<InfixExpression>()
                .unwrap_or_else(|| panic!("expected InfixExpression but got {:?}", func));

            check_identifier_equals(&infix.left, "x");
            assert_eq!(infix.operator, "+");
            check_identifier_equals(&infix.right, "y");
        }
    }

    fn check_identifier_equals(actual: &Box<dyn Expression>, expected: &str) {
        let actual_ident = actual 
            .as_any()
            .downcast_ref::<Identifier>()
            .unwrap_or_else(|| panic!("expected Identifer"));

        assert_eq!(actual_ident.value, expected.to_owned());
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
            ("3 + 4;; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
        ];
        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            check_parse_errors(parser);
            assert_eq!(program.to_string(), expected);
        }
    }

    #[test]
    fn test_string() {
        let program = Program {
            statements: vec![
                Box::from(LetStatement {
                    token: Token::Let,
                    name: Identifier {
                        token: Token::Ident("my_var".to_owned()),
                        value: "my_var".to_owned(),
                    },
                    value: Some(Box::from(Identifier {
                        token: Token::Ident("another_var".to_owned()),
                        value: "another_var".to_owned(),
                    })),
                }) as Box<dyn Statement>,
            ],
        };

        assert_eq!(program.to_string(), "let my_var = another_var;", "to_string() method should work properly")
    }
}

