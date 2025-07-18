use std::fmt;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

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

    fn next(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        self.current_token == token
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

    fn push_parse_error(&mut self, message: &str) {
        self.errors.push(ParseError {
            message: message.to_owned(),
        })
    }

    fn peek_error(&mut self, expected_token: &Token) {
        self.push_parse_error(&format!(
            "unexpected '{}', should be '{}'",
            self.peek_token.as_string(),
            expected_token.as_string()
        ));
    }

    pub fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn precedence_from_token(&self, token: &Token) -> Precedence {
        match token {
            Token::Lbracket => Precedence::Index,
            Token::Lparen => Precedence::Call,
            Token::Asterisk | Token::Slash | Token::Percentage => Precedence::Product,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Gt | Token::Lt | Token::Gte | Token::Lte => Precedence::LessGreater,
            Token::Or => Precedence::Or,
            Token::And => Precedence::And,
            Token::BitOr => Precedence::BitOr,
            Token::BitAnd => Precedence::BitAnd,
            Token::BitXor => Precedence::BitXor,
            Token::BitShiftLeft | Token::BitShiftRight => Precedence::BitShift,
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Assign => Precedence::Assign,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        self.precedence_from_token(&self.current_token)
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

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.current_token.clone();
        let identifier = match self.peek_token.clone() {
            Token::Identifier(value) => {
                self.next();
                Identifier {
                    token: self.current_token.clone(),
                    value,
                }
            }
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

        Some(Statement::Let {
            name: identifier,
            token,
            value,
        })
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

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut list = Vec::new();
        if self.peek_token_is(&end) {
            self.next();
            return Some(list);
        }

        self.next();
        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.expect_peek(&end) {
            return None;
        }
        Some(list)
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let elements = self.parse_expression_list(Token::Rbracket)?;
        Some(Expression::Array {
            token: Token::Lbracket,
            elements,
        })
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let mut pairs: Vec<(Expression, Expression)> = Vec::new();
        while !self.peek_token_is(&Token::Rbrace) {
            self.next();
            let key = self.parse_expression(Precedence::Lowest)?;
            if !self.expect_peek(&Token::Colon) {
                return None;
            }
            self.next();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));
            if !self.peek_token_is(&Token::Rbrace) && !self.expect_peek(&Token::Comma) {
                return None;
            }
        }
        if !self.expect_peek(&Token::Rbrace) {
            return None;
        }
        Some(Expression::Hash {
            token: Token::Lbrace,
            pairs,
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.current_token.clone();
        self.next();
        let index = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&Token::Rbracket) {
            return None;
        }
        Some(Expression::Index {
            token,
            left: Box::new(left),
            index: Box::new(index),
        })
    }

    fn parse_reassign_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.current_token.clone();
        self.next();
        match &left {
            Expression::Identifier { .. } | Expression::Index { .. } => {
                let right = self.parse_expression(Precedence::Lowest)?;
                Some(Expression::Infix {
                    token,
                    operator: "=",
                    left: Box::from(left),
                    right: Box::from(right),
                })
            }
            _ => {
                self.push_parse_error("left value of = is not identifier");
                None
            }
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // prefix expressions
        let mut left_expression = match &self.current_token {
            Token::Function => self.parse_function_literal(),
            Token::Lbracket => self.parse_array_literal(),
            Token::Lbrace => self.parse_hash_literal(),
            Token::If => self.parse_if_expression(),
            Token::While => self.parse_while_expression(),
            Token::For => self.parse_for_expression(),
            Token::Lparen => self.parse_grouped_expression(),
            Token::Identifier(_) => self.parse_identifier(),
            Token::Integer(_) => self.parse_integer_literal(),
            Token::String(_) => self.parse_string_literal(),
            Token::True | Token::False => self.parse_boolean(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            _ => {
                self.push_parse_error(&format!(
                    "cannot parse expression, unexpected {}",
                    self.current_token.as_string()
                ));
                return None;
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedence() {
            // infix expressions
            left_expression = match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Percentage
                | Token::Asterisk
                | Token::Eq
                | Token::Neq
                | Token::And
                | Token::Or
                | Token::BitAnd
                | Token::BitOr
                | Token::BitXor
                | Token::BitShiftLeft
                | Token::BitShiftRight
                | Token::Lt
                | Token::Lte
                | Token::Gt
                | Token::Gte => {
                    self.next();
                    self.parse_infix_expression(left_expression?)
                }
                Token::Lparen => {
                    self.next();
                    self.parse_call_expression(left_expression?)
                }
                Token::Lbracket => {
                    self.next();
                    self.parse_index_expression(left_expression?)
                }
                Token::Assign => {
                    self.next();
                    self.parse_reassign_expression(left_expression?)
                }
                _ => {
                    return left_expression;
                }
            }
        }
        left_expression
    }

    fn parse_function_type_arguments(&mut self) -> Vec<Type> {
        let mut args = Vec::new();
        if self.peek_token_is(&Token::Rparen) {
            self.next();
            return args;
        }

        self.next();
        self.parse_argument_type().map(|arg| args.push(arg));

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            self.parse_argument_type().map(|arg| args.push(arg));
        }

        if !self.expect_peek(&Token::Rparen) {
            self.push_parse_error(&format!("Expected function declaration to finish with ')'"));
        }
        args
    }

    fn parse_argument_type(&mut self) -> Option<Type> {
        return match self.current_token.clone() {
            Token::Identifier(type_str) => match type_str {
                "Null" => Some(Type::Null),
                "String" => Some(Type::String),
                "Integer" => Some(Type::Integer),
                "Boolean" => Some(Type::Boolean),
                "Array" => Some(Type::Array),
                "HashMap" => Some(Type::Hash),
                _ => {
                    self.push_parse_error(&format!("Type {} does not exist", type_str));
                    None
                }
            },
            Token::Function => {
                if !self.expect_peek(&Token::Lparen) {
                    self.push_parse_error(&format!("Function type must follow format fn(a: a_type, b: b_type, ...): return_type"));
                    return None;
                }
                let args = self.parse_function_type_arguments();

                let mut return_type = Type::Null;
                if self.peek_token_is(&Token::Colon) {
                    self.next();
                    self.next();
                    return_type = self.parse_argument_type()?; // error has already been registered
                }
                return Some(Type::Function(args, Box::from(return_type)));
            }
            _ => {
                self.push_parse_error(&format!(
                    "Expected type identifier but got {}",
                    &self.current_token.as_string()
                ));
                None
            }
        };
    }

    fn parse_single_argument(&mut self) -> Option<(Identifier, Type)> {
        // TODO: remove clone
        let value_token = self.current_token.clone();
        if let Token::Identifier(value) = value_token {
            self.expect_peek(&Token::Colon);
            self.next();

            let typ = self.parse_argument_type()?;
            return Some((
                Identifier {
                    token: value_token.clone(),
                    value,
                },
                typ,
            ));
        };
        return None;
    }

    fn parse_function_arguments(&mut self) -> Option<Vec<(Identifier, Type)>> {
        let mut args = Vec::new();

        // no arguments provided
        if self.peek_token_is(&Token::Rparen) {
            self.next();
            return Some(args);
        }

        self.next();
        self.parse_single_argument().map(|arg| args.push(arg));

        while self.peek_token_is(&Token::Comma) {
            self.next();
            self.next();
            self.parse_single_argument().map(|arg| args.push(arg));
        }

        // panic!("{:?} {:?}", self.current_token, self.peek_token);
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }

        // panic!("{:?} {:?} {:?}", self.current_token, self.peek_token, args);
        Some(args)
    }

    fn parse_function_literal(&mut self) -> Option<Expression> {
        // function definition:
        // fn(arg1: type1, arg2: type2, ...): return_type { ... }
        let function_token = self.current_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }

        let arguments = self.parse_function_arguments();
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }
        let body = self.parse_block_statement();

        Some(Expression::Function {
            token: function_token,
            arguments: arguments?,
            body,
        })
    }

    fn parse_if_branch(&mut self) -> Option<(Expression, BlockStatement)> {
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }
        self.next();
        let condition = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(&Token::Rparen) {
            return None;
        }
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }
        let consequence = self.parse_block_statement();
        Some((condition, consequence))
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        let mut conditions = vec![self.parse_if_branch()?];

        let mut alternative: Option<BlockStatement> = None;
        while self.peek_token_is(&Token::Else) {
            self.next();
            if self.peek_token_is(&Token::If) {
                self.next();
                conditions.push(self.parse_if_branch()?);
            } else {
                if !self.expect_peek(&Token::Lbrace) {
                    return None;
                }
                alternative = Some(self.parse_block_statement());
                break;
            }
        }

        Some(Expression::If {
            token: current_token,
            conditions,
            alternative,
        })
    }

    fn parse_while_expression(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }
        self.next();
        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }
        Some(Expression::While {
            token: current_token,
            condition: Box::new(condition),
            statement: self.parse_block_statement(),
        })
    }

    fn parse_for_expression(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        if !self.expect_peek(&Token::Lparen) {
            return None;
        }
        self.next();

        let declaration = self.parse_statement()?;
        self.next();

        let condition = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&Token::Semicolon) {
            return None;
        }
        self.next();

        let operation = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }
        if !self.expect_peek(&Token::Lbrace) {
            return None;
        }

        Some(Expression::For {
            token: current_token,
            declaration: Box::from(declaration),
            condition: Box::from(condition),
            operation: Box::from(operation),
            statement: self.parse_block_statement(),
        })
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next();

        let expression = self.parse_expression(Precedence::Lowest);
        if !self.expect_peek(&Token::Rparen) {
            return None;
        }
        expression
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

        BlockStatement {
            token: current_token,
            statements,
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        let value = match &self.current_token {
            Token::Identifier(value) => value,
            _ => {
                self.push_parse_error(&format!(
                    "expected identifier, but got {}",
                    self.current_token.as_string()
                ));
                return None;
            }
        };
        Some(Expression::Identifier {
            token: self.current_token.clone(),
            value,
        })
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        let value = match self.current_token {
            Token::Integer(value) => value,
            _ => {
                self.push_parse_error(&format!(
                    "expected integer, but got {}",
                    self.current_token.as_string()
                ));
                return None;
            }
        };
        Some(Expression::Integer {
            token: self.current_token.clone(),
            value,
        })
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        let value = match &self.current_token {
            Token::String(value) => value,
            _ => {
                self.push_parse_error(&format!(
                    "expected string, but got {}",
                    self.current_token.as_string()
                ));
                return None;
            }
        };
        Some(Expression::String {
            token: self.current_token.clone(),
            value,
        })
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean {
            token: self.current_token.clone(),
            value: self.current_token_is(Token::True),
        })
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let current_token = self.current_token.clone();
        self.next();

        Some(Expression::Prefix {
            token: current_token.clone(),
            operator: current_token.as_string().to_string(),
            right: Box::new(self.parse_expression(Precedence::Prefix)?),
        })
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let current_token = self.current_token.clone();
        let precedence = self.current_precedence();
        self.next();

        Some(Expression::Infix {
            token: current_token.clone(),
            operator: Box::leak(current_token.as_string().to_string().into_boxed_str()),
            left: Box::new(left),
            right: Box::new(self.parse_expression(precedence).unwrap()),
        })
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        Some(Expression::Call {
            token: self.current_token.clone(),
            function: Box::new(left),
            arguments: self.parse_expression_list(Token::Rparen)?,
        })
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

    fn identifier(value: &'static str) -> Expression {
        Expression::Identifier {
            token: Token::Identifier(value),
            value,
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

    fn string(value: &'static str) -> Expression {
        Expression::String {
            token: Token::String(value),
            value,
        }
    }

    fn index(value: Expression, index: Expression) -> Expression {
        Expression::Index {
            token: Token::Lbracket,
            left: Box::from(value),
            index: Box::from(index),
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
            let y = true;
            let foobar = y;"
            .as_bytes();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![
                Statement::Let {
                    token: Token::Let,
                    name: Identifier {
                        token: Token::Identifier("x"),
                        value: "x"
                    },
                    value: Some(integer(5)),
                },
                Statement::Let {
                    token: Token::Let,
                    name: Identifier {
                        token: Token::Identifier("y"),
                        value: "y"
                    },
                    value: Some(boolean(true)),
                },
                Statement::Let {
                    token: Token::Let,
                    name: Identifier {
                        token: Token::Identifier("foobar"),
                        value: "foobar"
                    },
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
            return xy;"
            .as_bytes();

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
                Statement::Return {
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
            Program(vec![Statement::Expression {
                token: Token::Identifier(expected_value),
                expression: Some(identifier(expected_value)),
            },]),
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
            Program(vec![Statement::Expression {
                token: Token::Integer(5),
                expression: Some(integer(5)),
            },]),
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
                Program(vec![Statement::Expression {
                    token: Token::Bang,
                    expression: Some(Expression::Prefix {
                        token: Token::Bang,
                        operator: "!".to_owned(),
                        right: Box::new(integer(5)),
                    }),
                }]),
            ),
            (
                "-15;",
                Program(vec![Statement::Expression {
                    token: Token::Minus,
                    expression: Some(Expression::Prefix {
                        token: Token::Minus,
                        operator: "-".to_owned(),
                        right: Box::new(integer(15)),
                    }),
                }]),
            ),
        ];

        for (input, expected) in tests {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);

            assert_eq!(expected, program);
        }
    }

    fn token_from_expression(expr: &Expression) -> Token {
        match expr {
            Expression::Identifier { value, .. } => Token::Identifier(value),
            Expression::Integer { value, .. } => Token::Integer(value.to_owned()),
            Expression::Index { left, .. } => token_from_expression(left),
            _ => Token::Illegal('1'),
        }
    }

    fn infix_template(
        left: Expression,
        op: &'static str,
        op_token: Token,
        right: Expression,
    ) -> Statement {
        let token = token_from_expression(&left);
        Statement::Expression {
            token,
            expression: Some(Expression::Infix {
                token: op_token,
                operator: op,
                left: Box::new(left),
                right: Box::new(right),
            }),
        }
    }

    fn int_infix_template(left: i64, op: &'static str, op_token: Token, right: i64) -> Statement {
        Statement::Expression {
            token: Token::Integer(left),
            expression: Some(Expression::Infix {
                token: op_token,
                operator: op,
                left: Box::new(integer(left)),
                right: Box::new(integer(right)),
            }),
        }
    }

    #[test]
    fn test_infix_expression() {
        let tests = vec![
            (
                "5 + 5;",
                Program(vec![int_infix_template(5, "+", Token::Plus, 5)]),
            ),
            (
                "5 - 5;",
                Program(vec![int_infix_template(5, "-", Token::Minus, 5)]),
            ),
            (
                "5 * 5;",
                Program(vec![int_infix_template(5, "*", Token::Asterisk, 5)]),
            ),
            (
                "5 / 5;",
                Program(vec![int_infix_template(5, "/", Token::Slash, 5)]),
            ),
            (
                "5 % 5;",
                Program(vec![int_infix_template(5, "%", Token::Percentage, 5)]),
            ),
            (
                "5 > 5;",
                Program(vec![int_infix_template(5, ">", Token::Gt, 5)]),
            ),
            (
                "5 >= 5;",
                Program(vec![int_infix_template(5, ">=", Token::Gte, 5)]),
            ),
            (
                "5 < 5;",
                Program(vec![int_infix_template(5, "<", Token::Lt, 5)]),
            ),
            (
                "5 <= 5;",
                Program(vec![int_infix_template(5, "<=", Token::Lte, 5)]),
            ),
            (
                "5 == 5;",
                Program(vec![int_infix_template(5, "==", Token::Eq, 5)]),
            ),
            (
                "5 != 5;",
                Program(vec![int_infix_template(5, "!=", Token::Neq, 5)]),
            ),
            (
                "5 & 5;",
                Program(vec![int_infix_template(5, "&", Token::BitAnd, 5)]),
            ),
            (
                "5 && 5;",
                Program(vec![int_infix_template(5, "&&", Token::And, 5)]),
            ),
            (
                "5 | 5;",
                Program(vec![int_infix_template(5, "|", Token::BitOr, 5)]),
            ),
            (
                "5 || 5;",
                Program(vec![int_infix_template(5, "||", Token::Or, 5)]),
            ),
            (
                "5 ^ 5;",
                Program(vec![int_infix_template(5, "^", Token::BitXor, 5)]),
            ),
            (
                "5 << 5;",
                Program(vec![int_infix_template(5, "<<", Token::BitShiftLeft, 5)]),
            ),
            (
                "5 >> 5;",
                Program(vec![int_infix_template(5, ">>", Token::BitShiftRight, 5)]),
            ),
            (
                "value = 5;",
                Program(vec![infix_template(
                    identifier("value"),
                    "=",
                    Token::Assign,
                    integer(5),
                )]),
            ),
            (
                "value[1] = 5;",
                Program(vec![infix_template(
                    index(identifier("value"), integer(1)),
                    "=",
                    Token::Assign,
                    integer(5),
                )]),
            ),
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
                    conditions: vec![(
                        Expression::Infix {
                            token: Token::Lt,
                            operator: "<",
                            left: Box::new(identifier("x")),
                            right: Box::new(identifier("y")),
                        },
                        BlockStatement {
                            token: Token::Lbrace,
                            statements: vec![Statement::Expression {
                                token: Token::Identifier("x"),
                                expression: Some(identifier("x")),
                            },],
                        }
                    )],
                    alternative: None,
                })
            }]),
            program
        );
    }

    #[test]
    fn test_while_statement() {
        let input = "while (x < y) { x }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::While,
                expression: Some(Expression::While {
                    token: Token::While,
                    condition: Box::new(Expression::Infix {
                        token: Token::Lt,
                        operator: "<",
                        left: Box::new(identifier("x")),
                        right: Box::new(identifier("y")),
                    }),
                    statement: BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![Statement::Expression {
                            token: Token::Identifier("x"),
                            expression: Some(identifier("x")),
                        },],
                    },
                })
            }]),
            program
        );
    }

    #[test]
    fn test_for_statement() {
        let input = "for (let i = 0; i < x; i = i + 1) { x }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::For,
                expression: Some(Expression::For {
                    token: Token::For,
                    declaration: Box::new(Statement::Let {
                        token: Token::Let,
                        name: Identifier {
                            token: Token::Identifier("i"),
                            value: "i",
                        },
                        value: Some(integer(0)),
                    }),
                    condition: Box::new(Expression::Infix {
                        token: Token::Lt,
                        operator: "<",
                        left: Box::new(identifier("i")),
                        right: Box::new(identifier("x")),
                    }),
                    operation: Box::new(Expression::Infix {
                        token: Token::Assign,
                        operator: "=",
                        left: Box::from(Expression::Identifier {
                            token: Token::Identifier("i"),
                            value: "i",
                        }),
                        right: Box::from(Expression::Infix {
                            token: Token::Plus,
                            operator: "+",
                            left: Box::new(identifier("i")),
                            right: Box::new(integer(1)),
                        }),
                    }),
                    statement: BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![Statement::Expression {
                            token: Token::Identifier("x"),
                            expression: Some(identifier("x")),
                        },],
                    },
                })
            }]),
            program
        );
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (x < y) { x } else if (x > y) { y } else { z }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::If,
                expression: Some(Expression::If {
                    token: Token::If,
                    conditions: vec![
                        (
                            Expression::Infix {
                                token: Token::Lt,
                                operator: "<",
                                left: Box::new(identifier("x")),
                                right: Box::new(identifier("y")),
                            },
                            BlockStatement {
                                token: Token::Lbrace,
                                statements: vec![Statement::Expression {
                                    token: Token::Identifier("x"),
                                    expression: Some(identifier("x")),
                                },],
                            },
                        ),
                        (
                            Expression::Infix {
                                token: Token::Gt,
                                operator: ">",
                                left: Box::new(identifier("x")),
                                right: Box::new(identifier("y")),
                            },
                            BlockStatement {
                                token: Token::Lbrace,
                                statements: vec![Statement::Expression {
                                    token: Token::Identifier("y"),
                                    expression: Some(identifier("y")),
                                },],
                            },
                        ),
                    ],
                    alternative: Some(BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![Statement::Expression {
                            token: Token::Identifier("z"),
                            expression: Some(identifier("z")),
                        },],
                    }),
                })
            }]),
            program
        );
    }

    #[test]
    fn test_function_literal() {
        let input = "fn(x: Integer, y: Integer) { x + y; }".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Function,
                expression: Some(Expression::Function {
                    token: Token::Function,
                    arguments: vec![
                        (
                            Identifier {
                                token: Token::Identifier("x"),
                                value: "x"
                            },
                            Type::Integer,
                        ),
                        (
                            Identifier {
                                token: Token::Identifier("y"),
                                value: "y"
                            },
                            Type::Integer,
                        )
                    ],
                    body: BlockStatement {
                        token: Token::Lbrace,
                        statements: vec![Statement::Expression {
                            token: Token::Identifier("x"),
                            expression: Some(Expression::Infix {
                                token: Token::Plus,
                                operator: "+",
                                left: Box::new(identifier("x")),
                                right: Box::new(identifier("y")),
                            })
                        }],
                    }
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_function_literal_argument_types() {
        let test_cases = vec![
            (
                "fn(a: Integer, b: String, c: Boolean, d: Null, e: Array, f: HashMap) {}",
                vec![
                    (Identifier::from("a"), Type::Integer),
                    (Identifier::from("b"), Type::String),
                    (Identifier::from("c"), Type::Boolean),
                    (Identifier::from("d"), Type::Null),
                    (Identifier::from("e"), Type::Array),
                    (Identifier::from("f"), Type::Hash),
                ],
            ),
            (
                "fn(a: fn(Integer)) {}",
                vec![(
                    Identifier::from("a"),
                    Type::Function(vec![Type::Integer], Box::new(Type::Null)),
                )],
            ),
            (
                "fn(a: fn(Integer, String): Boolean) {}",
                vec![(
                    Identifier::from("a"),
                    Type::Function(vec![Type::Integer, Type::String], Box::new(Type::Boolean)),
                )],
            ),
            (
                "fn(a: fn()) {}",
                vec![(
                    Identifier::from("a"),
                    Type::Function(vec![], Box::new(Type::Null)),
                )],
            ),
        ];

        for (input, expected_arguments) in test_cases {
            let lexer = Lexer::new(input.as_bytes());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parse_errors(parser);

            assert_eq!(
                Program(vec![Statement::Expression {
                    token: Token::Function,
                    expression: Some(Expression::Function {
                        token: Token::Function,
                        arguments: expected_arguments,
                        body: BlockStatement {
                            token: Token::Lbrace,
                            statements: vec![],
                        }
                    }),
                }],),
                program,
                "input: {}",
                input,
            );
        }
    }

    #[test]
    fn test_function_call() {
        let input = "add(1, 2 * 3, 4 + 5)".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Identifier("add"),
                expression: Some(Expression::Call {
                    token: Token::Lparen,
                    function: Box::new(identifier("add")),
                    arguments: vec![
                        Expression::Integer {
                            token: Token::Integer(1),
                            value: 1
                        },
                        Expression::Infix {
                            token: Token::Asterisk,
                            operator: "*",
                            left: Box::new(integer(2)),
                            right: Box::new(integer(3)),
                        },
                        Expression::Infix {
                            token: Token::Plus,
                            operator: "+",
                            left: Box::new(integer(4)),
                            right: Box::new(integer(5)),
                        },
                    ],
                }),
            }]),
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
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 >= 5 == false", "((3 >= 5) == false)"),
            ("3 > 5 && false == true", "((3 > 5) && (false == true))"),
            ("a || b && c", "(a || (b && c))"),
            ("a != b && c", "((a != b) && c)"),
            ("a | b && c || d & e", "(((a | b) && c) || (d & e))"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
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
        let program = Program(vec![Statement::Let {
            token: Token::Let,
            name: Identifier {
                token: Token::Identifier("my_var"),
                value: "my_var",
            },
            value: Some(identifier("another_var")),
        }]);
        assert_eq!(format!("{}", program), "let my_var = another_var;")
    }

    #[test]
    fn test_string_literal() {
        let input = "\"hello world\"".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::String("hello world"),
                expression: Some(Expression::String {
                    token: Token::String("hello world"),
                    value: "hello world",
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Lbracket,
                expression: Some(Expression::Array {
                    token: Token::Lbracket,
                    elements: vec![
                        Expression::Integer {
                            token: Token::Integer(1),
                            value: 1,
                        },
                        Expression::Infix {
                            token: Token::Asterisk,
                            operator: "*",
                            left: Box::new(integer(2)),
                            right: Box::new(integer(2)),
                        },
                        Expression::Infix {
                            token: Token::Plus,
                            operator: "+",
                            left: Box::new(integer(3)),
                            right: Box::new(integer(3)),
                        },
                    ],
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_empty_hash_literal() {
        let input = "{}".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Lbrace,
                expression: Some(Expression::Hash {
                    token: Token::Lbrace,
                    pairs: vec![],
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_hash_literal_string_keys() {
        let input = "{\"one\": 1, \"two\": 2}".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Lbrace,
                expression: Some(Expression::Hash {
                    token: Token::Lbrace,
                    pairs: vec![(string("one"), integer(1)), (string("two"), integer(2)),],
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_hash_literal_with_expressions() {
        let input = "{\"one\": 0 + 1, \"two\": 10 - 8, true: 15/5}".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Lbrace,
                expression: Some(Expression::Hash {
                    token: Token::Lbrace,
                    pairs: vec![
                        (
                            string("one"),
                            Expression::Infix {
                                token: Token::Plus,
                                operator: "+",
                                left: Box::new(integer(0)),
                                right: Box::new(integer(1)),
                            },
                        ),
                        (
                            string("two"),
                            Expression::Infix {
                                token: Token::Minus,
                                operator: "-",
                                left: Box::new(integer(10)),
                                right: Box::new(integer(8)),
                            },
                        ),
                        (
                            boolean(true),
                            Expression::Infix {
                                token: Token::Slash,
                                operator: "/",
                                left: Box::new(integer(15)),
                                right: Box::new(integer(5)),
                            },
                        ),
                    ],
                }),
            }]),
            program
        );
    }

    #[test]
    fn test_index_exprtession() {
        let input = "myArray[1 + 1]".as_bytes();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parse_errors(parser);

        assert_eq!(
            Program(vec![Statement::Expression {
                token: Token::Identifier("myArray"),
                expression: Some(Expression::Index {
                    token: Token::Lbracket,
                    left: Box::new(Expression::Identifier {
                        token: Token::Identifier("myArray"),
                        value: "myArray"
                    }),
                    index: Box::new(Expression::Infix {
                        token: Token::Plus,
                        operator: "+",
                        left: Box::new(integer(1)),
                        right: Box::new(integer(1)),
                    }),
                }),
            }]),
            program
        );
    }
}
