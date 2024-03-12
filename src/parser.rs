use core::fmt;
use std::collections::HashMap;
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


type PrefixParseFn = fn(p: &mut Parser) -> Box<dyn Expression>;
type InfixParseFn = fn(p: &mut Parser, prefix: Box<dyn Expression>) -> Box<dyn Expression>;

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParseError>,

    prefix_parse_fns: HashMap<Discriminant<Token>, Box<PrefixParseFn>>,
    infix_parse_fns: HashMap<Discriminant<Token>, Box<InfixParseFn>>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],

            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        // set current and peek tokens
        parser.next_token();
        parser.next_token();

        // TODO holly shit this is so ugly
        parser.register_prefix(
            discriminant(&Token::Ident("".to_owned())),
            |p| p.parse_identifier(),
        );
        parser.register_prefix(
            discriminant(&Token::Integer(0)),
            |p| p.parse_integer_literal(),
        );
        parser.register_prefix(
            discriminant(&Token::Bang),
            |p| p.parse_prefix_expression(),
        );
        parser.register_prefix(
            discriminant(&Token::Minus),
            |p| p.parse_prefix_expression(),
        );

        parser.register_infix(
            discriminant(&Token::Plus),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Minus),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Slash),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Asterisk),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Eq),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Neq),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Lt),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser.register_infix(
            discriminant(&Token::Gt),
            |p, prefix| p.parse_infix_expression(prefix),
        );
        parser
    }

    fn parse_identifier(&mut self) -> Box<dyn Expression> {
        let value = match self.current_token.as_ref().unwrap() {
            Token::Ident(value) => value.clone(),
            _ => panic!("expected an identifier token"),
        };
        Box::new(Identifier {
            token: self.current_token.clone().unwrap(),
            value,
        })
    }

    fn parse_integer_literal(&mut self) -> Box<dyn Expression> {
        let value = match self.current_token.as_ref().unwrap() {
            Token::Integer(value) => value,
            _ => panic!("expected integer token"),
        };
        Box::new(IntegerLiteral {
            token: self.current_token.clone().unwrap(),
            value: *value,
        })
    }

    fn parse_prefix_expression(&mut self) -> Box<dyn Expression> {
        let current_token = self.current_token.clone().unwrap();
        self.next_token();
        
        Box::from(PrefixExpression {
            token: current_token.clone(),
            operator: current_token.to_string(),
            right: self.parse_expression(Precedence::Prefix).unwrap_or_else(||
                panic!("prefix expressions should have right")
            ),
        })
    }

    fn parse_infix_expression(&mut self, left: Box<dyn Expression>) -> Box<dyn Expression> {
        let current_token = self.current_token.clone().unwrap();
        let precedence = self.current_precedence();
        self.next_token();

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

    fn register_prefix(&mut self, token: Discriminant<Token>, fnc: PrefixParseFn) {
        self.prefix_parse_fns.insert(token, Box::from(fnc));
    }

    fn register_infix(&mut self, token: Discriminant<Token>, fnc: InfixParseFn) {
        self.infix_parse_fns.insert(token, Box::from(fnc));
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    fn peek_token_is(&mut self, token: Token) -> bool {
        self.peek_token.as_ref().is_some_and(|peek_token| *peek_token == token)
    }

    fn token_types_match(&self, t1: &Token, t2: &Token) -> bool {
        std::mem::discriminant(t1) == std::mem::discriminant(t2)
    }

    fn peek_error(&mut self, expected_token: &Token) {
        self.errors.push(ParseError { 
            message: format!("expected next token to be {:?} but got {:?}", expected_token, self.peek_token).to_owned() 
        });
    }

    fn expect_peek(&mut self, token: &Token) -> bool {
        if let Some(peek_token) = &self.peek_token {
            if self.token_types_match(peek_token, token) {
                self.next_token();
                return true;
            }
        }
        self.peek_error(token);
        false
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        if let Some(current_token) = self.current_token.take() {
            if self.token_types_match(&current_token, &token) {
                return true;
            }
        }
        false
    }

    fn parse_let_statement(&mut self) -> Option<Box<LetStatement>> {
        let let_token = self.current_token.clone().unwrap();

        // TODO remove this String::from
        if !self.expect_peek(&Token::Ident(String::from(""))) {
            return None;
        }

        let identifier = match self.current_token.take() {
            Some(token) => {
                match token.clone() {
                    Token::Ident(value) => Identifier { token, value },
                    _ => panic!("branch cannot happen")
                }
            },
            _ => panic!("branch cannot happen")
        };

        if !self.expect_peek(&Token::Assign) {
            return None;
        }
        
        // TODO skipping expressions until a semicolon is found
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Box::from(LetStatement {
            name: identifier,
            token: let_token,
            value: None,
        }))
    }

    fn parse_return_statement(&mut self) -> Option<Box<ReturnStatement>> {
        let return_token = self.current_token.clone().unwrap();
        self.next_token();

        // TODO skipping expressions until a semicolon is found
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }
        Some(Box::from(ReturnStatement {
            token: return_token,
            value: None,
        }))
    }

    fn precedence_from_token(&self, token: &Option<Token>) -> Precedence {
        match token {
            Some(Token::Eq) => Precedence::Equals,
            Some(Token::Neq) => Precedence::Equals,
            Some(Token::Lt) => Precedence::LessGreater,
            Some(Token::Gt) => Precedence::LessGreater,
            Some(Token::Plus) => Precedence::Sum,
            Some(Token::Minus) => Precedence::Sum,
            Some(Token::Asterisk) => Precedence::Product,
            Some(Token::Slash) => Precedence::Product,
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
        // TODO need to add parse error here if result is none
        let prefix_func = self.prefix_parse_fns.get(&discriminant(self.current_token.as_ref().unwrap()))?;
        // TODO if possible, remove this clone
        let mut left_expression = prefix_func.clone()(self);

        while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
            // TODO remove clone
            left_expression = if let Some(infix) = self.infix_parse_fns.clone().get(&discriminant(self.peek_token.as_ref().unwrap())) {
                self.next_token();
                infix(self, left_expression)
            } else {
                return Some(left_expression);
            };
        }
        Some(left_expression)
    }

    fn parse_expression_statement(&mut self) -> Option<Box<ExpressionStatement>> {
        let expression_token = self.current_token.clone().unwrap();
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }
        Some(Box::from(ExpressionStatement {
            token: expression_token,
            expression,
        }))
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement().map(|s| s as Box<dyn Statement>),
            Some(Token::Return) => self.parse_return_statement().map(|s| s as Box<dyn Statement>),
            _ => self.parse_expression_statement().map(|s| s as Box<dyn Statement>),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut statements = Vec::new();
        while self.current_token.is_some() {
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            }
            self.next_token();
        }
        Program { statements }
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

