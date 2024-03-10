use core::fmt;

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

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current_token: None,
            peek_token: None,
            errors: vec![],
        };
        // set current and peek tokens
        parser.next_token();
        parser.next_token();
        parser
    }

    fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    fn peek_is(&mut self, token: Token) -> bool {
        if self.peek_token.as_ref().is_some_and(|peek_token| *peek_token == token) {
            self.next_token();
            return true;
        }
        return false;
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
            if self.token_types_match(&peek_token, token) {
                self.next_token();
                return true;
            }
        }
        self.peek_error(token);
        return false;
    }

    fn current_token_is(&mut self, token: Token) -> bool {
        if let Some(current_token) = self.current_token.take() {
            if self.token_types_match(&current_token, &token) {
                return true;
            }
        }
        return false;
    }

    fn parse_let_statement(&mut self) -> Option<Box<LetStatement>> {
        let let_token = self.current_token.clone();

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

        return Some(Box::from(LetStatement {
            name: identifier,
            token: let_token.unwrap(),
        }));
    }

    fn parse_statement(&mut self) -> Option<Box<dyn Statement>> {
        match self.current_token {
            Some(Token::Let) => self.parse_let_statement().map(|s| s as Box<dyn Statement>),
            _ => None,
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
        return Program { statements }
    }
}

#[cfg(test)]
mod parser_tests {
    use super::*;

    fn test_let_statement(statement: &Box<dyn Statement>, name: &String) {
        if *statement.get_token() != Token::Let {
            panic!("expected statement with token <Token::Let> but got <{:?}>", statement.get_token());
        }
        match statement.as_any().downcast_ref::<LetStatement>() {
            None => panic!("expected LetStatement but got {:?}", statement),
            Some(let_statement) => {
                if let_statement.name.value != *name {
                    panic!("expected identifier <{}> for let statement but got <{}>", name, let_statement.name.value)
                }
            },
        }
    }

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
        let input = "let x 5;
let y = 10;
let foobar 838383;".as_bytes();
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        check_parse_errors(parser);

        if program.statements.len() != 3 {
            panic!("program does not contain 3 elements")
        }

        let expected_identifiers = vec![
            "x".to_string(),
            "y".to_string(),
            "foobar".to_string(),
        ];

        for (i, expected_identifier) in expected_identifiers.iter().enumerate() {
            match program.statements.get(i) {
                Some(statement) => test_let_statement(statement, expected_identifier),
                None => panic!("index out of bounds fo statements"),
            }
        }
    }

}

