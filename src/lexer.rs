use std::str;
use crate::token::*;

// TODO keep track of file, line and column for better error reporting
pub struct Lexer<'a> {
    input: &'a [u8],
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &[u8]) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0.into(),
        };
        l.read_char();
        return l;
    }

    // TODO add UTF8 support
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0.into();
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        if self.read_position > self.input.len() {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while is_letter(self.ch) {
            self.read_char();
        }
        return String::from_utf8(self.input[start_pos..self.position].to_vec())
            .unwrap();
    }

    fn read_integer(&mut self) -> i64 {
        let start_pos = self.position;
        while is_digit(self.ch) {
            self.read_char()
        }
        return str::from_utf8(self.input[start_pos..self.position].as_ref())
            .unwrap()
            .parse()
            .expect("should be valid integer")
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        let token = match self.ch as char {
            // EOF
            '\0' => None,
            // TODO abstract peeking logic
            '=' => {
                match self.peek_char() as char {
                    '=' => {
                        self.read_char();
                        Some(Token::Eq)
                    }
                    _ => Some(Token::Assign)
                }
            },
            '!' => {
                match self.peek_char() as char {
                    '=' => {
                        self.read_char();
                        Some(Token::Neq)
                    }
                    _ => Some(Token::Bang)
                }
            },
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Asterisk),
            '/' => Some(Token::Slash),
            '>' => Some(Token::Gt),
            '<' => Some(Token::Lt),
            ';' => Some(Token::Semicolon),
            '(' => Some(Token::Lparen),
            ')' => Some(Token::Rparen),
            ',' => Some(Token::Comma),
            '{' => Some(Token::Lbrace),
            '}' => Some(Token::Rbrace),
            _ => {
                if is_letter(self.ch) {
                    return Token::from_literal(self.read_identifier());
                } else if is_digit(self.ch) {
                    return Some(Token::Integer(self.read_integer()));
                } else {
                    Some(Token::Illegal(self.ch as char))
                }
            },
        };
        self.read_char();
        return token;
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
return true;
} else {
return false;
}

10 == 10;
10 != 9;".as_bytes();

        let tests: Vec<Option<Token>> = vec![
            Some(Token::Let),
            Some(Token::Ident("five".to_string())),
            Some(Token::Assign),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("ten".to_string())),
            Some(Token::Assign),
            Some(Token::Integer(10)),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("add".to_string())),
            Some(Token::Assign),
            Some(Token::Function),
            Some(Token::Lparen),
            Some(Token::Ident("x".to_string())),
            Some(Token::Comma),
            Some(Token::Ident("y".to_string())),
            Some(Token::Rparen),
            Some(Token::Lbrace),
            Some(Token::Ident("x".to_string())),
            Some(Token::Plus),
            Some(Token::Ident("y".to_string())),
            Some(Token::Semicolon),
            Some(Token::Rbrace),
            Some(Token::Semicolon),
            Some(Token::Let),
            Some(Token::Ident("result".to_string())),
            Some(Token::Assign),
            Some(Token::Ident("add".to_string())),
            Some(Token::Lparen),
            Some(Token::Ident("five".to_string())),
            Some(Token::Comma),
            Some(Token::Ident("ten".to_string())),
            Some(Token::Rparen),
            Some(Token::Semicolon),
            Some(Token::Bang),
            Some(Token::Minus),
            Some(Token::Slash),
            Some(Token::Asterisk),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),
            Some(Token::Integer(5)),
            Some(Token::Lt),
            Some(Token::Integer(10)),
            Some(Token::Gt),
            Some(Token::Integer(5)),
            Some(Token::Semicolon),
            Some(Token::If),
            Some(Token::Lparen),
            Some(Token::Integer(5)),
            Some(Token::Lt),
            Some(Token::Integer(10)),
            Some(Token::Rparen),
            Some(Token::Lbrace),
            Some(Token::Return),
            Some(Token::True),
            Some(Token::Semicolon),
            Some(Token::Rbrace),
            Some(Token::Else),
            Some(Token::Lbrace),
            Some(Token::Return),
            Some(Token::False),
            Some(Token::Semicolon),
            Some(Token::Rbrace),
            Some(Token::Integer(10)),
            Some(Token::Eq),
            Some(Token::Integer(10)),
            Some(Token::Semicolon),
            Some(Token::Integer(10)),
            Some(Token::Neq),
            Some(Token::Integer(9)),
            Some(Token::Semicolon),
            None,
        ];

        let mut l = Lexer::new(input);
        for (i, tt) in tests.iter().enumerate() {
            let token = l.next_token();
            assert_eq!(token, *tt, "tests[{}] - wrong token type. expected={:?}, got={:?}", i, tt, token);
        }
    }
}

