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
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    // TODO add UTF8 support
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position]
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> u8 {
        match self.read_position > self.input.len() {
            true => 0,
            false => self.input[self.read_position],
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        String::from_utf8(self.input[start_pos..self.position].to_vec()).unwrap()
    }

    fn read_integer(&mut self) -> i64 {
        let start_pos = self.position;
        while Self::is_digit(self.ch) {
            self.read_char()
        }
        return str::from_utf8(self.input[start_pos..self.position].as_ref())
            .unwrap()
            .parse()
            .expect("should be valid integer")
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop { 
            self.read_char();
            if self.ch as char == '"' || self.ch == 0 {
                break
            }
        }
        match String::from_utf8(self.input[position .. self.position].to_vec()) {
            Ok(v) => v,
            Err(e) => panic!("invalid UTF8 string {}", e)
        }
    }
    
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn compare_peek(&mut self, ch: char, eq_token: Token, else_token: Token) -> Token {
        if (self.peek_char() as char) == ch {
            self.read_char();
            return eq_token
        }
        else_token
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch as char {
            '\0' => Token::Eof,
            '=' => self.compare_peek('=', Token::Eq, Token::Assign),
            '!' => self.compare_peek('=', Token::Neq, Token::Bang),
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '>' => Token::Gt,
            '<' => Token::Lt,
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '[' => Token::Lbracket,
            ']' => Token::Rbracket,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '"' => Token::String(self.read_string()),
            _ => {
                if Self::is_letter(self.ch) {
                    return Token::from_literal(self.read_identifier());
                } else if Self::is_digit(self.ch) {
                    return Token::Integer(self.read_integer());
                } else {
                    Token::Illegal(self.ch as char)
                }
            },
        };
        self.read_char();
        token
    }

    fn is_letter(ch: u8) -> bool {
        ch.is_ascii_alphabetic() || ch == b'_'
    }

    fn is_digit(ch: u8) -> bool {
        ch.is_ascii_digit()
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
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
".as_bytes();

        let tests = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::Lparen,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::Rparen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::Lt,
            Token::Integer(10),
            Token::Gt,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Integer(5),
            Token::Lt,
            Token::Integer(10),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Integer(10),
            Token::Eq,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::Neq,
            Token::Integer(9),
            Token::Semicolon,
            Token::String("foobar".to_string()),
            Token::String("foo bar".to_string()),
            Token::Lbracket,
            Token::Integer(1),
            Token::Comma,
            Token::Integer(2),
            Token::Rbracket,
            Token::Semicolon,
            Token::Eof,
        ];

        let mut l = Lexer::new(input);
        for (i, tt) in tests.iter().enumerate() {
            let token = l.next_token();
            assert_eq!(token, *tt, "tests[{}] - wrong token type. expected={:?}, got={:?}", i, tt, token);
        }
    }
}

