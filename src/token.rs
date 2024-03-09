// TODO add support for floats
#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(u8),
    Eof,

    // Identifiers
    Ident(String),
    Integer(i64),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Gt,
    Lt,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,

    Eq,
    Neq,
}

impl Token {
    pub fn from_literal(literal: String) -> Token {
        match literal.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(literal),
        }
    }
}

pub fn is_letter(ch: u8) -> bool {
    return ch.is_ascii_alphabetic() || (ch == ('_' as u8));
}

pub fn is_digit(ch: u8) -> bool {
    return ch.is_ascii_digit();
}
