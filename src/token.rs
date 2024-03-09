// TODO add support for floats
#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(u8),

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
    pub fn from_literal(literal: String) -> Option<Token> {
        match literal.as_str() {
            "fn" => Some(Token::Function),
            "let" => Some(Token::Let),
            "if" => Some(Token::If),
            "else" => Some(Token::Else),
            "return" => Some(Token::Return),
            "true" => Some(Token::True),
            "false" => Some(Token::False),
            _ => Some(Token::Ident(literal)),
        }
    }
}

pub fn is_letter(ch: u8) -> bool {
    return ch.is_ascii_alphabetic() || (ch == ('_' as u8));
}

pub fn is_digit(ch: u8) -> bool {
    return ch.is_ascii_digit();
}

