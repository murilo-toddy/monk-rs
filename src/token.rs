// TODO add support for floats
// TODO add for loops
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Illegal(char),
    Eof,

    // Identifiers
    Identifier(String),
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
            _ => Token::Identifier(literal),
        }
    }

    // TODO this feels wrong
    pub fn to_string(&self) -> String {
        match self {
            Token::Plus => "+",
            Token::Bang => "!",
            Token::Minus => "-",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Eq => "==",
            Token::Neq => "!=",
            Token::Lt => "<",
            Token::Gt => ">",
            _ => "",
        }.to_owned()
    }
}

