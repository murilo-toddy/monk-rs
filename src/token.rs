#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Illegal(char),
    Eof,
    
    // Identifiers
    // TODO add support for floats
    Identifier(String),
    Integer(i64),
    String(String),

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
    // TODO add loops
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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal(v) => write!(f, "{}", v),
            Token::Eof => write!(f, "\0"),
            Token::Identifier(v) => write!(f, "{}", v),
            Token::Integer(v) => write!(f, "{}", v),
            Token::String(v) => write!(f, "{}", v),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Gt => write!(f, ">"),
            Token::Lt => write!(f, "<"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Eq => write!(f, "=="),
            Token::Neq => write!(f, "!="),
        }
    }
}

