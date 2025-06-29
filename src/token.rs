#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Token {
    Illegal(char),
    Eof,

    // Identifiers
    // TODO add support for floats
    Identifier(&'static str),
    Integer(i64),
    String(&'static str),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Percentage,

    // Logical operations
    Eq,
    Neq,

    Gt,
    Gte,
    Lt,
    Lte,

    And,
    Or,

    // Bitwise operations
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,

    // Separators
    Comma,
    Colon,
    Semicolon,

    Lparen,
    Rparen,
    Lbracket,
    Rbracket,
    Lbrace,
    Rbrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    While,
    For,
    Return,
    True,
    False,
}

impl Token {
    pub fn from_literal(literal: &'static str) -> Token {
        match literal {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(literal),
        }
    }

    pub fn as_string(&self) -> &'static str {
        match self {
            Token::Illegal(v) => Box::leak(v.to_string().into_boxed_str()),
            Token::Eof => "\0",
            Token::Identifier(v) => v,
            Token::Integer(v) => Box::leak(v.to_string().into_boxed_str()),
            Token::String(v) => v,
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",
            Token::Percentage => "%",
            Token::Eq => "==",
            Token::Neq => "!=",
            Token::Gt => ">",
            Token::Gte => ">=",
            Token::Lt => "<",
            Token::Lte => "<=",
            Token::And => "&&",
            Token::Or => "||",
            Token::BitAnd => "&",
            Token::BitOr => "|",
            Token::BitXor => "^",
            Token::BitShiftLeft => "<<",
            Token::BitShiftRight => ">>",
            Token::Comma => ",",
            Token::Colon => ":",
            Token::Semicolon => ";",
            Token::Lparen => "(",
            Token::Rparen => ")",
            Token::Lbracket => "[",
            Token::Rbracket => "]",
            Token::Lbrace => "{",
            Token::Rbrace => "}",
            Token::Function => "fn",
            Token::Let => "let",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::For => "for",
            Token::Return => "return",
            Token::True => "true",
            Token::False => "false",
        }
    }
}
