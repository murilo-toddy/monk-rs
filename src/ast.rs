use crate::token::Token;

#[derive(Eq, PartialEq, Debug)]
pub struct Identifier{ 
    pub token: Token,
    pub value: String,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Option<Expression>,
    },
    Return {
        token: Token,
        value: Option<Expression>,
    },
    Expression {
        token: Token,
        expression: Option<Expression>,
    },
}

#[derive(Eq, PartialEq, Debug)]
pub enum Expression {
    Identifier {
        token: Token,
        value: String,
    },
    Integer {
        token: Token,
        value: i64,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    Prefix {
        token: Token,
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: Token,
        operator: String,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    If {
        token: Token,
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        token: Token,
        arguments: Vec<Identifier>,
        body: BlockStatement,
    },
}

#[derive(Debug, Eq, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

pub type Program = Vec<Statement>;

#[derive(Eq, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,      // ==
    LessGreater = 3, // > or <
    Sum = 4,         // +
    Product = 5,     // *
    Prefix = 6,      // -x or !x
    Call = 7,        // func(x)
}

