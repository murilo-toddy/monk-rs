use core::fmt::Debug;
use std::any::Any;

use crate::token::Token;

pub trait Node {
    fn get_token(&self) -> &Token;
}

#[derive(Debug, PartialEq)]
pub enum StatementType {
    LetStatement,
    ReturnStatement,
}

pub trait Statement: Node {
    fn as_any(&self) -> &dyn Any;
    fn get_type(&self) -> StatementType;
    fn statement_node(&self);
}

impl Debug for dyn Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.get_type())
    }
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn get_token(&self) -> &Token {
        match self.statements.len() {
            0 => &Token::Eof,
            _ => &self.statements[0].get_token()
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Expression for Identifier {
    fn expression_node(&self) {
        todo!("not implemented");
    }
}

impl Node for Identifier {
    fn get_token(&self) -> &Token {
        todo!("not implemented");
    }
}

pub struct LetStatement {
    pub token: Token, // Token::Let
    pub name: Identifier,
}

impl Statement for LetStatement {
    fn as_any(&self) -> &dyn Any {
        self   
    }

    fn get_type(&self) -> StatementType {
        StatementType::LetStatement
    }

    fn statement_node(&self) {
        todo!("not implemented");
    }
}

impl Node for LetStatement {
    fn get_token(&self) -> &Token {
        &self.token
    }
}

pub struct ReturnStatement {
    pub token: Token,
}

impl Statement for ReturnStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_type(&self) -> StatementType {
        StatementType::ReturnStatement
    }

    fn statement_node(&self) {
        todo!("not implemented")
    }
}

impl Node for ReturnStatement {
    fn get_token(&self) -> &Token {
        &self.token
    }
}
