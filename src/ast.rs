use core::fmt::{Debug, Display};
use std::{any::Any};

use crate::token::Token;

pub trait Node {
    fn get_token(&self) -> &Token;
    fn to_string(&self) -> String; // TODO can this be a Display trait?
}

#[derive(Debug, PartialEq)]
pub enum StatementType {
    LetStatement,
    ReturnStatement,
    ExpressionStatement,
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

    fn to_string(&self) -> String {
        let mut string = String::new();
        for s in &self.statements {
            string.push_str(&s.to_string()[..]);
        }
        return string;
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

    fn to_string(&self) -> String {
        self.value.clone()
    }
}

pub struct LetStatement {
    pub token: Token, // Token::Let
    pub name: Identifier,
    pub value: Option<Box<dyn Expression>>,
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

    fn to_string(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("{:?} ", self.token).to_lowercase());
        s.push_str(&format!("{}", self.name.to_string()));
        s.push_str(" = ");
        if let Some(value) = &self.value {
            s.push_str(&format!("{}", value.to_string()));
        }
        s.push_str(";");
        return s;
    }
}

pub struct ReturnStatement {
    pub token: Token, // Token::Return
    pub value: Option<Box<dyn Expression>>,
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

    fn to_string(&self) -> String {
        let mut s = String::new();
        s.push_str(&format!("{:?} ", self.token).to_lowercase());
        if let Some(value) = &self.value {
            s.push_str(&format!("{}", value.to_string()));
        }
        s.push_str(";");
        return s;
    }
}

pub struct ExpressionStatement {
    pub token: Token, // first token of the expression
    pub expression: Option<Box<dyn Expression>>,
}

impl Statement for ExpressionStatement {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn get_type(&self) -> StatementType {
        StatementType::ExpressionStatement
    }

    fn statement_node(&self) {
        todo!("not implemented")
    }
}

impl Node for ExpressionStatement {
    fn get_token(&self) -> &Token {
        &self.token
    }

    fn to_string(&self) -> String {
        let mut s = String::new();
        if let Some(exp) = &self.expression {
            s.push_str(&exp.to_string());
        }
        return s; 
    }
}
