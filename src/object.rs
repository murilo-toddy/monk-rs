use crate::{environment::Environment, ast::{BlockStatement, Identifier}};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Null,
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Environment),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_owned(),
            Object::ReturnValue(value) => value.inspect(),
            Object::Error(message) => format!("ERROR: {}", message),
            Object::Function(params, body, _) => {
                let s = params.iter().map(|p| p.value.clone()).collect::<Vec<String>>().join(", ");
                format!("fn({}) {{\n{}\n}}", s, body)
            },
        }
    }
}

