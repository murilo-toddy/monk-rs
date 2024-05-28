use crate::{environment::Environment, ast::{BlockStatement, Identifier}};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Object {
    Integer(i64),
    String(String),
    Boolean(bool),
    ReturnValue(Box<Object>),
    Null,
    Error(String),
    Function(Vec<Identifier>, BlockStatement, Environment),
    BuiltinFunction(fn(Vec<Object>) -> Object),
    Array(Vec<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(value) => value.to_string(),
            Object::String(value) => value.to_owned(),
            Object::Boolean(value) => value.to_string(),
            Object::Null => "null".to_owned(),
            Object::ReturnValue(value) => value.inspect(),
            Object::Error(message) => format!("ERROR: {}", message),
            Object::Function(params, body, _) => {
                let s = params.iter().map(|p| p.value.clone()).collect::<Vec<String>>().join(", ");
                format!("fn({}) {{\n{}\n}}", s, body)
            },
            Object::BuiltinFunction(_) => format!("builtin function"),
            Object::Array(elements) => {
                format!("[{}]", elements.iter().map(|e| e.inspect()).collect::<Vec<String>>().join(", "))
            },
        }
    }
}

