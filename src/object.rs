use std::{collections::HashMap, hash::{Hash, Hasher}};

use crate::{environment::Environment, ast::{BlockStatement, Identifier}};

#[derive(PartialEq, Clone, Debug)]
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
    Hash(HashMap<Object, Object>),
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
            Object::BuiltinFunction(_) => "builtin function".to_owned(),
            Object::Array(elements) => {
                format!("[{}]", elements.iter().map(|e| e.inspect()).collect::<Vec<String>>().join(", "))
            },
            Object::Hash(pairs) => {
                format!("{{{}}}", pairs.iter().map(|(k, v)| format!("{}: {}", k.inspect(), v.inspect())).collect::<Vec<String>>().join(", "))
            },
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Integer(ref i) => i.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
