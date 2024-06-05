use std::collections::HashMap;

use crate::object::Object;

type BuiltinFunction = fn(Vec<Object>) -> Object;

fn validate_args(args: &Vec<Object>, expected_args: usize, func: &str) -> Option<Object> {
    if args.len() != expected_args {
        return Some(Object::Error(
            format!("wrong number of arguments on function `{}`. got={}, want={}", func, args.len(), expected_args)
        ));
    }
    None
}

fn print_function(args: Vec<Object>) -> Object {
    args.iter().for_each(|arg| println!("{}", arg.inspect()));
    Object::Null
}

fn len_function(args: Vec<Object>) -> Object {
    if let Some(error) = validate_args(&args, 1, "len") {
        return error;
    }
    match args[0] {
        Object::String(ref value) => Object::Integer(value.len() as i64),
        Object::Array(ref elems) => Object::Integer(elems.len() as i64),
        _ => Object::Error(format!("argument {} not supported by `len`", args[0].inspect()))
    }
}

fn first_function(args: Vec<Object>) -> Object {
    if let Some(error) = validate_args(&args, 1, "first") {
        return error;
    }
    match &args[0] {
        Object::Array(ref elems) => {
            if elems.is_empty() {
                return Object::Null;
            }
            elems[0].to_owned()
        }
        _ => Object::Error(format!("argument {} not supported by `first`", args[0].inspect()))
    }
}
fn last_function(args: Vec<Object>) -> Object {
    if let Some(error) = validate_args(&args, 1, "last") {
        return error;
    }
    match args[0] {
        Object::Array(ref elems) => {
            if elems.is_empty() {
                return Object::Null;
            }
            elems[elems.len() - 1].to_owned()
        }
        _ => Object::Error(format!("argument {} not supported by `last`", args[0].inspect()))
    }
}

fn rest_function(args: Vec<Object>) -> Object {
    if let Some(error) = validate_args(&args, 1, "rest") {
        return error;
    }
    match args[0] {
        Object::Array(ref elems) => {
            if elems.is_empty() {
                return Object::Null;
            }
            Object::Array(elems[1..].to_owned())
        }
        _ => Object::Error(format!("argument {} not supported by `rest`", args[0].inspect()))
    }
}

fn push_function(args: Vec<Object>) -> Object {
    if let Some(error) = validate_args(&args, 2, "push") {
        return error;
    }
    match args[0] {
        Object::Array(ref elems) => {
            let mut new_elems = elems.clone();
            new_elems.push(args[1].to_owned());
            Object::Array(new_elems)
        }
        _ => Object::Error(format!("argument {} not supported by `push`", args[0].inspect()))
    }
}

pub struct BuiltinFunctions {
    store: HashMap<String, Object>
}

impl Default for BuiltinFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinFunctions {
    fn get_store() -> HashMap<String, Object> {
        let functions = [
            ("len".to_owned(), len_function as BuiltinFunction),
            ("first".to_owned(), first_function as BuiltinFunction),
            ("last".to_owned(), last_function as BuiltinFunction),
            ("rest".to_owned(), rest_function as BuiltinFunction),
            ("push".to_owned(), push_function as BuiltinFunction),
            ("print".to_owned(), print_function as BuiltinFunction),
        ];
        functions
            .iter()
            .map(|(name, func)| (name.to_owned(), Object::BuiltinFunction(func.to_owned())))
            .collect::<Vec<(String, Object)>>()
            .into_iter()
            .collect()
    }

    pub fn new() -> BuiltinFunctions {
        BuiltinFunctions {
            store: BuiltinFunctions::get_store()
        }
    }

    pub fn get_function_object(&self, value: &String) -> Option<Object> {
        self.store.get(value).cloned()
    }
}
