use std::collections::HashMap;

use crate::object::Object;

pub struct BuiltinFunctions {
    store: HashMap<String, Object>
}

impl Default for BuiltinFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinFunctions {
    fn print_function(args: Vec<Object>) -> Object {
        args.iter().for_each(|arg| println!("{}", arg.inspect()));
        Object::Null
    }

    fn len_function(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error(
                format!("wrong number of arguments on function `len`. got={}, want=1", args.len())
            );
        }
        match args[0] {
            Object::String(ref value) => Object::Integer(value.len() as i64),
            Object::Array(ref elems) => Object::Integer(elems.len() as i64),
            _ => Object::Error("argument to `len` not supported".to_owned())
        }
    }

    fn first_function(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error(
                format!("wrong number of arguments on function `first`. got={}, want=1", args.len())
            );
        }
        match args[0] {
            Object::Array(ref elems) => {
                if elems.len() < 1 {
                    return Object::Null;
                }
                return elems[0].to_owned();
            }
            _ => Object::Error("argument to `first` not supported".to_owned())
        }
    }

    fn last_function(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error(
                format!("wrong number of arguments on function `last`. got={}, want=1", args.len())
            );
        }
        match args[0] {
            Object::Array(ref elems) => {
                if elems.len() < 1 {
                    return Object::Null;
                }
                return elems[elems.len() - 1].to_owned();
            }
            _ => Object::Error("argument to `last` not supported".to_owned())
        }
    }

    fn rest_function(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error(
                format!("wrong number of arguments on function `rest`. got={}, want=1", args.len())
            );
        }
        match args[0] {
            Object::Array(ref elems) => {
                if elems.len() < 1 {
                    return Object::Null;
                }
                return Object::Array(elems[1..].to_owned());
            }
            _ => Object::Error("argument to `rest` not supported".to_owned())
        }
    }

    fn push_function(args: Vec<Object>) -> Object {
        if args.len() != 2 {
            return Object::Error(
                format!("wrong number of arguments on function `rest`. got={}, want=2", args.len())
            );
        }
        match args[0] {
            Object::Array(ref elems) => {
                let mut new_elems = elems.clone();
                new_elems.push(args[1].to_owned());
                Object::Array(new_elems)
            }
            _ => Object::Error("argument to `push` not supported".to_owned())
        }
    }

    fn get_store() -> HashMap<String, Object> {
        let functions = [
            // TODO simplify
            ("len".to_owned(), BuiltinFunctions::len_function as fn(Vec<Object>) -> Object),
            ("first".to_owned(), BuiltinFunctions::first_function as fn(Vec<Object>) -> Object),
            ("last".to_owned(), BuiltinFunctions::last_function as fn(Vec<Object>) -> Object),
            ("rest".to_owned(), BuiltinFunctions::rest_function as fn(Vec<Object>) -> Object),
            ("push".to_owned(), BuiltinFunctions::push_function as fn(Vec<Object>) -> Object),
            ("print".to_owned(), BuiltinFunctions::print_function as fn(Vec<Object>) -> Object),
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
