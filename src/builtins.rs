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
    fn len_function(args: Vec<Object>) -> Object {
        if args.len() != 1 {
            return Object::Error(
                format!("wrong number of arguments on function `len`. got={}, want=1", args.len())
            );
        }
        match args[0] {
            Object::String(ref value) => Object::Integer(value.len() as i64),
            _ => Object::Error("argument to `len` not supported".to_owned())
        }
    }

    fn get_store() -> HashMap<String, Object> {
        let functions = [
            ("len".to_owned(), BuiltinFunctions::len_function)
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
