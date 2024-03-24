use std::collections::HashMap;

use crate::object::Object;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, value: &String) -> Option<Object> {
        // TODO remove this clone
        if let Some(obj) = self.store.get(value) {
            Some(obj.clone())
        } else {
            None
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Option<Object> {
        self.store.insert(name, value)
    }
}
