use std::collections::HashMap;

use crate::object::Object;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment { store: HashMap::new() }
    }

    pub fn get(&mut self, value: &String) -> Option<Object> {
        match self.store.get(value) {
            Some(value) => Some(value.clone()),
            None => None,
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
