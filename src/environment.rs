use std::collections::HashMap;

use crate::object::Object;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        Environment { 
            store: HashMap::new(),
            outer: None
        }
    }

    pub fn new_enclosed(outer: Environment) -> Environment {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&mut self, value: &String) -> Option<Object> {
        match self.store.get(value) {
            Some(value) => Some(value.clone()),
            None => {
                match self.outer.as_mut() {
                    Some(outer) => outer.get(value),
                    None => None,
                }
            },
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}
