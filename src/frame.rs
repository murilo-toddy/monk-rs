use crate::{code::Instructions, object::Object};

#[derive(Clone)]
pub struct Frame {
    pub closure: Object, // Object::Closure
    pub base_pointer: usize,
    pub ip: i64,
}

impl Frame {
    pub fn new(closure: Object, base_pointer: usize) -> Frame {
        match closure {
            Object::Closure { .. } => Frame {
                closure,
                base_pointer,
                ip: -1,
            },
            _ => Frame { closure: Object::Null, base_pointer: 0, ip: 0 }
        }
    }

    pub fn instructions(&self) -> Instructions {
        // TODO this is ass
        if let Object::Closure { function, .. } = self.closure.clone() {
            if let Object::CompiledFunction { function, .. } = *function {
                return function
            }
        }
        vec![]
    }
}
