use crate::code::Instructions;

#[derive(Clone)]
pub struct Frame {
    function: Instructions,
    pub base_pointer: usize,
    pub ip: i64,
}

impl Frame {
    pub fn new(function: Instructions, base_pointer: usize) -> Frame {
        Frame { 
            function,
            base_pointer,
            ip: -1,
        }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.clone()
    }
}
