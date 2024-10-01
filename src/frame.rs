use crate::code::Instructions;

#[derive(Clone)]
pub struct Frame {
    function: Instructions,
    pub ip: i64,
}

impl Frame {
    pub fn new(function: Instructions) -> Frame {
        Frame { function, ip: -1 }
    }

    pub fn instructions(&self) -> Instructions {
        self.function.clone()
    }
}
