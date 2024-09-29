use crate::{code::{Instructions, Opcode}, compiler::{Bytecode, Compiler}, object::Object};

const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Option<Object>>, // TODO is this gonna haunt me?
    sp: usize, // top of the stack is stac[sp - 1]
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![None; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn stack_top(&mut self) -> Option<Object> {
        match self.sp {
            0 => None,
            _ => self.stack[self.sp - 1].clone(),
        }
    }

    fn push(&mut self, obj: Object) -> Result<(), String> {
        if self.sp >= STACK_SIZE {
            return Err("stack overflow".to_owned());
        }
        self.stack[self.sp] = Some(obj);
        self.sp += 1;
        return Ok(())
    }

    fn pop(&mut self) -> Option<Object> {
        if self.sp <= 0 {
            return None
        }
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        return obj;
    }

    pub fn run(&mut self) -> Result<(), String> {
        let mut ip = 0;
        while ip < self.instructions.len() {
            if let Some(op) = Opcode::from(self.instructions[ip]) {
                match op {
                    Opcode::OpConstant => {
                        let index = u16::from_be_bytes(self.instructions[ip+1..ip+3].try_into().unwrap());
                        ip += 2;
                        self.push(self.constants[index as usize].clone())?;
                    }
                    Opcode::OpAdd => {
                        let left = self.pop();
                        let right = self.pop();
                        match (left, right) {
                            (Some(Object::Integer(left)), Some(Object::Integer(right))) => {
                                self.push(Object::Integer(left + right))?;
                            },
                            _ => {},
                        }
                    }
                    _ => panic!("branch not covered")
                }
                ip += 1;
            } else {
                return Err(format!("Opcode for {} not found", self.instructions[ip]));
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod vm_tests {
    use super::*;
    use crate::{ast::Program, lexer::Lexer, parser::Parser};

    struct VmTestCase {
        input: &'static str,
        expected: Object,
    }

    fn run_vm_tests(tests: Vec<VmTestCase>) {
        for test in tests {
            let mut compiler = Compiler::new();
            let program = parse(test.input);
            if let Err(msg) = compiler.compile(program) {
                panic!("compiler error {}", msg);
            }
            let mut vm = Vm::new(compiler.bytecode());
            if let Err(msg) = vm.run() {
                panic!("vm error {}", msg);
            }
            let stack_element = vm.stack_top();
            assert_eq!(Some(test.expected), stack_element);
        }
    }

    fn parse(input: &'static str) -> Program {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        return parser.parse();
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            VmTestCase { input: "1", expected: Object::Integer(1) },
            VmTestCase { input: "2", expected: Object::Integer(2) },
            VmTestCase { input: "1 + 2", expected: Object::Integer(3) },
        ];
        run_vm_tests(tests);
    }
}
