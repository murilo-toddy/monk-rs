use crate::{code::{Instructions, Opcode}, compiler::Bytecode, object::Object};

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

    pub fn last_popped_elem(&mut self) -> Option<Object> {
        return self.stack[self.sp].clone();
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

    fn execute_binary_integer_operation(&mut self, op: Opcode, left: i64, right: i64) -> Result<(), String> {
        let result = match op {
            Opcode::OpAdd => left + right,
            Opcode::OpSub => left - right,
            Opcode::OpMul => left * right,
            Opcode::OpDiv => left / right,
            _ => return Err(format!("ERROR: unknown integer operation {:?}", op)),
        };
        self.push(Object::Integer(result))
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (left, right) {
            (Some(Object::Integer(left)), Some(Object::Integer(right))) => {
                self.execute_binary_integer_operation(op, left, right)?;
            },
            _ => {},
        }
        Ok(())
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
                    Opcode::OpPop => {
                        self.pop();
                    },
                    Opcode::OpAdd | Opcode::OpSub | Opcode::OpMul | Opcode::OpDiv => {
                        self.execute_binary_operation(op)?;
                    }
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
    use crate::{ast::Program, lexer::Lexer, parser::Parser, compiler::Compiler};

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
            let stack_element = vm.last_popped_elem();
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
            VmTestCase { input: "1 - 2", expected: Object::Integer(-1) },
            VmTestCase { input: "1 * 2", expected: Object::Integer(2) },
            VmTestCase { input: "4 / 2", expected: Object::Integer(2) },
            VmTestCase { input: "50 / 2 * 2 + 10 - 5", expected: Object::Integer(55) },
            VmTestCase { input: "5 * (2 + 10)", expected: Object::Integer(60) },
            VmTestCase { input: "5 + 5 + 5 + 5 - 10", expected: Object::Integer(10) },
            VmTestCase { input: "2 * 2 * 2 * 2 * 2", expected: Object::Integer(32) },
            VmTestCase { input: "5 * 2 + 10", expected: Object::Integer(20) },
            VmTestCase { input: "5 + 2 * 10", expected: Object::Integer(25) },
            VmTestCase { input: "5 * (2 + 10)", expected: Object::Integer(60) },
        ];
        run_vm_tests(tests);
    }
}
