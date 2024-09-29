use crate::{code::Instructions, compiler::{Bytecode, Compiler}, object::Object};

const STACK_SIZE: usize = 2048;

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Option<Object>>, // TODO is this gonna haunt me?
    sp: i64, // top of the stack is stac[sp - 1]
}

impl Vm {
    fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,

            stack: vec![None; STACK_SIZE],
            sp: 0,
        }
    }

    fn run(&mut self) {
    }

    fn stack_top(&mut self) {
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
            let vm = Vm::new(compiler.bytecode());
            if let Err(msg) = vm.run() {
                panic!("vm error {}", msg);
            }
            let stack_element = vm.stack_top();
            assert_eq!(test.expected, stack_element);
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
            VmTestCase { input: "1 + 2", expected: Object::Integer(2) }, // TODO: fixme
        ];
        run_vm_tests(tests);
    }
}
