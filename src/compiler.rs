use crate::{ast::{Expression, Program, Statement}, code::{make, Instructions, Opcode}, object::Object};

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        return Compiler {
            instructions: vec![],
            constants: vec![],
        };
    }
    
    fn add_constant(&mut self, obj: Object) -> i64 {
        self.constants.push(obj);
        return (self.constants.len() - 1) as i64;
    }

    fn add_instruction(&mut self, instruction: Vec<u8>) -> usize {
        let new_instruction_idx = instruction.len();
        self.instructions.extend(instruction);
        return new_instruction_idx;
    }

    fn emit(&mut self, op: Opcode, operands: Vec<i64>) -> usize {
        let instruction = make(op, operands);
        return self.add_instruction(instruction);
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<(), String> {
        match expression {
            Expression::Identifier { value, .. } => todo!(),
            Expression::Integer { value, .. } => {
                let obj = Object::Integer(value);
                let pos = self.add_constant(obj);
                self.emit(Opcode::OpConstant, vec![pos]);
                Ok(())
            },
            Expression::String { value, .. } => todo!(),
            Expression::Boolean { value, .. } => todo!(),
            Expression::Prefix { operator, right, .. } => todo!(),
            Expression::Infix { operator, left, right, .. } => {
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                return Ok(());
            },
            Expression::If { conditions, alternative, .. } => todo!(),
            Expression::Function { arguments, body, .. } => todo!(),
            Expression::While { condition, statement, .. } => todo!(),
            Expression::For { declaration, condition, operation, statement, .. } => todo!(),
            Expression::Call { function, arguments, .. } => todo!(),
            Expression::Array { elements, .. } => todo!(),
            Expression::Hash { pairs, .. } => todo!(),
            Expression::Index { left, index, .. } => todo!(),
        }
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), String> {
        match statement {
            Statement::Let { .. } => todo!(),
            Statement::Return { .. } => todo!(),
            Statement::Expression { expression, .. } => {
                expression.map_or(Ok(()), |e| self.compile_expression(e))
            },
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<(), String> {
        for statement in program.0 {
            self.compile_statement(statement);
        }
        Ok(())
    }

    pub fn bytecode(self) -> Bytecode {
        return Bytecode {
            instructions: self.instructions,
            constants: self.constants,
        };
    }
}

#[cfg(test)]
mod compiler_tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::code::{Opcode, make, disassemble};

    struct CompilerTestCase {
        input: &'static str,
        expected_instructions: Vec<Instructions>,
        expected_constants: Vec<Object>,
    }

    fn run_compiler_tests(tests: Vec<CompilerTestCase>) {
        for test in tests {
            let mut compiler = Compiler::new();
            let program = parse(test.input);
            assert!(compiler.compile(program).is_ok());
            let bytecode = compiler.bytecode();
            test_instructions(test.expected_instructions, bytecode.instructions);
            test_constants(test.expected_constants, bytecode.constants);
        }
    }

    fn parse(input: &'static str) -> Program {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        return parser.parse();
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let exp_concat: Instructions = expected.into_iter().flatten().collect();
        assert_eq!(exp_concat, actual, "expected instructions to be {:?} but got {:?}", disassemble(&exp_concat), disassemble(&actual));
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
        assert_eq!(expected, actual, "expected constants to be {:?} but got {:?}", expected, actual);
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::OpConstant, vec![0]),
                    make(Opcode::OpConstant, vec![1]),
                ],
            }
        ];
        run_compiler_tests(tests);
    }
}
