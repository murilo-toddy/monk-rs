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
            Expression::Identifier { .. } => todo!(),
            Expression::Integer { value, .. } => {
                let obj = Object::Integer(value);
                let pos = self.add_constant(obj);
                self.emit(Opcode::Constant, vec![pos]);
                Ok(())
            },
            Expression::String { .. } => todo!(),
            Expression::Boolean { value, .. } => {
                if value {
                    self.emit(Opcode::True, vec![]);
                    Ok(())
                } else {
                    self.emit(Opcode::False, vec![]);
                    Ok(())
                }
            },
            Expression::Prefix { .. } => todo!(),
            Expression::Infix { operator, left, right, .. } => {
                if operator.as_str() == "<" {
                    self.compile_expression(*right)?;
                    self.compile_expression(*left)?;
                    self.emit(Opcode::GreaterThan, vec![]);
                    return Ok(());
                }
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match operator.as_str() {
                    "+" => self.emit(Opcode::Add, vec![]),
                    "-" => self.emit(Opcode::Sub, vec![]),
                    "*" => self.emit(Opcode::Mul, vec![]),
                    "/" => self.emit(Opcode::Div, vec![]),
                    "==" => self.emit(Opcode::Equal, vec![]),
                    "!=" => self.emit(Opcode::NotEqual, vec![]),
                    ">" => self.emit(Opcode::GreaterThan, vec![]),
                    _ => return Err(format!("unknown operator {}", operator)),
                };
                return Ok(());
            },
            Expression::If { .. } => todo!(),
            Expression::Function { .. } => todo!(),
            Expression::While { .. } => todo!(),
            Expression::For { .. } => todo!(),
            Expression::Call { .. } => todo!(),
            Expression::Array { .. } => todo!(),
            Expression::Hash { .. } => todo!(),
            Expression::Index { .. } => todo!(),
        }
    }

    fn compile_statement(&mut self, statement: Statement) -> Result<(), String> {
        match statement {
            Statement::Let { .. } => todo!(),
            Statement::Return { .. } => todo!(),
            Statement::Expression { expression, .. } => {
                expression.map_or(Ok(()), |e| self.compile_expression(e))?;
                self.emit(Opcode::Pop, vec![]);
                return Ok(())
            },
        }
    }

    pub fn compile(&mut self, program: Program) -> Result<(), String> {
        for statement in program.0 {
            self.compile_statement(statement)?;
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

            let exp_concat: Instructions = test.expected_instructions.into_iter().flatten().collect();
            assert_eq!(exp_concat, bytecode.instructions, 
                       "expected instructions to be {:?} but got {:?} in {}", disassemble(&exp_concat), disassemble(&bytecode.instructions), test.input);
            assert_eq!(test.expected_constants, bytecode.constants,
                       "expected constants to be {:?} but got {:?} in {}", test.expected_constants, bytecode.constants, test.input);
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
            CompilerTestCase {
                input: "1; 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Pop, vec![]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 + 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 - 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Sub, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 * 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Mul, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "2 / 1",
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Div, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "true",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::False, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 > 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::GreaterThan, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 < 2",
                expected_constants: vec![Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::GreaterThan, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 == 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Equal, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 != 2",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::NotEqual, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "true == false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),
                    make(Opcode::False, vec![]),
                    make(Opcode::Equal, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "true != false",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),
                    make(Opcode::False, vec![]),
                    make(Opcode::NotEqual, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }
}
