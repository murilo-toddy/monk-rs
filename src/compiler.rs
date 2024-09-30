use crate::{ast::{BlockStatement, Expression, Program, Statement}, code::{make, disassemble, Instructions, Opcode}, object::Object};

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Clone, Debug)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,

    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Compiler {
        return Compiler {
            instructions: vec![],
            constants: vec![],

            last_instruction: None,
            previous_instruction: None,
        };
    }
    
    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.last_instruction.clone();
        let last = EmittedInstruction {
            opcode: op,
            position: pos,
        };
        self.previous_instruction = previous;
        self.last_instruction = Some(last);
    }

    fn is_last_instruction_pop(&self) -> bool {
        return self.last_instruction.as_ref().map_or(false, |i| i.opcode == Opcode::Pop);
    }

    fn remove_last_pop(&mut self) {
        if !self.is_last_instruction_pop() {
            return;
        }
        if let Some(last_instruction) = &self.last_instruction {
            self.instructions = self.instructions[..last_instruction.position].to_vec();
            self.last_instruction = self.previous_instruction.clone();
        }
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Instructions) {
        self.instructions[position..position + new_instruction.len()].copy_from_slice(&new_instruction);
    }

    // NOTE: should be used only for replacing instructions with the same number of operands
    fn change_operand(&mut self, position: usize, operand: i64) {
        if let Some(new_instruction) = Opcode::from(self.instructions[position]).map(|op| make(op, vec![operand])) {
            self.replace_instruction(position, new_instruction);
        }
    }

    fn add_constant(&mut self, obj: Object) -> i64 {
        self.constants.push(obj);
        return (self.constants.len() - 1) as i64;
    }

    fn add_instruction(&mut self, instruction: Vec<u8>) -> usize {
        let new_instruction_idx = self.instructions.len();
        self.instructions.extend(instruction);
        return new_instruction_idx;
    }

    fn emit(&mut self, op: Opcode, operands: Vec<i64>) -> usize {
        let instruction = make(op.clone(), operands);
        let position = self.add_instruction(instruction);
        self.set_last_instruction(op, position);
        return position;
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
            Expression::Prefix { operator, right, .. } => {
                self.compile_expression(*right)?;
                match operator.as_str() {
                    "-" => self.emit(Opcode::Minus, vec![]),
                    "!" => self.emit(Opcode::Bang, vec![]),
                    _ => return Err(format!("unknown prefix operator {}", operator)),
                };
                return Ok(());
            },
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
                    _ => return Err(format!("unknown infix operator {}", operator)),
                };
                return Ok(());
            },
            Expression::If { conditions, alternative, .. } => {
                // TODO: add support for else if
                let (condition, consequence) = conditions[0].clone();
                self.compile_expression(condition)?;
                let jump_not_true_pos = self.emit(Opcode::JumpNotTrue, vec![9999]);

                self.compile_block_statement(consequence)?;
                self.remove_last_pop();

                let jump_pos = self.emit(Opcode::Jump, vec![9999]);

                let after_consequence_pos = self.instructions.len();
                self.change_operand(jump_not_true_pos, after_consequence_pos as i64);

                match alternative {
                    Some(alternative) => {
                        self.compile_block_statement(alternative)?;
                        self.remove_last_pop();
                    }
                    None => {
                        self.emit(Opcode::Null, vec![]);
                    }
                }

                let after_alternative_pos = self.instructions.len();
                self.change_operand(jump_pos, after_alternative_pos as i64);

                return Ok(());
            },
            Expression::Function { .. } => todo!(),
            Expression::While { .. } => todo!(),
            Expression::For { .. } => todo!(),
            Expression::Call { .. } => todo!(),
            Expression::Array { .. } => todo!(),
            Expression::Hash { .. } => todo!(),
            Expression::Index { .. } => todo!(),
        }
    }

    fn compile_block_statement(&mut self, block: BlockStatement) -> Result<(), String> {
        for statement in block.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
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
                       "wrong instructions\n    expected: {:?}\n    got:      {:?}\n    in {}", disassemble(&exp_concat), disassemble(&bytecode.instructions), test.input);
            assert_eq!(test.expected_constants, bytecode.constants,
                       "wrong constants\n       expected: {:?}\n    got:      {:?}\n    in {}", test.expected_constants, bytecode.constants, test.input);
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
            CompilerTestCase {
                input: "-1",
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Minus, vec![]),
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
            CompilerTestCase {
                input: "!true",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),
                    make(Opcode::Bang, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: "if (true) { 10 } else { 20 }; 3333",
                expected_constants: vec![Object::Integer(10), Object::Integer(20), Object::Integer(3333)],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),          // 0000
                    make(Opcode::JumpNotTrue, vec![10]), // 0001
                    make(Opcode::Constant, vec![0]),     // 0004
                    make(Opcode::Jump, vec![13]),        // 0007
                    make(Opcode::Constant, vec![1]),     // 0010
                    make(Opcode::Pop, vec![]),           // 0013
                    make(Opcode::Constant, vec![2]),     // 0014
                    make(Opcode::Pop, vec![]),           // 0017
                ],
            },
            CompilerTestCase {
                input: "if (true) { 10 }; 3333",
                expected_constants: vec![Object::Integer(10), Object::Integer(3333)],
                expected_instructions: vec![
                    make(Opcode::True, vec![]),          // 0000
                    make(Opcode::JumpNotTrue, vec![10]), // 0001
                    make(Opcode::Constant, vec![0]),     // 0004
                    make(Opcode::Jump, vec![11]),        // 0007
                    make(Opcode::Null, vec![]),          // 0010
                    make(Opcode::Pop, vec![]),           // 0011
                    make(Opcode::Constant, vec![1]),     // 0012
                    make(Opcode::Pop, vec![]),           // 0015
                ],
            },
        ];
        run_compiler_tests(tests)
    }
}
