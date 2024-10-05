use std::{cell::RefCell, rc::Rc};

use crate::{ast::{BlockStatement, Expression, Program, Statement}, builtins::BUILTINS, code::{make, Instructions, Opcode}, object::Object, symbol::{Symbol, SymbolScope, SymbolTable}};

#[derive(Clone, Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct EmittedInstruction {
    opcode: Opcode,
    position: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Compiler {
    constants: Vec<Object>,

    symbol_table: Rc<RefCell<SymbolTable>>,

    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Compiler {
        let main_scope = CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        };
        let mut symbol_table = SymbolTable::new();
        for (index, function) in BUILTINS.into_iter().enumerate() {
            symbol_table.define_builtin(index, function.0);
        }
        Compiler {
            constants: vec![],

            symbol_table: Rc::from(RefCell::new(symbol_table)),

            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn reset(&mut self) -> Compiler {
        let main_scope = CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        };
        Compiler {
            constants: self.constants.clone(),

            symbol_table: self.symbol_table.clone(),

            scopes: vec![main_scope],
            scope_index: 0,
        }
    }

    pub fn enter_scope(&mut self) {
        let scope = CompilationScope {
            instructions: vec![],
            last_instruction: None,
            previous_instruction: None,
        };
        self.scopes.push(scope);
        self.scope_index += 1;
        self.symbol_table = Rc::from(RefCell::new(SymbolTable::enclosing(Rc::clone(&self.symbol_table))));
    }

    pub fn leave_scope(&mut self) -> Result<Instructions, String> {
        let instructions = self.current_instructions();

        self.scopes.pop();
        self.scope_index -= 1;

        let outer_table = {
            let borrowed_table = self.symbol_table.borrow();
            match &borrowed_table.outer {
                Some(table) => Rc::clone(table),
                None => return Err("compiler lost reference to outer table when leaving scope".to_string()),
            }
        };
        self.symbol_table = Rc::clone(&outer_table);
        
        Ok(instructions)
    }

    fn current_instructions(&self) -> Instructions {
        self.scopes[self.scope_index].instructions.clone()
    }

    fn set_last_instruction(&mut self, op: Opcode, pos: usize) {
        let previous = self.scopes[self.scope_index].last_instruction.clone();
        let last = EmittedInstruction { opcode: op, position: pos };
        self.scopes[self.scope_index].previous_instruction = previous;
        self.scopes[self.scope_index].last_instruction = Some(last);
    }

    fn is_last_instruction(&self, op: Opcode) -> bool {
        if self.current_instructions().is_empty() {
            return false
        }
        return self.scopes[self.scope_index].last_instruction.as_ref().map_or(false, |i| i.opcode == op);
    }

    fn remove_last_pop(&mut self) {
        if !self.is_last_instruction(Opcode::Pop) {
            return;
        }
        if let Some(last_instruction) = &self.scopes[self.scope_index].last_instruction {
            self.scopes[self.scope_index].instructions = self.scopes[self.scope_index].instructions[..last_instruction.position].to_vec();
            self.scopes[self.scope_index].last_instruction = self.scopes[self.scope_index].previous_instruction.clone();
        }
    }

    fn replace_last_pop_with(&mut self, op: Opcode) {
        if !self.is_last_instruction(Opcode::Pop) {
            return;
        }
        if let Some(ref mut last_instruction) = self.scopes[self.scope_index].last_instruction {
            let last_position = last_instruction.position;
            self.replace_instruction(last_position, make(Opcode::ReturnValue, vec![]));
        }
        if let Some(instruction) = self.scopes[self.scope_index].last_instruction.as_mut() {
            instruction.opcode = op
        };
    }

    fn replace_instruction(&mut self, position: usize, new_instruction: Instructions) {
        self.scopes[self.scope_index].instructions[position..position + new_instruction.len()].copy_from_slice(&new_instruction);
    }

    // NOTE: should be used only for replacing instructions with the same number of operands
    fn change_operand(&mut self, position: usize, operand: i64) {
        if let Some(new_instruction) = Opcode::from(self.scopes[self.scope_index].instructions[position]).map(|op| make(op, vec![operand])) {
            self.replace_instruction(position, new_instruction);
        }
    }

    fn add_constant(&mut self, obj: Object) -> i64 {
        self.constants.push(obj);
        (self.constants.len() - 1) as i64
    }

    fn add_instruction(&mut self, instruction: Vec<u8>) -> usize {
        let new_instruction_idx = self.current_instructions().len();
        self.scopes[self.scope_index].instructions.extend(instruction);
        new_instruction_idx
    }

    fn emit(&mut self, op: Opcode, operands: Vec<i64>) -> usize {
        let instruction = make(op.clone(), operands);
        let position = self.add_instruction(instruction);
        self.set_last_instruction(op, position);
        position
    }

    fn compile_key_value_pair(&mut self, key: Expression, value: Expression) -> Result<(), String> {
        self.compile_expression(key)?;
        self.compile_expression(value)
    }

    fn load_symbol(&mut self, symbol: Symbol) {
        match symbol.scope {
            SymbolScope::Local => self.emit(Opcode::GetLocal, vec![symbol.index as i64]),
            SymbolScope::Global => self.emit(Opcode::GetGlobal, vec![symbol.index as i64]),
            SymbolScope::Builtin => self.emit(Opcode::GetBuiltin, vec![symbol.index as i64]),
            SymbolScope::Free => self.emit(Opcode::GetFree, vec![symbol.index as i64]),
        };
    }

    fn compile_expression(&mut self, expression: Expression) -> Result<(), String> {
        match expression {
            Expression::Identifier { value, .. } => {
                let symbol = match self.symbol_table.borrow_mut().resolve(value) {
                    Some(symbol) => symbol,
                    None => return Err(format!("undefined variable {}", value)),
                };
                self.load_symbol(symbol);
                Ok(())
            },
            Expression::Integer { value, .. } => {
                let obj = Object::Integer(value);
                let pos = self.add_constant(obj);
                self.emit(Opcode::Constant, vec![pos]);
                Ok(())
            },
            Expression::String { value, .. } => {
                let pos = self.add_constant(Object::String(value));
                self.emit(Opcode::Constant, vec![pos]);
                Ok(())
            },
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
                Ok(())
            },
            Expression::Infix { operator, left, right, .. } => {
                if operator == "<" {
                    self.compile_expression(*right)?;
                    self.compile_expression(*left)?;
                    self.emit(Opcode::GreaterThan, vec![]);
                    return Ok(());
                }
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                match operator {
                    "+" => self.emit(Opcode::Add, vec![]),
                    "-" => self.emit(Opcode::Sub, vec![]),
                    "*" => self.emit(Opcode::Mul, vec![]),
                    "/" => self.emit(Opcode::Div, vec![]),
                    "==" => self.emit(Opcode::Equal, vec![]),
                    "!=" => self.emit(Opcode::NotEqual, vec![]),
                    ">" => self.emit(Opcode::GreaterThan, vec![]),
                    _ => return Err(format!("unknown infix operator {}", operator)),
                };
                Ok(())
            },
            Expression::If { conditions, alternative, .. } => {
                // TODO: add support for else if
                let (condition, consequence) = conditions[0].clone();
                self.compile_expression(condition)?;
                let jump_not_true_pos = self.emit(Opcode::JumpNotTrue, vec![9999]);

                self.compile_block_statement(consequence)?;
                self.remove_last_pop();

                let jump_pos = self.emit(Opcode::Jump, vec![9999]);

                let after_consequence_pos = self.current_instructions().len();
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

                let after_alternative_pos = self.current_instructions().len();
                self.change_operand(jump_pos, after_alternative_pos as i64);

                Ok(())
            },
            Expression::Function { body, arguments, .. } => {
                self.enter_scope();
                let parameters_count = arguments.len();
                for argument in arguments {
                    self.symbol_table.borrow_mut().define(argument.value);
                }
                self.compile_block_statement(body)?;
                self.replace_last_pop_with(Opcode::ReturnValue);
                if !self.is_last_instruction(Opcode::ReturnValue) {
                    self.emit(Opcode::Return, vec![]);
                }
                let free_symbols = self.symbol_table.borrow().free_symbols.clone();
                let free_symbols_count = free_symbols.len() as i64;

                let locals_count = self.symbol_table.borrow().definition_count;
                let instructions = self.leave_scope()?;

                for symbol in free_symbols {
                    self.load_symbol(symbol);
                }

                let function_index = self.add_constant(Object::CompiledFunction {
                        function: instructions,
                        parameters_count,
                        locals_count,
                });
                self.emit(Opcode::Closure, vec![function_index, free_symbols_count]);
                Ok(())
            },
            Expression::While { .. } => todo!(),
            Expression::For { .. } => todo!(),
            Expression::Call { function, arguments, .. } => {
                self.compile_expression(*function)?;
                let arguments_count = arguments.len() as i64;
                for argument in arguments {
                    self.compile_expression(argument)?;
                }
                self.emit(Opcode::Call, vec![arguments_count]);
                Ok(())
            },
            Expression::Array { elements, .. } => {
                let array_len = elements.len();
                elements.into_iter().try_for_each(|e| self.compile_expression(e))?;
                self.emit(Opcode::Array, vec![array_len as i64]);
                Ok(())
            },
            Expression::Hash { pairs, .. } => {
                let elements_count = 2 * pairs.len();
                let mut sorted_pairs = pairs.clone();
                sorted_pairs.sort_by_key(|(k1, k2)| k1 < k2);
                sorted_pairs.into_iter().try_for_each(|(k, v)| self.compile_key_value_pair(k, v))?;
                self.emit(Opcode::Hash, vec![elements_count as i64]);
                Ok(())
            },
            Expression::Index { left, index, .. } => {
                self.compile_expression(*left)?;
                self.compile_expression(*index)?;
                self.emit(Opcode::Index, vec![]);
                Ok(())
            },
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
            Statement::Let { name, value, .. } => {
                if let Some(value) = value {
                    let symbol = self.symbol_table.borrow_mut().define(name.value);
                    self.compile_expression(value)?;
                    match symbol.scope {
                        SymbolScope::Local => self.emit(Opcode::SetLocal, vec![symbol.index as i64]),
                        SymbolScope::Global => self.emit(Opcode::SetGlobal, vec![symbol.index as i64]),
                        // TODO
                        SymbolScope::Builtin => todo!(),
                        SymbolScope::Free => todo!()
                    };
                }
                Ok(())
            },
            Statement::Return { value, .. } => {
                if let Some(value) = value {
                    self.compile_expression(value)?;
                    self.emit(Opcode::ReturnValue, vec![]);
                }
                Ok(())
            },
            Statement::Expression { expression, .. } => {
                expression.map_or(Ok(()), |e| self.compile_expression(e))?;
                self.emit(Opcode::Pop, vec![]);
                Ok(())
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
        Bytecode {
            instructions: self.current_instructions(),
            constants: self.constants,
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
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
            if let Err(msg) = compiler.compile(program) {
                panic!("unable to compile program {}: {}", test.input, msg);
            }
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

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            CompilerTestCase {
                input: "let one = 1; let two = 2; one;",
                expected_constants: vec![Object::Integer(1), Object::Integer(2)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::SetGlobal, vec![1]),
                    make(Opcode::GetGlobal, vec![0]),
                    make(Opcode::Pop, vec![0]),
                ],
            },
            CompilerTestCase {
                input: "let one = 1; let two = one; two;",
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::GetGlobal, vec![0]),
                    make(Opcode::SetGlobal, vec![1]),
                    make(Opcode::GetGlobal, vec![1]),
                    make(Opcode::Pop, vec![0]),
                ],
            }
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "\"monkey\"",
                expected_constants: vec![Object::String("monkey")],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Pop, vec![0]),
                ],
            },
            CompilerTestCase {
                input: "\"mon\" + \"key\"",
                expected_constants: vec![Object::String("mon"), Object::String("key")],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Pop, vec![0]),
                ],
            }
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            CompilerTestCase {
                input: "[]",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::Array, vec![0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "[1, 2, 3]",
                expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Array, vec![3]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "[1 + 2, 3 - 4, 5 * 6]",
                expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(4), Object::Integer(5), Object::Integer(6)],
                expected_instructions:  vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Sub, vec![]),
                    make(Opcode::Constant, vec![4]),
                    make(Opcode::Constant, vec![5]),
                    make(Opcode::Mul, vec![]),
                    make(Opcode::Array, vec![3]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            CompilerTestCase {
                input: "{}",
                expected_constants: vec![],
                expected_instructions: vec![
                    make(Opcode::Hash, vec![0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2, 3: 4, 5: 6}",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Constant, vec![4]),
                    make(Opcode::Constant, vec![5]),
                    make(Opcode::Hash, vec![6]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2 + 3, 4: 5 * 6}",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Constant, vec![4]),
                    make(Opcode::Constant, vec![5]),
                    make(Opcode::Mul, vec![]),
                    make(Opcode::Hash, vec![4]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            CompilerTestCase {
                input: "[1, 2, 3][1 + 1]",
                expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(3), Object::Integer(1), Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Array, vec![3]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Constant, vec![4]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Index, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2}[2 - 1]",
                expected_constants: vec![Object::Integer(1), Object::Integer(2), Object::Integer(2), Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Hash, vec![2]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Sub, vec![]),
                    make(Opcode::Index, vec![]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "{1: 2 + 3, 4: 5 * 6}",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Add, vec![]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Constant, vec![4]),
                    make(Opcode::Constant, vec![5]),
                    make(Opcode::Mul, vec![]),
                    make(Opcode::Hash, vec![4]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_functions() {
        let tests = vec![
            CompilerTestCase {
                input: "fn() { return 5 + 10; }",
                expected_constants: vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::Constant, vec![1]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![2, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { 5 + 10; }",
                expected_constants: vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::Constant, vec![1]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![2, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { return; }",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: make(Opcode::Return, vec![]),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![0, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { }",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: make(Opcode::Return, vec![]),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![0, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { 1; 2 }",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::Pop, vec![]),
                            make(Opcode::Constant, vec![1]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![2, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { 1 }()",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![1, 0]),
                    make(Opcode::Call, vec![0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "let noArg = fn() { 1 }; noArg()",
                expected_constants: vec![
                    Object::Integer(1),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![1, 0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::GetGlobal, vec![0]),
                    make(Opcode::Call, vec![0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "let oneArg = fn(a) { a; }; oneArg(24)",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 1,
                        locals_count: 1,
                    },
                    Object::Integer(24),
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![0, 0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::GetGlobal, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Call, vec![1]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "let manyArg = fn(a, b, c) { a; b; c; }; manyArg(24, 25, 26)",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Pop, vec![]),
                            make(Opcode::GetLocal, vec![1]),
                            make(Opcode::Pop, vec![]),
                            make(Opcode::GetLocal, vec![2]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 3,
                        locals_count: 3,
                    },
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![0, 0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::GetGlobal, vec![0]),
                    make(Opcode::Constant, vec![1]),
                    make(Opcode::Constant, vec![2]),
                    make(Opcode::Constant, vec![3]),
                    make(Opcode::Call, vec![3]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_let_statement_scopes() {
        let tests = vec![
            CompilerTestCase {
                input: "let num = 55; fn() { num }",
                expected_constants: vec![
                    Object::Integer(55),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetGlobal, vec![0]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::Closure, vec![1, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { let num = 55; num }",
                expected_constants: vec![
                    Object::Integer(55),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::SetLocal, vec![0]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 1,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![1, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { let a = 55; let b = 77; a + b; }",
                expected_constants: vec![
                    Object::Integer(55),
                    Object::Integer(77),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![0]),
                            make(Opcode::SetLocal, vec![0]),
                            make(Opcode::Constant, vec![1]),
                            make(Opcode::SetLocal, vec![1]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::GetLocal, vec![1]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        parameters_count: 0,
                        locals_count: 2,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![2, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            CompilerTestCase {
                input: "len([]); push([], 1);",
                expected_constants: vec![Object::Integer(1)],
                expected_instructions: vec![
                    make(Opcode::GetBuiltin, vec![0]),
                    make(Opcode::Array, vec![0]),
                    make(Opcode::Call, vec![1]),
                    make(Opcode::Pop, vec![]),
                    make(Opcode::GetBuiltin, vec![4]),
                    make(Opcode::Array, vec![0]),
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::Call, vec![2]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "fn() { len([]); }",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetBuiltin, vec![0]),
                            make(Opcode::Array, vec![0]),
                            make(Opcode::Call, vec![1]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 0,
                        parameters_count: 0,
                    }
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![0, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_closures() {
        let tests = vec![
            CompilerTestCase {
                input: "fn(a) { fn(b) { a + b; } }",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetFree, vec![0]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 1,
                    },
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Closure, vec![0, 1]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 1,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![1, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "
                    fn(a) {
                        fn(b) {
                            fn(c) {
                            a + b + c
                            }
                        }
                    };
                ",
                expected_constants: vec![
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetFree, vec![0]),
                            make(Opcode::GetFree, vec![1]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 1,
                    },
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetFree, vec![0]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Closure, vec![0, 2]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 1,
                    },
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Closure, vec![1, 1]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 1,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Closure, vec![2, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "
                    let global = 55;
                        fn() {
                            let a = 66;
                            fn() {
                            let b = 77;
                                fn() {
                                let c = 88;
                                global + a + b + c;
                            }
                        }
                    }
                ",
                expected_constants: vec![
                    Object::Integer(55),
                    Object::Integer(66),
                    Object::Integer(77),
                    Object::Integer(88),
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![3]),
                            make(Opcode::SetLocal, vec![0]),
                            make(Opcode::GetGlobal, vec![0]),
                            make(Opcode::GetFree, vec![0]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::GetFree, vec![1]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Add, vec![]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 0,
                    },
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![2]),
                            make(Opcode::SetLocal, vec![0]),
                            make(Opcode::GetFree, vec![0]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Closure, vec![4, 2]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 0,
                    },
                    Object::CompiledFunction {
                        function: vec![
                            make(Opcode::Constant, vec![1]),
                            make(Opcode::SetLocal, vec![0]),
                            make(Opcode::GetLocal, vec![0]),
                            make(Opcode::Closure, vec![5, 1]),
                            make(Opcode::ReturnValue, vec![]),
                        ].into_iter().flatten().collect(),
                        locals_count: 1,
                        parameters_count: 0,
                    },
                ],
                expected_instructions: vec![
                    make(Opcode::Constant, vec![0]),
                    make(Opcode::SetGlobal, vec![0]),
                    make(Opcode::Closure, vec![6, 0]),
                    make(Opcode::Pop, vec![]),
                ],
            },
        ];
        run_compiler_tests(tests);
    }

    #[test]
    fn test_compiler_scopes() {
        let mut compiler = Compiler::new();
        assert_eq!(compiler.scope_index, 0, "wrong scope index, expected {} but got {}", 0, compiler.scope_index);
        let global_table = Rc::clone(&compiler.symbol_table);

        compiler.emit(Opcode::Mul, vec![]);

        compiler.enter_scope();
        assert_eq!(compiler.scope_index, 1, "wrong scope index, expected {} but got {}", 1, compiler.scope_index);

        compiler.emit(Opcode::Sub, vec![]);
        assert_eq!(compiler.scopes[compiler.scope_index].instructions, vec![Opcode::Sub as u8], 
            "wrong instructions, expected {:?} but got {:?}", vec![Opcode::Sub as u8], compiler.scopes[compiler.scope_index].instructions);

        assert!(
            Rc::ptr_eq(&compiler.symbol_table.borrow().outer.clone().expect("should have outer table"), &global_table),
            "compiler did not enclose symbol table"
        );

        compiler.leave_scope().expect("should be able to restore table");
        assert_eq!(compiler.scope_index, 0, "wrong scope index, expected {} but got {}", 0, compiler.scope_index);

        assert!(Rc::ptr_eq(&compiler.symbol_table, &global_table), "compiler did not restore symbol table");
        assert_eq!(compiler.symbol_table.borrow().outer, None, "compiler modified global symbol table incorrecly");

        compiler.emit(Opcode::Add, vec![]);
        let last_instruction = compiler.scopes[compiler.scope_index].last_instruction.clone().expect("should have last instruction").opcode;
        let previous_instruction = compiler.scopes[compiler.scope_index].previous_instruction.clone().expect("should have previous instruction").opcode;
        assert_eq!(last_instruction, Opcode::Add, "wrong last instruction, expected {:?} but got {:?}", Opcode::Add, last_instruction);
        assert_eq!(previous_instruction, Opcode::Mul, "wrong previous instruction, expected {:?} but got {:?}", Opcode::Mul, previous_instruction);
    }
}
