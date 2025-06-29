use std::convert::TryInto;
use std::collections::HashMap;

use crate::{builtins::BUILTINS, code::Opcode, compiler::Bytecode, frame::Frame, object::Object};

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 10;
const MAX_FRAMES: usize = 1024;

pub struct Vm {
    constants: Vec<Object>,

    stack: Vec<Option<Object>>, // TODO is this gonna haunt me?
    sp: usize, // top of the stack is stac[sp - 1]

    globals: Vec<Option<Object>>,

    frames: Vec<Option<Frame>>,
    frame_index: usize,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            constants: vec![],

            stack: vec![None; STACK_SIZE],
            sp: 0,

            globals: vec![None; GLOBALS_SIZE],

            frames: vec![],
            frame_index: 0,
        }
    }

    pub fn reset(&mut self, bytecode: Bytecode) -> Vm {
        let main_function = Object::CompiledFunction { 
            function: bytecode.instructions,
            locals_count: 0,
            parameters_count: 0,
        };
        let main_closure = Object::Closure {
            function: Box::from(main_function),
            free_variables: vec![],
        };
        let main_frame = Frame::new(main_closure, 0);
        let mut frames = vec![None; MAX_FRAMES];
        frames[0] = Some(main_frame);
        Vm {
            constants: bytecode.constants,

            stack: vec![None; STACK_SIZE],
            sp: 0,

            globals: self.globals.clone(),

            frames,
            frame_index: 1,
        }
    }

    // TODO can i remove the option?
    fn current_frame(&mut self) -> &mut Option<Frame> {
        &mut self.frames[self.frame_index - 1]
    }

    fn current_frame_ref(&self) -> Frame {
        // TODO: remove this clone
        self.frames[self.frame_index - 1].clone().expect("should always have a current frame")
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.frame_index] = Some(frame);
        self.frame_index += 1;
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        self.frame_index -= 1;
        self.frames[self.frame_index].clone()
    }

    pub fn last_popped_elem(&mut self) -> Option<Object> {
        self.stack[self.sp].clone()
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
        Ok(())
    }

    fn pop(&mut self) -> Option<Object> {
        if self.sp == 0 {
            return None
        }
        let obj = self.stack[self.sp - 1].clone();
        self.sp -= 1;
        obj
    }

    fn execute_binary_integer_operation(&mut self, op: Opcode, left: i64, right: i64) -> Result<(), String> {
        let result = match op {
            Opcode::Add => left + right,
            Opcode::Sub => left - right,
            Opcode::Mul => left * right,
            Opcode::Div => left / right,
            _ => return Err(format!("ERROR: unknown integer operation {:?}", op)),
        };
        self.push(Object::Integer(result))
    }

    fn execute_binary_string_operation(&mut self, op: Opcode, left: &'static str, right: &'static str) -> Result<(), String> {
        let result = match op {
            Opcode::Add => Box::leak((left.to_string() + right).into_boxed_str()),
            _ => return Err(format!("ERROR: unknown string operation {:?}", op)),
        };
        self.push(Object::String(result))
    }

    fn execute_binary_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Some(Object::Integer(left)), Some(Object::Integer(right))) => {
                self.execute_binary_integer_operation(op, *left, *right)?;
            },
            (Some(Object::String(left)), Some(Object::String(right))) => {
                self.execute_binary_string_operation(op, left, right)?;
            },
            _ => return Err(format!("ERROR: binary operation {:?} with unsupported args {:?}, {:?}", op, left, right)),
        }
        Ok(())
    }

    fn execute_integer_comparison(&mut self, op: Opcode, left: i64, right: i64) -> Result<(), String> {
        match op {
            Opcode::Equal => self.push(Object::Boolean(left == right))?,
            Opcode::NotEqual => self.push(Object::Boolean(left != right))?,
            Opcode::GreaterThan => self.push(Object::Boolean(left > right))?,
            _ => return Err(format!("ERROR: unknown integer comparison {:?}", op)),
        }
        Ok(())
    }

    fn execute_comparison_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        let left = self.pop();
        match (&left, &right) {
            (Some(Object::Integer(left)), Some(Object::Integer(right))) => {
                self.execute_integer_comparison(op, *left, *right)?;
            },
            (Some(Object::Boolean(left)), Some(Object::Boolean(right))) => {
                match op {
                    Opcode::Equal => {
                        self.push(Object::Boolean(*left == *right))?;
                    },
                    Opcode::NotEqual => {
                        self.push(Object::Boolean(*left != *right))?;
                    },
                    _ => return Err(format!("ERROR: unknown boolean comparison {:?}", op)),
                }
            },
            _ => return Err(format!("ERROR: comparison operation {:?} with unsupported args {:?}, {:?}", op, left, right)),
        }
        Ok(())
    }

    fn execute_prefix_operation(&mut self, op: Opcode) -> Result<(), String> {
        let right = self.pop();
        match &right {
            Some(Object::Integer(right)) => {
                match op {
                    Opcode::Minus => { self.push(Object::Integer(-*right))?; },
                    Opcode::Bang => { self.push(Object::Boolean(*right == 0))?; },
                    _ => return Err(format!("ERROR: unknown integer prefix {:?}", op)),
                }
            },
            Some(Object::Boolean(right)) => {
                match op {
                    Opcode::Bang => { self.push(Object::Boolean(!right))?; }
                    _ => return Err(format!("ERROR: unknown integer prefix {:?}", op)),
                }
            },
            Some(Object::Null) => { self.push(Object::Boolean(true))?; },
            _ => return Err(format!("ERROR: comparison operation {:?} with unsupported args {:?}", op, right)),
        };
        Ok(())
    }

    fn execute_index_operation(&mut self, collection: Option<Object>, index: Option<Object>) -> Result<Object, String> {
        match collection {
            Some(Object::Array(array)) => {
                match index {
                    Some(Object::Integer(index)) => {
                        if index < 0 || index as usize >= array.len() {
                            Ok(Object::Null)
                        } else {
                            Ok(array[index as usize].clone())
                        }
                    },
                    _ => Err(format!("expected index to be integer but got {:?}", index)),
                }
            },
            Some(Object::Hash(hash)) => {
                match index {
                    Some(object) => Ok(hash.get(&object).cloned().unwrap_or(Object::Null)),
                    None => Err("could not fetch index".to_string()),
                }
            },
            _ => Err(format!("index operation not supported in {:?}", collection)),
        }
    }

    fn build_array(&mut self, start_index: usize, end_index: usize) -> Result<Object, String> {
        match self.stack[start_index..end_index]
            .iter()
            .map(|o| o.clone().ok_or_else(|| "unable to build array, got empty object from stack".to_string()))
            .collect() {
                Ok(elements) => Ok(Object::Array(elements)),
                Err(msg) => Err(msg),
            }
    }

    fn build_hash(&mut self, start_index: usize, end_index: usize) -> Result<Object, String> {
        let mut hash_map = HashMap::<Object, Object>::new();
        match self.stack[start_index..end_index]
            .iter()
            .map(|o| o.clone().ok_or_else(|| "unable to build hash, got empty object from stack".to_string()))
            .collect::<Result<Vec<Object>, String>>() {
                Ok(elements) => {
                    elements.chunks_exact(2).for_each(|chunk| {
                        if let [key, value] = chunk {
                            hash_map.insert(key.clone(), value.clone());
                        };
                    });
                    Ok(Object::Hash(hash_map))
                },
                Err(msg) => Err(msg),
            }
    }

    fn execute_call(&mut self, arguments_count: usize) -> Result<(), String> {
        match &self.stack[self.sp - 1 - arguments_count] {
            Some(Object::Closure { function: closure_function, free_variables }) => {
                // TODO remove
                match *closure_function.clone() {
                    Object::CompiledFunction { parameters_count, locals_count, .. } => {
                        if arguments_count != parameters_count {
                            return Err(format!("wrong number of arguments: expected {} but got {}", parameters_count, arguments_count));
                        }
                        let frame = Frame::new(Object::Closure { function: closure_function.clone(), free_variables: free_variables.clone() }, self.sp - arguments_count);
                        self.sp = frame.base_pointer + locals_count;
                        self.push_frame(frame);
                        Ok(())
                    }
                    _ => return Err("".to_string()),
                }
            } 
            Some(Object::BuiltinFunction(function)) => {
                let arguments = self.stack[self.sp - arguments_count..self.sp]
                    .iter()
                    .map(|o| o.clone().ok_or_else(|| "".to_string()))
                    .collect::<Result<Vec<Object>, String>>()?;
                let result = function(arguments);
                self.sp = self.sp - arguments_count - 1;
                self.push(result)?;
                Ok(())
            }
            _ => todo!()
        }
    }

    fn push_closure(&mut self, index: usize, free_count: usize) -> Result<(), String> {
        let constant = self.constants[index].clone();
        if let Object::CompiledFunction { .. } = constant {
            let free_variables = self.stack[self.sp - free_count..self.sp]
                .iter()
                .map(|o| o.clone().ok_or_else(|| "unable to build array, got empty object from stack".to_string()))
                .collect::<Result<Vec<Object>, String>>()?;

            self.sp = self.sp - free_count;
            self.push(Object::Closure { function: Box::from(constant), free_variables })?;
            return Ok(());
        }
        Err(format!("not a function {:?}", constant))
    }

    fn ip_add(&mut self, value: i64) {
        if let Some(frame) = self.current_frame() { 
            frame.ip += value; 
        }
    }

    fn ip_set(&mut self, value: i64) {
        if let Some(frame) = self.current_frame() {
            frame.ip = value;
        }
    }

    pub fn run(&mut self) -> Result<(), String> {
        while self.current_frame_ref().ip < self.current_frame_ref().instructions().len() as i64 - 1 {
            self.ip_add(1);
            let ip = self.current_frame_ref().ip as usize;
            let instructions = self.current_frame_ref().instructions();
            let op = Opcode::from(instructions[ip]);

            if let Some(op) = op {
                match op {
                    Opcode::Constant => {
                        let index = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap());
                        self.ip_add(2);
                        self.push(self.constants[index as usize].clone())?;
                    }
                    Opcode::Pop => { self.pop(); },
                    Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div => {
                        self.execute_binary_operation(op)?;
                    }
                    Opcode::True => { self.push(Object::Boolean(true))?; }
                    Opcode::False => { self.push(Object::Boolean(false))?; }
                    Opcode::GreaterThan | Opcode::Equal | Opcode::NotEqual => {
                        self.execute_comparison_operation(op)?;
                    },
                    Opcode::Minus | Opcode::Bang => {
                        self.execute_prefix_operation(op)?;
                    },
                    Opcode::Jump => {
                        let position = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap());
                        self.ip_set((position - 1) as i64);
                    },
                    Opcode::JumpNotTrue => {
                        let position = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap());
                        self.ip_add(2);
                        if let Some(condition) = self.pop() {
                            if !is_truthy(&condition) {
                                self.ip_set((position - 1) as i64);
                            }
                        }
                    }
                    Opcode::Null => { self.push(Object::Null)?; },
                    Opcode::SetGlobal => {
                        let global_index = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap());
                        self.ip_add(2);
                        self.globals[global_index as usize] = self.pop();
                    },
                    Opcode::GetGlobal => {
                        let global_index = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap());
                        self.ip_add(2);
                        if let Some(global) = self.globals[global_index as usize].clone() {
                            self.push(global)?;
                        }
                    },
                    Opcode::Array => {
                        let array_len = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap()) as usize;
                        self.ip_add(2);
                        let array = self.build_array(self.sp - array_len, self.sp)?;
                        self.sp -= array_len;
                        self.push(array)?;
                    },
                    Opcode::Hash => {
                        let elements_count = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap()) as usize;
                        self.ip_add(2);
                        let hash = self.build_hash(self.sp - elements_count, self.sp)?;
                        self.sp -= elements_count;
                        self.push(hash)?;
                    },
                    Opcode::Index => {
                        let index = self.pop();
                        let collection = self.pop();
                        match self.execute_index_operation(collection, index) {
                            Ok(object) => self.push(object)?,
                            Err(msg) => return Err(msg),
                        }
                    },
                    Opcode::Call => {
                        let arguments_count = instructions[ip + 1] as usize;
                        if let Some(frame) = self.current_frame() {
                            frame.ip += 1;
                        }
                        self.execute_call(arguments_count)?;
                    }, 
                    Opcode::Return => {
                        match self.pop_frame() {
                            Some(frame) => self.sp = frame.base_pointer - 1,
                            None => return Err("attempting to return from no function call".to_string()),
                        };
                        self.push(Object::Null)?;
                    },
                    Opcode::ReturnValue => {
                        let return_value = self.pop();
                        match self.pop_frame() {
                            Some(frame) => self.sp = frame.base_pointer - 1,
                            None => return Err("attempting to return from no function call".to_string()),
                        };
                        if let Some(value) = return_value {
                            self.push(value)?;
                        }
                    },
                    Opcode::SetLocal => {
                        let index = instructions[ip + 1] as usize;
                        match self.current_frame() {
                            Some(frame) => {
                                frame.ip += 1;
                                let stack_index = frame.base_pointer + index;
                                self.stack[stack_index] = self.pop();
                            }
                            None => return Err("vm has no current frame".to_string()),
                        }
                    },
                    Opcode::GetLocal => {
                        match self.current_frame() {
                            Some(frame) => {
                                let index = instructions[ip + 1] as usize;
                                frame.ip += 1;
                                let stack_index = frame.base_pointer + index;
                                match &self.stack[stack_index] {
                                    Some(object) => self.push(object.to_owned())?,
                                    None => return Err("could not find local binding to push to the stack".to_string()),
                                }
                            }
                            None => return Err("vm has no current frame".to_string()),
                        }
                    },
                    Opcode::GetBuiltin => {
                        match self.current_frame() {
                            Some(frame) => {
                                let index = instructions[ip + 1] as usize;
                                frame.ip += 1;
                                let definition = BUILTINS[index].clone();
                                self.push(definition.1)?;
                            },
                            None => return Err("vm has no current frame".to_string()),
                        }
                    },
                    Opcode::Closure => {
                        match self.current_frame() {
                            Some(frame) => {
                                let index = u16::from_be_bytes(instructions[ip+1..ip+3].try_into().unwrap()) as usize;
                                let free_count = instructions[ip + 3] as usize;
                                frame.ip += 3;
                                self.push_closure(index, free_count)?;
                            },
                            None => return Err("vm has no current frame".to_string())
                        }
                    },
                    Opcode::GetFree => {
                        let object = match self.current_frame() {
                            Some(frame) => {
                                let index = instructions[ip + 1] as usize;
                                frame.ip += 1;

                                match &frame.closure {
                                    Object::Closure { free_variables, .. } => free_variables[index].clone(),
                                    _ => return Err("vm has no current frame".to_string())
                                }
                            },
                            None => return Err("vm has no current frame".to_string())
                        };
                        self.push(object)?;
                    },
                };
            } else {
                return Err(format!("Opcode for {} not found", instructions[ip]));
            }
        }
        Ok(())
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

fn is_truthy(object: &Object) -> bool {
    !matches!(object, Object::Boolean(false) | Object::Integer(0) | Object::Null)
}

#[cfg(test)]
mod vm_tests {
    use std::collections::HashMap;

    use super::*;
    use crate::{ast::Program, compiler::Compiler, lexer::Lexer, parser::Parser};
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
            let mut vm = Vm::new();
            let mut vm = vm.reset(compiler.bytecode());
            if let Err(msg) = vm.run() {
                panic!("vm error {}", msg);
            }
            let expected = Some(test.expected);
            let stack_element = vm.last_popped_elem();
            assert_eq!(expected, stack_element, "expected {:?} to equal {:?} in test {}", stack_element, expected, test.input);
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
            VmTestCase { input: "-1", expected: Object::Integer(-1) },
            VmTestCase { input: "-1", expected: Object::Integer(-1) },
            VmTestCase { input: "-5", expected: Object::Integer(-5) },
            VmTestCase { input: "-10", expected: Object::Integer(-10) },
            VmTestCase { input: "-50 + 100 + -50", expected: Object::Integer(0) },
            VmTestCase { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: Object::Integer(50) },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = vec![
            VmTestCase { input: "true", expected: Object::Boolean(true) },
            VmTestCase { input: "false", expected: Object::Boolean(false) },
            VmTestCase { input: "1 < 2", expected: Object::Boolean(true) },
            VmTestCase { input: "1 > 2", expected: Object::Boolean(false) },
            VmTestCase { input: "1 < 1", expected: Object::Boolean(false) },
            VmTestCase { input: "1 > 1", expected: Object::Boolean(false) },
            VmTestCase { input: "1 == 1", expected: Object::Boolean(true) },
            VmTestCase { input: "1 != 1", expected: Object::Boolean(false) },
            VmTestCase { input: "1 == 2", expected: Object::Boolean(false) },
            VmTestCase { input: "1 != 2", expected: Object::Boolean(true) },
            VmTestCase { input: "true == true", expected: Object::Boolean(true) },
            VmTestCase { input: "false == false", expected: Object::Boolean(true) },
            VmTestCase { input: "true == false", expected: Object::Boolean(false) },
            VmTestCase { input: "true != false", expected: Object::Boolean(true) },
            VmTestCase { input: "false != true", expected: Object::Boolean(true) },
            VmTestCase { input: "(1 < 2) == true", expected: Object::Boolean(true) },
            VmTestCase { input: "(1 < 2) == false", expected: Object::Boolean(false) },
            VmTestCase { input: "(1 > 2) == true", expected: Object::Boolean(false) },
            VmTestCase { input: "(1 > 2) == false", expected: Object::Boolean(true) },
            VmTestCase { input: "!true", expected: Object::Boolean(false) },
            VmTestCase { input: "!false", expected: Object::Boolean(true) },
            VmTestCase { input: "!5", expected: Object::Boolean(false) },
            VmTestCase { input: "!!true", expected: Object::Boolean(true) },
            VmTestCase { input: "!!false", expected: Object::Boolean(false) },
            VmTestCase { input: "!!5", expected: Object::Boolean(true) },
            VmTestCase { input: "!(if (false) { 5 })", expected: Object::Boolean(true) },
            VmTestCase { input: "if ((if (false) { 10 })) { 10 } else { 20 }", expected: Object::Integer(20) },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_conditional_expressions() {
        let tests = vec![
            VmTestCase { input: "if (true) { 10 }", expected: Object::Integer(10) },
            VmTestCase { input: "if (true) { 10 } else { 20 }", expected: Object::Integer(10) },
            VmTestCase { input: "if (false) { 10 } else { 20 } ", expected: Object::Integer(20) },
            VmTestCase { input: "if (1) { 10 }", expected: Object::Integer(10) },
            VmTestCase { input: "if (1 < 2) { 10 }", expected: Object::Integer(10) },
            VmTestCase { input: "if (1 < 2) { 10 } else { 20 }", expected: Object::Integer(10) },
            VmTestCase { input: "if (1 > 2) { 10 } else { 20 }", expected: Object::Integer(20) },
            VmTestCase { input: "if (1 > 2) { 10 }", expected: Object::Null },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_let_global_statements() {
        let tests = vec![
            VmTestCase { input: "let one = 1; one", expected: Object::Integer(1) },
            VmTestCase { input: "let one = 1; let two = 2; one + two;", expected: Object::Integer(3) },
            VmTestCase { input: "let one = 1; let two = one + one; one + two;", expected: Object::Integer(3) },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_string_expressions() {
        let tests = vec![
            VmTestCase { input: "\"monkey\"", expected: Object::String("monkey") },
            VmTestCase { input: "\"mon\" + \"key\"", expected: Object::String("monkey") },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_array_literals() {
        let tests = vec![
            VmTestCase { input: "[]", expected: Object::Array(vec![]) },
            VmTestCase { input: "[1, 2, 3]", expected: Object::Array(vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)]) },
            VmTestCase { input: "[1 + 2, 3 * 4, 6 - 5]", expected: Object::Array(vec![Object::Integer(3), Object::Integer(12), Object::Integer(1)]) },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_hash_literals() {
        let tests = vec![
            VmTestCase { input: "{}", expected: Object::Hash(HashMap::new()) },
            VmTestCase { 
                input: "{1: 2, 2: 3}",
                expected: Object::Hash(
                    HashMap::from([
                        (Object::Integer(1), Object::Integer(2)),
                        (Object::Integer(2), Object::Integer(3)),
                    ])
                ),
            },
            VmTestCase { 
                input: "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                expected: Object::Hash(
                    HashMap::from([
                        (Object::Integer(2), Object::Integer(4)),
                        (Object::Integer(6), Object::Integer(16)),
                    ])
                ),
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_index_expressions() {
        let tests = vec![
            VmTestCase { input: "[1, 2, 3][1]", expected: Object::Integer(2) },
            VmTestCase { input: "[1, 2, 3][0 + 2]", expected: Object::Integer(3) },
            VmTestCase { input: "[[1, 1, 1]][0][0]", expected: Object::Integer(1) },
            VmTestCase { input: "[][0]", expected: Object::Null },
            VmTestCase { input: "[1, 2, 3][99]", expected: Object::Null },
            VmTestCase { input: "[1][-1]", expected: Object::Null },
            VmTestCase { input: "{1: 1, 2: 2}[1]", expected: Object::Integer(1) },
            VmTestCase { input: "{1: 1, 2: 2}[2]", expected: Object::Integer(2) },
            VmTestCase { input: "{1: 1}[0]", expected: Object::Null },
            VmTestCase { input: "{}[0]", expected: Object::Null },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_function_call_without_arguments() {
        let tests = vec![
            VmTestCase { input: "let fivePlusTen = fn() { 5 + 10; }; fivePlusTen()", expected: Object::Integer(15) },
            VmTestCase { 
                input: "let one = fn() { 1 }; let two = fn() { 2; }; one() + two()",
                expected: Object::Integer(3) 
            },
            VmTestCase { 
                input: "let a = fn() { 1 }; let b = fn() { a() + 1 }; let c = fn() { b() + 1 }; c()",
                expected: Object::Integer(3) 
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_function_with_return_statement() {
        let tests = vec![
            VmTestCase { input: "let early = fn() { return 99; 100 }; early()", expected: Object::Integer(99) },
            VmTestCase { input: "let early = fn() { return 99; return 100 }; early()", expected: Object::Integer(99) },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_function_without_return_value() {
        let tests = vec![
            VmTestCase { input: "let noReturn = fn() { } noReturn()", expected: Object::Null },
            VmTestCase { 
                input: "let noReturn = fn() { }; let noReturnTwo = fn() { noReturn() } noReturn() noReturnTwo()",
                expected: Object::Null 
            },
        ];
        run_vm_tests(tests);
    }
    
    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = vec![
            VmTestCase { 
                input: "let one = fn() { let one = 1; one }; one()",
                expected: Object::Integer(1),
            },
            VmTestCase { 
                input: "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; }; oneAndTwo()",
                expected: Object::Integer(3),
            },
            VmTestCase { 
                input: "
                    let oneAndTwo = fn() {
                        let one = 1;
                        let two = 2;
                        one + two;
                    };
                    let threeAndFour = fn() {
                        let three = 3;
                        let four = 4;
                        three + four;
                    };
                    oneAndTwo() + threeAndFour();
                ",
                expected: Object::Integer(10),
            },
            VmTestCase { 
                input: "
                    let firstFoobar = fn() { 
                        let foobar = 50;
                        foobar;
                    }; 
                    let secondFoobar = fn() { 
                        let foobar = 100; 
                        foobar; 
                    }; 
                    firstFoobar() + secondFoobar();
                ",
                expected: Object::Integer(150),
            },
            VmTestCase { 
                input: "
                    let globalSeed = 50;
                    let minusOne = fn() { 
                        let num = 1; 
                        globalSeed - num; 
                    } 
                    let minusTwo = fn() { 
                        let num = 2; 
                        globalSeed - num;
                    } 
                    minusOne() + minusTwo();
                ",
                expected: Object::Integer(97),
            },
            VmTestCase {
                input: "
                    let returnsOneReturner = fn() {
                        let returnsOne = fn() { 1; };
                        returnsOne;
                    }
                    returnsOneReturner()()
                ",
                expected: Object::Integer(1),
            }
        ];
        run_vm_tests(tests);
    }
    
    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = vec![
            VmTestCase { 
                input: "let identify = fn(a: Integer) { a }; identify(4);",
                expected: Object::Integer(4),
            },
            VmTestCase { 
                input: "let sum = fn(a: Integer, b: Integer) { let c = a + b; c; }; sum(1, 2);",
                expected: Object::Integer(3),
            },
            VmTestCase {
                input: "let sum = fn(a: Integer, b: Integer) { let c = a + b; c; } sum(1, 2) + sum(3, 4)",
                expected: Object::Integer(10),
            },
            VmTestCase {
                input: "
                    let sum = fn(a: Integer, b: Integer) { let c = a + b; c; }
                    let outer = fn() { sum(1, 2) + sum(3, 4); }
                    outer();
                ",
                expected: Object::Integer(10),
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            VmTestCase { 
                input: "len(\"\")",
                expected: Object::Integer(0),
            },
            VmTestCase { 
                input: "len(\"four\")",
                expected: Object::Integer(4),
            },
            VmTestCase { 
                input: "len([1, 2, 3])",
                expected: Object::Integer(3),
            },
            VmTestCase { 
                input: "len([])",
                expected: Object::Integer(0),
            },
            VmTestCase { 
                input: "print(\"aa\")",
                expected: Object::Null,
            },
            VmTestCase {
                input: "first([])",
                expected: Object::Null,
            },
            VmTestCase {
                input: "first([1, 2, 3])",
                expected: Object::Integer(1),
            },
            VmTestCase {
                input: "push([], 1)",
                expected: Object::Array(vec![Object::Integer(1)]),
            },
            VmTestCase {
                input: "rest([1, 2, 3])",
                expected: Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_closures() {
        let tests = vec![
            VmTestCase { 
                input: "let newClosure = fn(a: Integer) { fn() { a } }; let closure = newClosure(99); closure();",
                expected: Object::Integer(99),
            },
            VmTestCase { 
                input: "
                    let newAdder = fn(a: Integer, b: Integer) {
                        fn(c: Integer) { a + b + c };
                    };
                    let adder = newAdder(1, 2);
                    adder(8) 
                ",
                expected: Object::Integer(11),
            },
            VmTestCase { 
                input: "
                    let newAdder = fn(a: Integer, b: Integer) {
                        fn(c: Integer) { a + b + c; }
                    };
                    let adder = newAdder(1, 2);
                    adder(8);
                ",
                expected: Object::Integer(11),
            },
            VmTestCase { 
                input: "
                    let newClosure = fn(a: Integer, b: Integer) {
                        let one = fn() { a }
                        let two = fn() { b }
                        fn() { one() + two() }
                    }
                    let closure = newClosure(9, 90);
                    closure();
                ",
                expected: Object::Integer(99),
            },
        ];
        run_vm_tests(tests);
    }

    #[test]
    fn test_calling_functions_with_wrong_arguments() {
        // TODO for crying out loud this should be a compiler error!!!!
        let tests: Vec<(_, Result<(), _>)> = vec![
            ("fn() { 1; }(1)", Err("wrong number of arguments: expected 0 but got 1".to_string())),
            ("fn(a: Integer) { }()", Err("wrong number of arguments: expected 1 but got 0".to_string())),
            ("fn(a: Integer, b: Integer) { a + b; }(1)", Err("wrong number of arguments: expected 2 but got 1".to_string())),
        ];
        for (input, expected) in tests {
            let mut compiler = Compiler::new();
            let program = parse(input);
            if let Err(msg) = compiler.compile(program) {
                panic!("compiler error {}", msg);
            }
            let mut vm = Vm::new();
            let mut vm = vm.reset(compiler.bytecode());
            let result = vm.run();
            assert_eq!(expected, result, "expected {:?} to equal {:?} in test {}", expected, result, input);
        }
    }
}
