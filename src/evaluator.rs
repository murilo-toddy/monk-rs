use std::collections::HashMap;

use crate::{object::Object, ast::{Program, Statement, Expression, BlockStatement}, environment::Environment, builtins::BuiltinFunctions};

pub struct Evaluator {
    env: Environment,
    builtin: BuiltinFunctions,
}

impl Evaluator {
    pub fn new(env: Environment) -> Evaluator {
        Evaluator { 
            env,
            builtin: BuiltinFunctions::new(),
        }
    }

    fn is_error(&self, object: &Object) -> bool {
        matches!(object, Object::Error(_))
    }

    fn is_truthy(&self, object: &Object) -> bool {
        !matches!(object, Object::Boolean(false) | Object::Integer(0) | Object::Null)
    }

    fn evaluate_prefix_expression(&mut self, operator: String, right: Expression) -> Object {
        let right_eval = self.evaluate_expression(right);
        if self.is_error(&right_eval) {
            return right_eval;
        }
        match operator.as_str() {
            "!" => {
                match right_eval {
                    Object::Boolean(value) => return Object::Boolean(!value),
                    Object::Integer(value) => return Object::Boolean(value == 0),
                    Object::Null => return Object::Boolean(true),
                    _ => {}
                }
            }
            "-" => {
                match right_eval {
                    Object::Integer(value) => return Object::Integer(-value),
                    _ => {}
                }
            }
            _ => {}
        }
        Object::Error(format!("unknown operation: {}{}", operator, right_eval.inspect()))
    }

    fn evaluate_reassign_expression(&mut self, left: Expression, right: Expression) -> Object {
        match left {
            Expression::Identifier { value, .. } => {
                let right_eval = self.evaluate_expression(right);
                if self.is_error(&right_eval) {
                    return right_eval;
                }
                if self.env.get(&value).is_some() {
                    self.env.set(value.to_owned(), right_eval.clone());
                    return right_eval;
                } else {
                    return Object::Error(format!("identifier {} not found", value));
                }
            },
            Expression::Index { left, index, .. } => {
                if let Expression::Identifier { value, .. } = *left.clone() {
                    let left_eval = self.evaluate_expression(*left);
                    if self.is_error(&left_eval) {
                        return left_eval;
                    }
                    let index_eval = self.evaluate_expression(*index);
                    if self.is_error(&index_eval) {
                        return index_eval;
                    }

                    let right_eval = self.evaluate_expression(right);
                    if self.is_error(&right_eval) {
                        return right_eval;
                    }

                    match left_eval {
                        Object::Array(mut arr) => {
                            if let Object::Integer(idx) = index_eval {
                                if arr.len() == 0 || idx < 0 || idx > (arr.len() as i64) - 1 {
                                    return Object::Error(format!("index {} out of bounds for array [{}]", 
                                        idx, arr.iter().map(|e| e.inspect()).collect::<Vec<String>>().join(", ")));
                                }
                                arr[idx as usize] = right_eval.clone();
                                self.env.set(value, Object::Array(arr.to_owned()).to_owned());
                                return right_eval;
                            }
                            return Object::Error(format!("expected index to be integer but got {}", index_eval.inspect()));
                        },
                        Object::Hash(mut hash) => {
                            hash.insert(index_eval, right_eval.clone()); 
                            self.env.set(value, Object::Hash(hash));
                            return right_eval;
                        },
                        _ => return Object::Error(format!("cannot reassign object {}", left_eval.inspect())),
                    }
                } 
            }
            _ => {}
        }
        Object::Null
    }
        
    fn evaluate_infix_expression(
        &mut self,
        operator: String, 
        left: Expression,
        right: Expression,
    ) -> Object {
        if operator.as_str() == "=" {
            return self.evaluate_reassign_expression(left, right);
        }

        let left_eval = self.evaluate_expression(left);
        if self.is_error(&left_eval) {
            return left_eval;
        }
        let right_eval = self.evaluate_expression(right);
        if self.is_error(&right_eval) {
            return right_eval;
        }

        match (&left_eval, &right_eval) {
            (Object::Integer(left_val), Object::Integer(right_val)) => {
                match operator.as_str() {
                    "+" => Object::Integer(left_val + right_val),
                    "-" => Object::Integer(left_val - right_val),
                    "*" => Object::Integer(left_val * right_val),
                    "/" => Object::Integer(left_val / right_val),
                    "==" => Object::Boolean(left_val == right_val),
                    "!=" => Object::Boolean(left_val != right_val),
                    "&" => Object::Integer(left_val & right_val),
                    "|" => Object::Integer(left_val | right_val),
                    "^" => Object::Integer(left_val ^ right_val),
                    "<<" => Object::Integer(left_val << right_val),
                    ">>" => Object::Integer(left_val >> right_val),
                    "&&" => Object::Boolean(*left_val != 0 && *right_val != 0),
                    "||" => Object::Boolean(*left_val != 0 || *right_val != 0),
                    ">=" => Object::Boolean(left_val >= right_val),
                    "<=" => Object::Boolean(left_val <= right_val),
                    ">" => Object::Boolean(left_val > right_val),
                    "<" => Object::Boolean(left_val < right_val),
                    _ => Object::Error(format!("unknown operation: {} {} {}", left_val, operator, right_val))
                }
            },
            (Object::Boolean(left_val), Object::Boolean(right_val)) => {
                match operator.as_str() {
                    "==" => Object::Boolean(left_val == right_val),
                    "!=" => Object::Boolean(left_val != right_val),
                    "&&" => Object::Boolean(*left_val && *right_val),
                    "||" => Object::Boolean(*left_val || *right_val),
                    _ => Object::Error(format!("unknown operation: {} {} {}", left_val, operator, right_val))
                }
            }
            (Object::String(left_val), Object::String(right_val)) => {
                match operator.as_str() {
                    "+" => Object::String(left_val.to_owned() + right_val),
                    "==" => Object::Boolean(left_val == right_val),
                    "!=" => Object::Boolean(left_val != right_val),
                    _ => Object::Error(format!("unknown operation: string {} string", operator))
                }
            }
            _ => {
                if std::mem::discriminant(&left_eval) != std::mem::discriminant(&right_eval) {
                    Object::Error(format!("type mismatch: {} {} {}", left_eval.inspect(), operator, right_eval.inspect()))
                } else {
                    Object::Error(format!("unknown operation: {} {} {}", left_eval.inspect(), operator, right_eval.inspect()))
                }
            }
        }
    }

    fn evaluate_block_statement(&mut self, block: BlockStatement) -> Object {
        let mut result = None;
        for statement in block.statements {
            result = self.evaluate_statement(statement);
            match result {
                Some(Object::ReturnValue(_)) | Some(Object::Error(_)) => {
                    return result.unwrap();
                }
                _ => {}
            }
        }
        result.unwrap_or(Object::Null)
    }

    fn evaluate_conditional_expression(
        &mut self,
        conditions: Vec<(Expression, BlockStatement)>,
        alternative: Option<BlockStatement>,
    ) -> Object {
        for (condition, consequence) in conditions {
            let condition_eval = self.evaluate_expression(condition);
            if self.is_error(&condition_eval) {
                return condition_eval;
            }
            if self.is_truthy(&condition_eval) {
                return self.evaluate_block_statement(consequence);
            }
        }
        alternative.map_or(Object::Null, |a| self.evaluate_block_statement(a))
    }

    fn evaluate_while_expression(&mut self, condition: Expression, block_statement: BlockStatement) -> Object {
        let mut result: Option<Object> = None;
        loop {
            let condition_eval = self.evaluate_expression(condition.clone());
            if self.is_error(&condition_eval) {
                return condition_eval;
            }
            if !self.is_truthy(&condition_eval) {
                break
            }
            result = Some(self.evaluate_block_statement(block_statement.clone()));
        }
        result.unwrap_or(Object::Null)
    }

    fn evaluate_for_expression(
        &mut self,
        declaration: Statement,
        condition: Expression,
        operation: Expression,
        block_statement: BlockStatement
    ) -> Object {
        let mut result = None;
        self.evaluate_statement(declaration);
        loop {
            let condition_eval = self.evaluate_expression(condition.clone());
            if self.is_error(&condition_eval) {
                return condition_eval;
            }
            if !self.is_truthy(&condition_eval) {
                break
            }
            result = Some(self.evaluate_block_statement(block_statement.clone()));
            self.evaluate_expression(operation.clone());
        }
        
        result.unwrap_or(Object::Null)
    }

    fn evaluate_expressions(&mut self, expressions: Vec<Expression>) -> Vec<Object> {
        let mut result = vec![];
        for expression in expressions {
            let evaluated = self.evaluate_expression(expression);
            if self.is_error(&evaluated) {
                return vec![evaluated];
            }
            result.push(evaluated);
        }
        result
    }

    fn evaluate_index_expression(&mut self, left: Object, index: Object) -> Object {
        match left {
            Object::Array(elements) => {
                if let Object::Integer(index) = index {
                    if elements.len() == 0 || index < 0 || index > elements.len() as i64 - 1 {
                        return Object::Null;
                    }
                    return elements[index as usize].clone();
                }
            },
            Object::Hash(elements) => {
                return elements.get(&index).map(|v| v.to_owned()).unwrap_or(Object::Null);
            },
            _ => {}
        }
        Object::Error("index operator not supported".to_owned())
    }

    fn evaluate_hash_literal(&mut self, pairs: Vec<(Expression, Expression)>) -> Object {
        let mut hash_map = HashMap::new();
        for (key, value) in pairs {
            let eval_key = self.evaluate_expression(key);
            if self.is_error(&eval_key) {
                return eval_key;
            }
            let eval_value = self.evaluate_expression(value);
            if self.is_error(&eval_value) {
                return eval_value;
            }
            hash_map.insert(eval_key, eval_value);
        }
        Object::Hash(hash_map)
    }

    fn evaluate_expression(&mut self, expression: Expression) -> Object {
        match expression {
            Expression::Identifier { value, .. } => {
                self.env.get(&value)
                    .or_else(|| self.builtin.get_function_object(&value))
                    .unwrap_or(Object::Error(format!("identifier not found: {}", value)))
            },
            Expression::Integer { value, .. } => Object::Integer(value),
            Expression::String { value, .. } => Object::String(value),
            Expression::Boolean { value, .. } => Object::Boolean(value),
            Expression::Prefix { operator, right, .. } => {
                self.evaluate_prefix_expression(operator, *right)
            },
            Expression::Infix { operator, left, right, .. } => {
                self.evaluate_infix_expression(operator, *left, *right)
            },
            Expression::If { conditions, alternative, .. } => {
                self.evaluate_conditional_expression(conditions, alternative)
            },
            Expression::Function { arguments, body, .. } => {
                Object::Function(arguments, body, self.env.clone())
            },
            Expression::While { condition, statement, .. } => {
                self.evaluate_while_expression(*condition, statement)
            },
            Expression::For { declaration, condition, operation, statement, .. } => {
                self.evaluate_for_expression(*declaration, *condition, *operation, statement)
            },
            Expression::Call { function, arguments, .. } => {
                let func = self.evaluate_expression(*function);
                if self.is_error(&func) {
                    return func;
                } 
                let args = self.evaluate_expressions(arguments);
                if let Some(Object::Error(_)) = args.first() {
                    return args[0].to_owned();
                }
                self.apply_function(func, args)
            }
            Expression::Array { elements, .. } => {
                let elements = self.evaluate_expressions(elements);
                if elements.len() == 1 && self.is_error(&elements[0]) {
                    return elements[0].to_owned();
                }
                Object::Array(elements)
            },
            Expression::Hash { pairs, .. } => self.evaluate_hash_literal(pairs),
            Expression::Index { left, index, .. } => {
                let left_exp = self.evaluate_expression(*left);
                if self.is_error(&left_exp) {
                    return left_exp;
                }
                let index_exp = self.evaluate_expression(*index);
                if self.is_error(&index_exp) {
                    return index_exp;
                }
                self.evaluate_index_expression(left_exp, index_exp)
            },
        }
    }

    fn apply_function(&mut self, func: Object, args: Vec<Object>) -> Object {
        match func.clone() {
            Object::Function(_, body, _) => {
                let extended_env = self.extend_function_environment(func, args);
                let mut extended_evaluator = Evaluator::new(extended_env);
                let evaluated = extended_evaluator.evaluate_block_statement(body.to_owned());
                match evaluated {
                    Object::ReturnValue(value) => *value.to_owned(),
                    _ => evaluated,
                }
            },
            Object::BuiltinFunction(func) => func(args),
            _ => Object::Error("expected function".to_owned())
        }
    }

    fn extend_function_environment(&mut self, func: Object, args: Vec<Object>) -> Environment {
        if let Object::Function(arguments, _, environment) = func {
            let mut env = Environment::new_enclosed(environment);
            for (i, arg) in arguments.iter().enumerate() {
                env.set(arg.value.clone(), args[i].clone());
            }
            return env;
        }
        panic!("branch cannot happen")
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Option<Object> {
        match statement {
            Statement::Let { name, value, .. } => {
                let value_eval = value.map(|v| self.evaluate_expression(v))?;
                if self.is_error(&value_eval) {
                    return Some(value_eval);
                }
                self.env.set(name.value, value_eval.clone());
                Some(value_eval)
            },
            Statement::Return { value, .. } => {
                let value_eval = value.map(|v| self.evaluate_expression(v))?;
                Some(Object::ReturnValue(Box::new(value_eval)))
            },
            Statement::Expression { expression, .. } => expression.map(|v|  self.evaluate_expression(v)),
        }
    }

    pub fn evaluate_program(&mut self, program: Program) -> Option<Object> {
        let mut result = None;
        for statement in program.0 {
            result = self.evaluate_statement(statement);
            match result {
                Some(Object::ReturnValue(result)) => return Some(*result),
                Some(Object::Error(_)) => return result,
                _ => {}
            }
        }
        result
    }
}


#[cfg(test)]
mod evaluator_tests {
    use std::collections::HashMap;

    use crate::{lexer::Lexer, parser::Parser, ast::Identifier, token::Token};

    use super::*;

    fn eval_input(input: &str) -> Object {
        let env = Environment::new();
        let mut evaluator = Evaluator::new(env);
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        assert_eq!(parser.get_errors().len(), 0);
        return evaluator.evaluate_program(program).unwrap();
    }

    #[test]
    fn test_integer_expression_eval() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("5 + 5", Object::Integer(10)),
            ("5 - 5", Object::Integer(0)),
            ("5 * 5", Object::Integer(25)),
            ("5 / 5", Object::Integer(1)),
            ("3 & 1", Object::Integer(1)),
            ("2 | 1", Object::Integer(3)),
            ("3 ^ 5", Object::Integer(6)),
            ("3 << 1", Object::Integer(6)),
            ("2 >> 1", Object::Integer(1)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{}", input);
        }
    }

    #[test]
    fn test_string_expression_eval() {
        let tests = vec![
            ("\"Hello World!\"", Object::String("Hello World!".to_string())),
            ("\"Hello\" + \" \" + \"World!\"", Object::String("Hello World!".to_string())),
            ("\"a\" == \"a\"", Object::Boolean(true)),
            ("\"a\" == \"aa\"", Object::Boolean(false)),
            ("\"a\" == \"b\"", Object::Boolean(false)),
            ("\"a\" != \"b\"", Object::Boolean(true)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_boolean_expression_eval() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 >= 1) || false", Object::Boolean(true)),
            ("(1 >= 1) && false", Object::Boolean(false)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
            ("1 < 2", Object::Boolean(true)),
            ("1 <= 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 >= 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 <= 1", Object::Boolean(true)),
            ("1 > 1", Object::Boolean(false)),
            ("1 >= 1", Object::Boolean(true)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_prefix_operator_eval() {
        let tests = vec![
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!5", Object::Boolean(false)),
            ("!0", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
            ("!!0", Object::Boolean(false)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_conditional_expression_eval() {
        let tests = vec![
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (false) { 10 }", Object::Null),
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (0) { 10 }", Object::Null),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else if (1 == 2) { 20 } else { 30 }", Object::Integer(30)),
            ("if (1 > 2) { 10 } else if (1 < 2) { 20 } else { 30 }", Object::Integer(20)),
            ("if (1 > 2) { 10 } else if (1 == 2) { 20 }", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{}", input);
        }
    }

    #[test]
    fn test_while_expression_eval() {
        let tests = vec![
            ("let i = 0; while (i < 2) { let i = i + 1; }", Object::Integer(2)),
            ("let i = 0; while (i < 25) { let i = i + 10; }", Object::Integer(30)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_for_expression_eval() {
        let tests = vec![
            ("let x = 0; for (let i = 0; i < 11; i = i + 1) { let x = x + i; }; x;", Object::Integer(55)),
            ("for (let i = 0; i < 11; i = i + 1) { i; };", Object::Integer(10)),
            ("for (let i = 10; i < 0; i = i + 1) { i; };", Object::Null),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_return_statement_eval() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) {
                    if (10 > 1) {
                        return 10;
                    }
                    return 1;
                }", 
                Object::Integer(10)
            ),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }


    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                Object::Error("type mismatch: 5 + true".to_owned()),
            ),
            (
                "5 + true; 5;",
                Object::Error("type mismatch: 5 + true".to_owned()),
            ),
            (
                "-true",
                Object::Error("unknown operation: -true".to_owned()),
            ),
            (
                "true + false;",
                Object::Error("unknown operation: true + false".to_owned()),
            ),
            (
                "5; true + false; 5",
                Object::Error("unknown operation: true + false".to_owned()),
            ),
            (
                "if (10 > 1) { true + false; }",
                Object::Error("unknown operation: true + false".to_owned()),
            ),
            (
                "
                    if (10 > 1) {
                        if (10 > 1) {
                            return true + false;
                        }
                        return 1;
                    }
                ",
                Object::Error("unknown operation: true + false".to_owned()),
            ),
            (
                "foobar",
                Object::Error("identifier not found: foobar".to_owned()),
            ),
            (
                "\"hello\" - \"world\"",
                Object::Error("unknown operation: string - string".to_owned()),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", Object::Integer(15)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    fn identifier(value: &str) -> Identifier {
        Identifier {
            token: Token::Identifier(value.to_owned()),
            value: value.to_owned(),
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = eval_input(input);
        match evaluated {
            Object::Function(params, block, ..) => {
                assert_eq!(params, vec![identifier("x")]);
                assert_eq!(format!("{}", block), "(x + 2)");
            },
            _ => panic!("expected function object")
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", Object::Integer(5)),
            ("let identity = fn(x) { return x; }; identity(5);", Object::Integer(5)),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Integer(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Integer(10)),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", Object::Integer(20)),
            ("fn(x) { x; }(5)", Object::Integer(5)),
            (    
                "
                    let newAdder = fn(x) {
                        fn(y) { x + y; };
                    }
                    let addTwo = newAdder(2);
                    addTwo(2);
                ",
                Object::Integer(4),
            ),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_builtin_function() {
        let tests = [
            ("len(\"\")", Object::Integer(0)),
            ("len(\"four\")", Object::Integer(4)),
            ("len(1)", Object::Error("argument 1 not supported by `len`".to_owned())),
            ("len(\"one\", \"two\")", Object::Error("wrong number of arguments on function `len`. got=2, want=1".to_owned())),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_array_literals() {
        let tests = [
            ("[1, 2 * 2, 3 + 3]", Object::Array(vec![Object::Integer(1), Object::Integer(4), Object::Integer(6)])),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_hash_literals() {
        let tests = [
            (
                "let two = \"two\";
                 {
                    \"one\": 10-9,
                    two: 1 + 1,
                    \"thr\" + \"ee\": 6 / 2,
                    4: 4,
                    true: 5,
                    false: 6
                 }",
                Object::Hash(HashMap::from([
                    (Object::String("one".to_owned()), Object::Integer(1)), 
                    (Object::String("two".to_owned()), Object::Integer(2)), 
                    (Object::String("three".to_owned()), Object::Integer(3)), 
                    (Object::Integer(4), Object::Integer(4)), 
                    (Object::Boolean(true), Object::Integer(5)), 
                    (Object::Boolean(false), Object::Integer(6)), 
                ]))
            ),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_index_expression() {
        let tests = [
            ("[1, 2, 3][0]", Object::Integer(1)),
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][2]", Object::Integer(3)),
            ("let i = 0; [1][i];", Object::Integer(1)),
            ("[1, 2, 3][1 + 1];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Object::Integer(3)),
            ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Object::Integer(6)),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Object::Integer(2)),
            ("[1, 2, 3][3]", Object::Null),
            ("[1, 2, 3][-1]", Object::Null),
            ("{\"foo\": 5}[\"foo\"]", Object::Integer(5)),
            ("{\"foo\": 5}[\"bar\"]",Object::Null),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Object::Integer(5)),
            ("{}[\"foo\"]", Object::Null),
            ("{5: 5}[5]", Object::Integer(5)),
            ("{true: 5}[true]", Object::Integer(5)),
            ("{false: 5}[false]", Object::Integer(5)),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }

    #[test]
    fn test_variable_reassignment() {
        let tests = [
            ("let i = 1; i = 2; i", Object::Integer(2)),
            ("let i = 1; i = \"hello\"; i", Object::String("hello".to_owned())),
            ("i = 1; i", Object::Error("identifier i not found".to_owned())),
            (
                "let arr = [0, 1, 2]; arr[0] = {}; arr", 
                Object::Array(vec![
                    Object::Hash(HashMap::new()),
                    Object::Integer(1), Object::Integer(2)
                ])
            ),
            (
                "let arr = [0]; arr[0] = [1]; arr",
                Object::Array(vec![
                    Object::Array(vec![Object::Integer(1)])
                ])
            ),
            (
                "let hash = {1: 1, 2: 2}; hash[1] = 3; hash",
                Object::Hash(HashMap::from([
                    (Object::Integer(1), Object::Integer(3)),
                    (Object::Integer(2), Object::Integer(2))
                ]))
            ),
            (
                "let hash = {}; hash[\"a\"] = \"b\"; hash",
                Object::Hash(HashMap::from([
                    (Object::String("a".to_owned()), Object::String("b".to_owned()))
                ]))
            ),
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }
}
