use crate::{object::Object, ast::{Program, Statement, Expression, BlockStatement}, environment::Environment};

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {

    pub fn new(env: Environment) -> Evaluator {
        Evaluator { env }
    }

    // TODO handle errors in a more rusty way
    fn is_error(&self, object: &Object) -> bool {
        matches!(object, Object::Error(_))
    }

    fn is_truthy(&self, object: Object) -> bool {
        !matches!(object, Object::Boolean(false) | Object::Integer(0) | Object::Null)
    }

    fn evaluate_prefix_expression(&mut self, operator: String, right: Expression) -> Object {
        let right_eval = self.evaluate_expression(right);
        if self.is_error(&right_eval) {
            return right_eval;
        }
        // TODO move the common error out
        match operator.as_str() {
            "!" => {
                match right_eval {
                    Object::Boolean(value) => Object::Boolean(!value),
                    Object::Integer(value) => Object::Boolean(value == 0),
                    Object::Null => Object::Boolean(true),
                    _ => Object::Error(format!("unknown operation: {}{}", operator, right_eval.inspect())),
                }
            }
            "-" => {
                match right_eval {
                    Object::Integer(value) => Object::Integer(-value),
                    _ => Object::Error(format!("unknown operation: {}{}", operator, right_eval.inspect())),
                }
            }
            _ => Object::Error(format!("unknown operation: {}{}", operator, right_eval.inspect())),
        }
    }

    fn evaluate_infix_expression(
        &mut self,
        operator: String, 
        left: Expression,
        right: Expression,
    ) -> Object {
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
                    ">" => Object::Boolean(left_val > right_val),
                    "<" => Object::Boolean(left_val < right_val),
                    _ => Object::Error(format!("unknown operation: {} {} {}", left_val, operator, right_val))
                }
            },
            (Object::Boolean(left_val), Object::Boolean(right_val)) => {
                match operator.as_str() {
                    "==" => Object::Boolean(left_val == right_val),
                    "!=" => Object::Boolean(left_val != right_val),
                    _ => Object::Error(format!("unknown operation: {} {} {}", left_val, operator, right_val))
                }
            }
            (Object::String(left_val), Object::String(right_val)) => {
                match operator.as_str() {
                    "+" => Object::String(left_val.to_owned() + right_val),
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
        condition: Expression, 
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
        ) -> Object {
        let condition_eval = self.evaluate_expression(condition);
        if self.is_error(&condition_eval) {
            return condition_eval;
        }

        if self.is_truthy(condition_eval) {
            self.evaluate_block_statement(consequence)
        } else {
            alternative.map_or(Object::Null, |a| self.evaluate_block_statement(a))
        }
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
        return result;
    }

    fn evaluate_expression(&mut self, expression: Expression) -> Object {
        match expression {
            Expression::Identifier { value, .. } => {
                self.env.get(&value).unwrap_or(Object::Error(format!("identifier not found: {}", value)))
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
            Expression::If { condition, consequence, alternative, .. } => {
                self.evaluate_conditional_expression(*condition, consequence, alternative)
            },
            Expression::Function { arguments, body, .. } => {
                Object::Function(arguments, body, self.env.clone())
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
            }
            _ => Object::Error("expected function".to_owned())
        }
    }

    fn extend_function_environment(&mut self, func: Object, args: Vec<Object>) -> Environment {
        if let Object::Function(arguments, _, environment) = func {
            let mut env = Environment::new_enclosed(environment);
            for (i, arg) in arguments.iter().enumerate() {
                // TODO remove clones
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
                // TODO remove this clone
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
            assert_eq!(evaluated, expected);
        }
    }

    #[test]
    fn test_string_expression_eval() {
        let tests = vec![
            ("\"Hello World!\"", Object::String("Hello World!".to_string())),
            ("\"Hello\" + \" \" + \"World!\"", Object::String("Hello World!".to_string())),
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
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
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
}

