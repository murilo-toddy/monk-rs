use crate::{object::Object, ast::{Program, Statement, Expression, BlockStatement}};

// TODO handle errors in a more rusty way
fn is_error(object: &Object) -> bool {
    matches!(object, Object::Error(_))
}

fn is_truthy(object: Object) -> bool {
    !matches!(object, Object::Boolean(false) | Object::Integer(0) | Object::Null)
}

fn evaluate_prefix_expression(operator: String, right: Expression) -> Object {
    let right_eval = evaluate_expression(right);
    if is_error(&right_eval) {
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

fn evaluate_infix_expression(operator: String, left: Expression, right: Expression) -> Object {
    let left_eval = evaluate_expression(left);
    if is_error(&left_eval) {
        return left_eval;
    }
    let right_eval = evaluate_expression(right);
    if is_error(&right_eval) {
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
        _ => {
            if std::mem::discriminant(&left_eval) != std::mem::discriminant(&right_eval) {
                Object::Error(format!("type mismatch: {} {} {}", left_eval.inspect(), operator, right_eval.inspect()))
            } else {
                Object::Error(format!("unknown operation: {} {} {}", left_eval.inspect(), operator, right_eval.inspect()))
            }
        }
    }
}

fn evaluate_block_statement(block: BlockStatement) -> Object {
    let mut result = None;
    for statement in block.statements {
        result = evaluate_statement(statement);
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
    condition: Expression, 
    consequence: BlockStatement,
    alternative: Option<BlockStatement>
) -> Object {
    let condition_eval = evaluate_expression(condition);
    if is_error(&condition_eval) {
        return condition_eval;
    }

    if is_truthy(condition_eval) {
        evaluate_block_statement(consequence)
    } else {
        alternative.map_or(Object::Null, evaluate_block_statement)
    }
}

fn evaluate_expression(expression: Expression) -> Object {
    match expression {
        Expression::Identifier { .. } => todo!("not implemented"),
        Expression::Integer { value, .. } => Object::Integer(value),
        // TODO there's the suggestion to share boolean objects but it's probably
        // not going to satisfy the borrow checker
        Expression::Boolean { value, .. } => Object::Boolean(value),
        Expression::Prefix { operator, right, .. } => {
            evaluate_prefix_expression(operator, *right)
        },
        Expression::Infix { operator, left, right, .. } => {
            evaluate_infix_expression(operator, *left, *right)
        },
        Expression::If { condition, consequence, alternative, .. } => {
            evaluate_conditional_expression(*condition, consequence, alternative)
        },
        Expression::Function { .. } => todo!("not implemented"),
        Expression::Call { .. } => todo!("not implemented"),
    }
}

fn evaluate_statement(statement: Statement) -> Option<Object> {
    match statement {
        Statement::Let { .. } => todo!("not implemented"),
        Statement::Return { value, .. } => {
            let value_eval = value.map(evaluate_expression)?;
            Some(Object::ReturnValue(Box::new(value_eval)))
        },
        Statement::Expression { expression, .. } => expression.map(evaluate_expression),
    }
}

pub fn evaluate_program(program: Program) -> Option<Object> {
    let mut result = None;
    for statement in program.0 {
        result = evaluate_statement(statement);
        match result {
            Some(Object::ReturnValue(result)) => return Some(*result),
            Some(Object::Error(_)) => return result,
            _ => {}
        }
    }
    result
}

#[cfg(test)]
mod evaluator_tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn eval_input(input: &str) -> Object {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        assert_eq!(parser.get_errors().len(), 0);
        return evaluate_program(program).unwrap();
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
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected, "{:?}", input);
        }
    }
}

