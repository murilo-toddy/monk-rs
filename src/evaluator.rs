use crate::{object::Object, ast::{Program, Statement, Expression}};

fn evaluate_prefix_expression(operator: String, right: Expression) -> Object {
    let right_eval = evaluate_expression(right);
    match operator.as_str() {
        "!" => {
            match right_eval {
                Object::Boolean(value) => Object::Boolean(!value),
                Object::Integer(value) => Object::Boolean(value == 0),
                Object::Null => Object::Boolean(true),
                _ => Object::Boolean(false),
            }
        }
        "-" => {
            match right_eval {
                Object::Integer(value) => Object::Integer(-value),
                _ => Object::Null,
            }
        }
        _ => Object::Null,
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
        Expression::Infix { .. } => todo!("not implemented"),
        Expression::If { .. } => todo!("not implemented"),
        Expression::Function { .. } => todo!("not implemented"),
        Expression::Call { .. } => todo!("not implemented"),
    }
}

fn evaluate_statement(statement: Statement) -> Option<Object> {
    match statement {
        Statement::Let { .. } => todo!("not implemented"),
        Statement::Return { .. } => todo!("not implemented"),
        Statement::Expression { expression, .. } => expression.map(|e| evaluate_expression(e)),
    }
}

pub fn evaluate_program(program: Program) -> Option<Object> {
    let mut result = None;
    for statement in program.0 {
        result = evaluate_statement(statement);
    }
    return result;
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
}
