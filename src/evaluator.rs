use crate::{object::Object, ast::{Program, Statement, Expression}};

fn evaluate_expression(expression: Expression) -> Object {
    match expression {
        Expression::Identifier { value, .. } => todo!("not implemented"),
        Expression::Integer { value, .. } => Object::Integer(value),
        Expression::Boolean { value, .. } => todo!("not implemented"),
        Expression::Prefix { operator, right, .. } => todo!("not implemented"),
        Expression::Infix { left, operator, right, .. } => todo!("not implemented"),
        Expression::If { condition, consequence, alternative, .. } => todo!("not implemented"),
        Expression::Function { arguments, body, .. } => todo!("not implemented"),
        Expression::Call { function, arguments, .. } => todo!("not implemented"),
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
        ];

        for (input, expected) in tests {
            let evaluated = eval_input(input);
            assert_eq!(evaluated, expected);
        }
    }
}
