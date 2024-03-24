use std::io::{self, BufRead, Write};
use crate::{lexer, parser, evaluator::evaluate_program, environment::Environment};

fn print_parse_errors<W>(output: &mut W, errors: &Vec<parser::ParseError>) -> io::Result<()>
where W: Write {
    for error in errors {
        output.write_all(format!("{}", error).as_bytes())?;
    }
    Ok(())
}

pub fn start<R, W>(input: &mut R, output: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: Write,
{
    let mut env = Environment::new();

    output.write_all("\nMonkeyLang 0.1\n".as_bytes()).unwrap();
    output.write_all(">>> ".as_bytes()).unwrap();
    output.flush().unwrap();
    for line in input.lines() {
        if let Ok(line) = line {
            let lexer = lexer::Lexer::new(line.as_bytes());
            let mut parser = parser::Parser::new(lexer);

            let program = parser.parse();
            if !parser.get_errors().is_empty() {
                print_parse_errors(output, parser.get_errors())?;
                continue;
            }

            let evaluated = evaluate_program(program, &mut env);
            match evaluated {
                Some(obj) => output.write_all(obj.inspect().as_bytes())?,
                None => continue,
            };
        } else {
            eprintln!("ERROR: could not read input line");
        }
        output.write_all("\n>>> ".as_bytes()).unwrap();
        output.flush().unwrap();
    }
    Ok(())
}

