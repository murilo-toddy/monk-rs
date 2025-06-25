use crate::{
    compiler::Compiler, environment::Environment, evaluator::Evaluator, lexer, parser, vm::Vm,
};
use std::io::{self, BufRead, Write};

enum ExecutionMode {
    Interpreted,
    Compiled,
}

fn print_parse_errors<W>(output: &mut W, errors: &Vec<parser::ParseError>) -> io::Result<()>
where
    W: Write,
{
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
    let env = Environment::new();
    let mut evaluator = Evaluator::new(env);

    let mut compiler = Compiler::new();
    let mut vm = Vm::new();

    let mode = ExecutionMode::Compiled;

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

            let out = match mode {
                ExecutionMode::Interpreted => {
                    let evaluated = evaluator.evaluate_program(program);
                    match evaluated {
                        Some(obj) => obj.inspect(),
                        None => "".to_owned(),
                    }
                }
                ExecutionMode::Compiled => {
                    compiler = compiler.reset();
                    let mut error_message = String::new();
                    if let Err(msg) = compiler.compile(program) {
                        error_message = format!("Compilation failed:\n{}\n", msg);
                    };
                    // TODO remove this clone
                    vm = vm.reset(compiler.clone().bytecode());
                    if let Err(msg) = vm.run() {
                        error_message = format!("Running bytecode failed:\n{}\n", msg);
                    }
                    if !error_message.is_empty() {
                        error_message
                    } else {
                        match vm.last_popped_elem() {
                            Some(obj) => obj.inspect(),
                            None => "".to_owned(),
                        }
                    }
                }
            };
            output.write_all(out.as_bytes())?;
        } else {
            eprintln!("ERROR: could not read input line");
        }
        output.write_all("\n>>> ".as_bytes()).unwrap();
        output.flush().unwrap();
    }
    Ok(())
}
