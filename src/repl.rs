use std::io::{self, BufRead, Write};
use crate::{ast::Program, compiler::Compiler, environment::Environment, evaluator::Evaluator, lexer, parser, vm::Vm};

enum ExecutionMode {
    Interpreted,
    Compiled,
}

fn print_parse_errors<W>(output: &mut W, errors: &Vec<parser::ParseError>) -> io::Result<()>
where W: Write {
    for error in errors {
        output.write_all(format!("{}", error).as_bytes())?;
    }
    Ok(())
}

fn compile(program: Program) -> String {
    let mut compiler = Compiler::new();
    if let Err(msg) = compiler.compile(program) {
        return format!("Compilation failed:\n{}\n", msg);
    };
    let mut vm = Vm::new(compiler.bytecode());
    if let Err(msg) = vm.run() {
        return format!("Running bytecode failed:\n{}\n", msg);
    }
    match vm.last_popped_elem() {
        Some(obj) => return obj.inspect(),
        None => "".to_owned(),
    }
}

fn interpret(evaluator: &mut Evaluator, program: Program) -> String {
    let evaluated = evaluator.evaluate_program(program);
    match evaluated {
        Some(obj) => return obj.inspect(),
        None => "".to_owned(),
    }
}

pub fn start<R, W>(input: &mut R, output: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: Write,
{
    let env = Environment::new();
    let mut evaluator = Evaluator::new(env);

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
                ExecutionMode::Interpreted => interpret(&mut evaluator, program),
                ExecutionMode::Compiled => compile(program),
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

