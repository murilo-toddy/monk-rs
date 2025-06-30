use std::env;
use std::fs;
use std::io;
use std::io::{BufRead, Write};

pub mod ast;
pub mod builtins;
pub mod code;
pub mod compiler;
pub mod environment;
pub mod evaluator;
pub mod frame;
pub mod lexer;
pub mod object;
pub mod parser;
pub mod symbol;
pub mod token;
pub mod vm;

#[derive(Debug)]
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

pub fn start_repl<R, W>(input: &mut R, output: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: Write,
{
    let env = environment::Environment::new();
    let mut evaluator = evaluator::Evaluator::new(env);

    let mut compiler = compiler::Compiler::new();
    let mut vm = vm::Vm::new();

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

fn get_help_message() -> &'static str {
    return "  -h: 
    -h: ";
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let output = io::stdout();
    let mut writer = output.lock();

    let mut execution_mode = ExecutionMode::Compiled;

    if args.len() > 1 {
        let mut i = 1;
        let mut arg = &args[i];
        while arg.starts_with("-") {
            match arg.as_str() {
                "-c" => execution_mode = ExecutionMode::Compiled,
                "-i" => execution_mode = ExecutionMode::Interpreted,
                "-h" => {
                    writer.write_all(get_help_message().as_bytes())?;
                    writer.flush()?;
                    std::process::exit(0);
                }
                _ => {
                    writer.write_all(format!("ERROR: invalid flag {}.\n", arg).as_bytes())?;
                    writer.write_all(get_help_message().as_bytes())?;
                    writer.flush()?;
                    std::process::exit(1);
                }
            };
            i += 1;
            arg = &args[i];
        }
    }

    println!("{:?}", execution_mode);

    if args.len() == 2 {
        let file = &args[1];
        let input = match fs::read_to_string(file) {
            Ok(content) => content,
            Err(error) => {
                writer.write_all(
                    format!("ERROR: Unable to open file {}: {}\n", file, error.kind()).as_bytes(),
                )?;
                writer.flush()?;
                std::process::exit(1);
            }
        };

        let env = environment::Environment::new();
        let mut evaluator = evaluator::Evaluator::new(env);
        let lexer = lexer::Lexer::new(input.as_bytes());
        let mut parser = parser::Parser::new(lexer);
        let program = parser.parse();
        if !parser.get_errors().is_empty() {
            for error in parser.get_errors() {
                writer.write_all(format!("{}", error).as_bytes())?;
            }
            writer.flush()?;
        }
        evaluator.evaluate_program(program);
    } else if args.len() == 1 {
        let input = io::stdin();
        let mut reader = input.lock();
        start_repl(&mut reader, &mut writer)?;
    } else {
        writer.write_all(format!("Usage: {} <filename>.mk\n", args[0]).as_bytes())?;
        writer.flush()?;
    }
    Ok(())
}
