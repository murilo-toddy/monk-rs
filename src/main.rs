use std::env;
use std::fs;
use std::io;
use std::io::Write;

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
pub mod repl;
pub mod symbol;
pub mod token;
pub mod vm;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let output = io::stdout();
    let mut writer = output.lock();

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
        repl::start(&mut reader, &mut writer)?;
    } else {
        writer.write_all(format!("Usage: {} <filename>.mk\n", args[0]).as_bytes())?;
        writer.flush()?;
    }
    Ok(())
}
