use std::fs;
use std::io;
use std::env;
use std::io::Write;

pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;
pub mod parser;
pub mod object;
pub mod evaluator;
pub mod environment;
pub mod builtins;
pub mod code;
pub mod compiler;
pub mod vm;
pub mod symbol;
pub mod frame;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let output = io::stdout();
    let mut writer = output.lock();

    if args.len() == 2 {
        let input = fs::read_to_string(&args[1])
            .expect("Should be able to read file");

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
    }

    else if args.len() == 1 {
        let input = io::stdin();
        let mut reader = input.lock();
        repl::start(&mut reader, &mut writer)?;
    }

    else {
        writer.write_all("Usage: cargo run -- <filename>.mk\n".as_bytes())?;
        writer.flush()?;
    }
    Ok(())
}
