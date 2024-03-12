use std::io;

pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;
pub mod ast_refact;
pub mod parser;

fn main() -> io::Result<()> {
    let input = io::stdin();
    let output = io::stdout();

    let mut reader = input.lock();
    let mut writer = output.lock();

    repl::start(&mut reader, &mut writer)?;
    Ok(())
}
