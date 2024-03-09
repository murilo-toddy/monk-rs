use std::io;

pub mod token;
pub mod lexer;
pub mod repl;

fn main() -> io::Result<()> {
    let input = io::stdin();
    let output = io::stdout();

    let mut reader = input.lock();
    let mut writer = output.lock();

    repl::start(&mut reader, &mut writer)?;
    Ok(())
}
