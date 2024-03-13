use std::io::{self, BufRead, Write};
use crate::lexer;
use crate::token::Token;

pub fn start<R, W>(input: &mut R, output: &mut W) -> io::Result<()>
where
    R: BufRead,
    W: Write,
{
    output.write_all("\nMonkeyLang 0.1\n".as_bytes()).unwrap();
    output.write_all(">>> ".as_bytes()).unwrap();
    output.flush().unwrap();
    for line in input.lines() {
        if let Ok(line) = line {
            let mut lexer = lexer::Lexer::new(line.as_bytes());
            let mut token = lexer.next();
            while token != Token::Eof {
                // TODO handle errors
                output.write_all(format!("{:?}\n", token).as_bytes()).unwrap();
                output.flush().unwrap();
                token = lexer.next();
            }
        } else {
            eprintln!("ERROR: could not read input line");
        }
        output.write_all("\n>>> ".as_bytes()).unwrap();
        output.flush().unwrap();
    }
    Ok(())
}

