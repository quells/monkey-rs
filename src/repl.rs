use std::io::{self, Write};

fn read_line(prompt: Option<String>) -> io::Result<String> {
    match prompt {
        Some(p) => print!("{}", p),
        None => (),
    }
    let _ = io::stdout().flush();

    let mut buffer = String::new();
    let _ = io::stdin().read_line(&mut buffer)?;
    Ok(buffer)
}

use crate::lex;

pub fn tokenize() {
    println!("ctrl-c to quit");
    loop {
        let input = match read_line(Some("> ".to_owned())) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("could not read line: {}", e);
                return;
            }
        };
        for t in lex::lex(&input) {
            println!("{:?}", t);
        }
    }
}