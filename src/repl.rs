extern crate time;

use std::io::{self, Write};

fn read_line(prompt: Option<String>) -> io::Result<String> {
    if let Some(p) = prompt {
        print!("{}", p)
    }
    let _ = io::stdout().flush();

    let mut buffer = String::new();
    let _ = io::stdin().read_line(&mut buffer)?;
    Ok(buffer)
}

use crate::lex::lex;

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
        for t in lex(&input) {
            println!("{:?}", t);
        }
    }
}

use crate::parse::parse;

pub fn parser(debug: bool) {
    println!("ctrl-c to quit");
    loop {
        let input = match read_line(Some("> ".to_owned())) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("could not read line: {}", e);
                return;
            }
        };

        let mut lex_time = time::precise_time_ns();
        let tokens = lex(&input);
        lex_time = time::precise_time_ns() - lex_time;

        let mut parse_time = time::precise_time_ns();
        let program = parse(&tokens);
        parse_time = time::precise_time_ns() - parse_time;

        match program {
            Ok(program) => {
                if debug {
                    for s in program.statements {
                        println!("{:?}", s);
                    }
                } else {
                    for s in program.statements {
                        println!("{}", s);
                    }
                }
            }
            Err(e) => {
                eprintln!("{}", e);
            }
        }

        if debug {
            println!("{} to lex", human_time(lex_time));
            println!("{} to parse", human_time(parse_time));
        }
    }
}

fn human_time(t: u64) -> String {
    if t < 1000 {
        return format!("{} ns", t);
    }

    let t = t / 1000;
    if t < 1000 {
        return format!("{} us", t);
    }

    let t = t / 1000;
    if t < 1000 {
        return format!("{} ms", t);
    }

    let t = t / 1000;
    return format!("{} s", t);
}
