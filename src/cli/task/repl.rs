use std::io::{stdout, Write};
use crate::cli::task::Task;
use crate::parser::{Parser, SimpleErrorWithPos};
use crate::runtime::Runtime;
use crate::type_check::error::TypeCheckError;
use crate::type_check::TypeChecker;

pub struct Repl;

impl Task for Repl {
    type Environment = ();
    type Error = TypeCheckError;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        let mut line_count = 1;
        let runtime = Runtime::create();
        println!("Welcome to REPL!");
        let checker = TypeChecker::new();
        loop {
            print!("REPL:{line_count:03}> ");
            stdout().flush().unwrap();
            let line = {
                let mut buf = String::new();
                std::io::stdin().read_line(&mut buf).expect("??");
                buf
            };
            if line == "\x04\n" {
                break
            }
            let parser = Parser::create(line.as_str());
            match parser.parse() {
                Ok(ast) => {
                    checker.check(&ast)?;
                    runtime.execute(&ast);
                }
                Err(error_message) => {
                    eprintln!("There was an error while parse: {error_message}");
                }
            }
            line_count += 1;
        }

        Ok(())
    }
}