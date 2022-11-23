use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use crate::cli::task::Task;
use crate::parser::Parser;
use crate::parser::SimpleErrorWithPos;

pub struct PrintAst;

impl Task for PrintAst {
    type Environment = ParseSource;
    type Error = SimpleErrorWithPos;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let root_ast = Parser::create(environment.source().as_str()).parse()?;
        println!("{root_ast:#?}");
        Ok(())
    }
}

pub enum ParseSource {
    RawSource(String),
    FromFile(PathBuf)
}

impl ParseSource {
    pub fn source(&self) -> String {
        match self {
            Self::RawSource(a) => a.clone(),
            Self::FromFile(path) => {
                let mut buf = String::new();
                BufReader::new(File::open(path).unwrap()).read_to_string(&mut buf).unwrap();
                buf
            }
        }
    }
}
