#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod parser;
mod ast;
mod runtime;

use ast::{Expression, RootAst, Statement};
use runtime::Runtime;

fn main() -> Result<(), String> {
    eprintln!("start");
    test_source("123456\n")?;
    test_source("var x = 1\nx\n")?;
    test_source("var x = 1\nvar y = x\ny\n")?;
    eprintln!("end");
    Ok(())
}

fn test_source(src: &str) -> Result<(), String> {
    use crate::parser::Parser;
    eprintln!("source: ----------");
    eprint!("{src}");
    eprintln!("------------------");
    let source = "var x = 1\nvar y = x\ny\n";
    let parser = Parser::create(source);
    let root_ast = parser.parse()?;
    let runtime = Runtime::create(root_ast);
    runtime.execute();
    Ok(())
}