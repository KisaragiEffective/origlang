#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod parser;
mod ast;
mod runtime;
mod lexer;
mod char_list;

use ast::{First, RootAst, Statement};
use runtime::Runtime;
use crate::lexer::Lexer;

fn main() -> Result<(), String> {
    eprintln!("start");
    integrated_test("123456\n")?;
    integrated_test("1\n2\n")?;
    integrated_test("var x = 1\nx\n")?;
    integrated_test("var x = 1\nvar y = x\ny\n")?;
    // plus operator test (binary)
    integrated_test("1 + 2\n")?;
    integrated_test("var x = 1\n1 + x\n")?;
    integrated_test("var x = 1\nvar y = 3\nx + y\n")?;

    // plus operator test (more than twice)
    integrated_test("var x = 1\nvar y = 2\nvar z = 3\nx + y + z\n")?;

    // minus operator test (binary)
    integrated_test("1 - 2\n")?;
    integrated_test("var x = 1\n1 - x\n")?;
    integrated_test("var x = 1\nvar y = 3\nx - y\n")?;

    // minus operator test (more than twice)
    // NOTE: should be zero
    integrated_test("var x = 1\nvar y = 2\nvar z = 3\nz - x - y\n")?;

    // paren test
    // should be 1
    integrated_test("(1)\n")?;
    // should be 2
    integrated_test("3 - (2 - 1)\n")?;
    // should be 0
    integrated_test("(3 - 2) - 1\n")?;

    // multiply test
    // should be 6
    integrated_test("3 * 2\n")?;
    // should be 7, should not be 9
    integrated_test("3 * 2 + 1\n")?;
    // should be 9
    integrated_test("3 * (2 + 1)\n")?;

    eprintln!("end");
    Ok(())
}

fn integrated_test(src: &str) -> Result<(), String> {
    test_source_with_lexer(src);
    test_source(src)
}

fn test_source(src: &str) -> Result<(), String> {
    use crate::parser::Parser;
    eprintln!("source_test: ----------");
    eprint!("{src}");
    eprintln!("------------------");
    let source = src;
    let parser = Parser::create(source);
    let root_ast = parser.parse()?;
    let runtime = Runtime::create(root_ast);
    runtime.execute();
    Ok(())
}

fn test_source_with_lexer(src: &str) {
    eprintln!("lexer: ----------");
    eprint!("{src}");
    eprintln!("------------------");
    let lexer = Lexer::create(src);
    (0..9).into_iter().map(|_| lexer.next()).for_each(|token| {
        println!("{token:?}");
    });
}
