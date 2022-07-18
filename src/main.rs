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

#[allow(clippy::unreadable_literal)]
fn main() -> Result<(), String> {
    eprintln!("start");
    assert_eq!(expression_list("123456\n")?, vec![123456]);
    assert_eq!(expression_list("1\n2\n")?, vec![1, 2]);
    assert_eq!(expression_list("var x = 1\nx\n")?, vec![1]);
    assert_eq!(expression_list("var x = 1\nvar y = x\ny\n")?, vec![1]);
    // plus operator test (binary)
    assert_eq!(expression_list("1 + 2\n")?, vec![3]);
    assert_eq!(expression_list("var x = 1\n1 + x\n")?, vec![2]);
    assert_eq!(expression_list("var x = 1\nvar y = 3\nx + y\n")?, vec![4]);

    // plus operator test (more than twice)
    assert_eq!(expression_list("var x = 1\nvar y = 2\nvar z = 3\nx + y + z\n")?, vec![6]);

    // minus operator test (binary)
    assert_eq!(expression_list("1 - 2\n")?, vec![-1]);
    assert_eq!(expression_list("var x = 1\n1 - x\n")?, vec![0]);
    assert_eq!(expression_list("var x = 1\nvar y = 3\nx - y\n")?, vec![-2]);

    // minus operator test (more than twice)
    assert_eq!(expression_list("var x = 1\nvar y = 2\nvar z = 3\nz - x - y\n")?, vec![0]);

    // paren test
    assert_eq!(expression_list("(1)\n")?, vec![1]);
    assert_eq!(expression_list("3 - (2 - 1)\n")?, vec![2]);
    assert_eq!(expression_list("(3 - 2) - 1\n")?, vec![0]);

    // multiply test
    assert_eq!(expression_list("3 * 2\n")?, vec![6]);
    assert_eq!(expression_list("3 * 2 + 1\n")?, vec![7]);
    assert_ne!(expression_list("3 * 2 + 1\n")?, vec![9]);
    assert_eq!(expression_list("3 * (2 + 1)\n")?, vec![9]);
    assert_ne!(expression_list("3 * (2 + 1)\n")?, vec![7]);

    eprintln!("end");
    Ok(())
}

fn expression_list(src: &str) -> Result<Vec<i32>, String> {
    use crate::parser::Parser;
    let source = src;
    let parser = Parser::create(source);
    let root_ast = parser.parse()?;
    let runtime = Runtime::create(root_ast);
    Ok(runtime.yield_all_evaluated_expressions())
}
