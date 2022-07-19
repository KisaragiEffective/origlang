#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod parser;
mod ast;
mod runtime;
mod lexer;
mod char_list;
mod cli;

use ast::{First, RootAst, Statement};
use runtime::Runtime;
use crate::lexer::Lexer;

fn main() -> Result<(), String> {
    Ok(())
}

