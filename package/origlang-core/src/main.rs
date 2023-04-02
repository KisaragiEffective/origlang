#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use wasm_bindgen::prelude::wasm_bindgen;
use crate::ast::RootAst;
use crate::lexer::Lexer;
use crate::parser::{Parser, SimpleErrorWithPos};

mod parser;
mod ast;
mod runtime;
mod lexer;
mod char_list;
mod cli;
mod type_check;
pub(crate) mod error;
mod platform;

fn main() -> Result<(), crate::error::AllError> {
    use crate::cli::args::Args;
    use clap::Parser;
    // std::env::set_var("RUST_LOG", "trace");
    ::env_logger::init();
    let args = Args::parse();
    if let Err(e) = args.execute() {
        log::error!("{e}", e = &e);
        return Err(e)
    }
    Ok(())
}
