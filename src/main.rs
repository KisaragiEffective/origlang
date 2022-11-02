#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod parser;
mod ast;
mod runtime;
mod lexer;
mod char_list;
mod cli;
mod type_check;

use crate::cli::args::Args;

fn main() -> Result<(), String> {
    use clap::Parser;
    let args = Args::parse();
    args.execute()
}
