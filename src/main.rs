#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

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
    let args = Args::parse();
    args.execute()?;
    Ok(())
}
