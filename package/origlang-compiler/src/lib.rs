#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

pub mod lexer;
pub mod parser;
pub mod type_check;
mod chars;
