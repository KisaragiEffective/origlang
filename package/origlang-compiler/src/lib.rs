#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod char_list;
mod error;
pub mod lexer;
pub mod parser;
pub mod type_check;
mod chars;
