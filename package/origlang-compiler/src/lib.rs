#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

pub mod parser;
pub mod type_check;
mod token_stream;
