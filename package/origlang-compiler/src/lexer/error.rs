use std::convert::Infallible;
use thiserror::Error;
use crate::chars::boundary::Utf8CharBoundaryStartByte;

#[derive(Error, Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum LexerError {
    #[error("Invalid suffix for integer literal. Supported suffixes are [`i8`, `i16`, `i32`, `i64`]")]
    InvalidSuffix,
    #[error("Internal compiler error: lexer index overflow: {current:?} > {max}")]
    OutOfRange {
        current: Utf8CharBoundaryStartByte,
        max: usize,
    },
    #[error("Unclosed string literal was found")]
    UnclosedStringLiteral,
    #[error("Input is malformed UTF-8")]
    MalformedAsUtf8 {
        boundary: Utf8CharBoundaryStartByte,
    },
    #[error("never: {0}")]
    Never(#[from] Infallible)
}
