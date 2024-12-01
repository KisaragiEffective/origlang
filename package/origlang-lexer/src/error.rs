use crate::boundary::Utf8CharBoundaryStartByte;
use std::convert::Infallible;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
#[expect(clippy::module_name_repetitions)]
pub enum LexerError {
    #[error(
        "Invalid suffix for integer literal. Supported suffixes are [`i8`, `i16`, `i32`, `i64`]"
    )]
    InvalidSuffix,
    #[error("Internal compiler error: {0}")]
    OutOfRange(#[from] OutOfRangeError),
    #[error("Unclosed string literal was found")]
    UnclosedStringLiteral,
    #[error("Input is malformed UTF-8")]
    MalformedAsUtf8 { boundary: Utf8CharBoundaryStartByte },
    #[error("never: {0}")]
    Never(#[from] Infallible),
}

#[derive(Debug, Error, Eq, PartialEq)]
#[error("lexer index overflow: {current:?} > {max}")]
#[expect(clippy::module_name_repetitions)]
pub struct OutOfRangeError {
    pub current: Utf8CharBoundaryStartByte,
    pub max: usize,
}
