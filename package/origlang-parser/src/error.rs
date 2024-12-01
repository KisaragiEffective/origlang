use crate::parser::TokenKind;
use crate::recover::{IntermediateStateCandidate, PartiallyParseFixCandidate};
use derive_more::Display;
use origlang_lexer::error::LexerError;
use origlang_lexer::token::Token;
use origlang_source_span::{Pointed, SourcePosition as SourcePos, SourcePosition};
use std::fmt::{Display, Formatter};
use std::num::ParseIntError;
use thiserror::Error as ThisError;

#[derive(ThisError, Debug, Eq, PartialEq)]
pub struct ParserError(Pointed<ParserErrorInner>);

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} ({})", &self.0.data, &self.0.position))
    }
}

impl ParserError {
    #[must_use]
    pub const fn new(kind: ParserErrorInner, position: SourcePos) -> Self {
        Self(Pointed {
            data: kind,
            position,
        })
    }

    #[must_use]
    pub const fn kind(&self) -> &ParserErrorInner {
        &self.0.data
    }

    #[must_use]
    pub const fn position(&self) -> &SourcePosition {
        &self.0.position
    }
}

#[derive(ThisError, Debug, Eq, PartialEq)]
#[expect(clippy::module_name_repetitions)]
pub enum ParserErrorInner {
    #[error("lexer error: {_0}")]
    LexerError(#[from] LexerError),
    #[error("unconsumed token found: {token:?}")]
    UnconsumedToken { token: Token },
    #[error("statement must be terminated by a newline")]
    StatementTerminationError,
    #[error("EOF Error")]
    EndOfFileError,
    #[error("Expected {pat}, but got {unmatch:?}")]
    UnexpectedToken { pat: TokenKind, unmatch: Token },
    #[error("Incomplete program snippet. Check hint for fix candidates. hint:{hint:?} state:{intermediate_state:?}")]
    PartiallyParsed {
        hint: Vec<PartiallyParseFixCandidate>,
        intermediate_state: Vec<IntermediateStateCandidate>,
    },
    #[error("input sequence cannot be parsed as a int literal: {error}")]
    UnParsableIntLiteral { error: ParseIntError },
    #[error("int literal type of {tp} must be in range ({min}..={max}), but its value is {value}")]
    OverflowedLiteral {
        tp: Box<str>,
        min: i64,
        max: i64,
        value: i64,
    },
    #[error("if expression requires `else` clause")]
    IfExpressionWithoutElseClause,
    #[error("if expression requires `then` clause and `else` clause")]
    IfExpressionWithoutThenClauseAndElseClause,
    #[error("tuple literal requires 2 or more elements, but got {_0}")]
    InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount),
    #[error("`_` cannot used as right hand side expression")]
    UnderscoreCanNotBeRightHandExpression,
}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, Display)]
pub enum UnexpectedTupleLiteralElementCount {
    #[display(fmt = "no elements")]
    Zero = 0,
    #[display(fmt = "only one element")]
    One = 1,
}
