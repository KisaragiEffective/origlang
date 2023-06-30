#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt::{Display, Formatter, Write};
use std::num::NonZeroUsize;
use std::path::PathBuf;

#[derive(Eq, PartialEq, Copy, Clone, Debug, Ord, PartialOrd)]
pub struct SourcePosition {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

impl Display for SourcePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.line.to_string())?;
        f.write_char(':')?;
        f.write_str(&self.column.to_string())
    }
}
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct SourceSpan {
    pub start: SourcePosition,
    pub end_inclusive: SourcePosition,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Pointed<T> {
    pub data: T,
    pub position: SourcePosition,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct Spanned<T> {
    pub data: T,
    pub span: SourceSpan,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct CanonicalSourceSpan {
    pub file: PathBuf,
    pub span: SourceSpan,
}

#[cfg(test)]
mod tests {
    use crate::SourcePosition;

    #[test]
    fn source_pos_order() {
        // 辞書式順序の理解があっているかどうか
        assert!(SourcePosition {
            line: 1.try_into().unwrap(),
            column: 1.try_into().unwrap(),
        } < SourcePosition {
            line: 1.try_into().unwrap(),
            column: 2.try_into().unwrap(),
        });
        assert!(SourcePosition {
            line: 1.try_into().unwrap(),
            column: usize::MAX.try_into().unwrap(),
        } < SourcePosition {
            line: 2.try_into().unwrap(),
            column: 1.try_into().unwrap(),
        });
    }
}
