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

impl SourcePosition {
    #[must_use]
    pub const fn new(line: NonZeroUsize, column: NonZeroUsize) -> Self {
        Self {
            line, column
        }
    }

    pub fn try_new<E>(lc: impl TryIntoSourcePosition<Err = E>) -> Result<Self, E> {
        lc.try_into()
    }
}

pub trait TryIntoSourcePosition {
    type Err;

    fn try_into(self) -> Result<SourcePosition, Self::Err>;
}

impl<R: TryInto<NonZeroUsize> + Ord + Eq> TryIntoSourcePosition for (R, R) {
    type Err = R::Error;

    fn try_into(self) -> Result<SourcePosition, Self::Err> {
        let (l, c) = self;
        Ok(SourcePosition::new(l.try_into()?, c.try_into()?))
    }
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
        assert!(SourcePosition::try_new((1, 1)).unwrap() < SourcePosition::try_new((1, 2)).unwrap());
        assert!(SourcePosition::try_new((1, usize::MAX)).unwrap() < SourcePosition::try_new((2, 1)).unwrap());
    }
}
