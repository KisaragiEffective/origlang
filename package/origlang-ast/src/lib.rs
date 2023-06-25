#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use derive_more::Display;
use crate::after_parse::Expression;

pub mod after_parse;

/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RootAst {
    pub statement: Vec<Statement>
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Ord, PartialOrd, Display)]
#[display(fmt = "{line}:{column}")]
pub struct SourcePos {
    pub line: NonZeroUsize,
    pub column: NonZeroUsize,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub struct WithPosition<T> {
    pub position: SourcePos,
    pub data: T,
}

#[cfg(test)]
mod tests {
    use crate::SourcePos;

    #[test]
    fn source_pos_order() {
        // 辞書式順序の理解があっているかどうか
        assert!(SourcePos {
            line: 1.try_into().unwrap(),
            column: 1.try_into().unwrap(),
        } < SourcePos {
            line: 1.try_into().unwrap(),
            column: 2.try_into().unwrap(),
        });
        assert!(SourcePos {
            line: 1.try_into().unwrap(),
            column: usize::MAX.try_into().unwrap(),
        } < SourcePos {
            line: 2.try_into().unwrap(),
            column: 1.try_into().unwrap(),
        });
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Expression,
    },
    VariableDeclaration {
        identifier: Identifier,
        expression: Expression,
    },
    VariableAssignment {
        identifier: Identifier,
        expression: Expression,
    },
    Block {
        inner_statements: Vec<Self>
    },
    Comment {
        content: Comment,
    },
    Exit,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Comment {
    pub content: String
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier(String);

impl Identifier {
    #[must_use = "If you don't use it, it will drop entire String"]
    pub const fn new(s: String) -> Self {
        Self(s)
    }

    #[must_use = "If you don't use it, it will drop entire String"]
    #[allow(clippy::missing_const_for_fn)] // see https://github.com/rust-lang/rust-clippy/issues/10617
    pub fn name(self) -> String {
        self.0
    }

    #[allow(clippy::must_use_candidate)]
    pub fn as_name(&self) -> &str {
        &self.0
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
