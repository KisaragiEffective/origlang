#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use crate::after_parse::Expression;
use std::fmt::{Display, Formatter};

pub mod after_parse;

/// 現時点のプログラムとは、プリントするべき式の列である
#[expect(clippy::module_name_repetitions)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RootAst {
    pub statement: Vec<Statement>,
}

/// 分岐を含まない任意のパターン
#[derive(Eq, PartialEq, Clone, Debug)]
pub enum SinglePattern {
    Discard,
    Bind(Identifier),
    Tuple(Vec<Self>),
    // TODO: 定数パターンやガード付きパターンを記述できるようにする
}

impl SinglePattern {
    // 将来実装するので今は黙らせる
    #[expect(clippy::must_use_candidate, clippy::missing_const_for_fn)]
    pub fn is_irrefutable_with_scrutinee_type_information(&self) -> bool {
        true
    }
}

impl Display for SinglePattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Discard => f.write_str("_"),
            Self::Bind(i) => f.write_str(i.as_name()),
            Self::Tuple(v) => {
                f.write_str("(")?;
                for i in v {
                    Display::fmt(i, f)?;
                    f.write_str(", ")?;
                }
                f.write_str(")")
            }
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    /// <`int_literal`> <`new_line`>
    Print {
        expression: Expression,
    },
    VariableDeclaration {
        pattern: SinglePattern,
        expression: Expression,
        type_annotation: Option<TypeSignature>,
    },
    VariableAssignment {
        identifier: Identifier,
        expression: Expression,
    },
    Block {
        inner_statements: Vec<Self>,
    },
    Comment {
        content: Comment,
    },
    Exit,
    TypeAliasDeclaration {
        new_name: Identifier,
        replace_with: TypeSignature,
    },
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Comment {
    pub content: Box<str>,
}

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Identifier(Box<str>);

impl Identifier {
    #[must_use = "If you don't use it, it will drop entire string"]
    pub const fn new(s: Box<str>) -> Self {
        Self(s)
    }

    #[must_use = "If you don't use it, it will drop entire string"]
    pub fn name(self) -> Box<str> {
        self.0
    }

    #[expect(clippy::must_use_candidate)]
    pub fn as_name(&self) -> &str {
        &self.0
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum TypeSignature {
    Simple(Identifier),
    Tuple(Vec<Self>),
}

impl From<Identifier> for TypeSignature {
    fn from(value: Identifier) -> Self {
        Self::Simple(value)
    }
}

impl Display for TypeSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple(x) => <Identifier as Display>::fmt(x, f),
            Self::Tuple(v) => {
                for x in v {
                    <Self as Display>::fmt(x, f)?;
                }

                Ok(())
            }
        }
    }
}
