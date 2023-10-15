#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt::{Display, Formatter};
use crate::after_parse::Expression;

pub mod after_parse;

/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RootAst {
    pub statement: Vec<Statement>
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum AtomicPattern {
    Discard,
    Bind(Identifier)
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Expression,
    },
    VariableDeclaration {
        pattern: AtomicPattern,
        expression: Expression,
        type_annotation: Option<TypeSignature>,
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
    TypeAliasDeclaration {
        new_name: Identifier,
        replace_with: TypeSignature,
    },
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
            TypeSignature::Simple(x) => <Identifier as Display>::fmt(x, f),
            TypeSignature::Tuple(v) => {
                for x in v {
                    <Self as Display>::fmt(x, f)?;
                }

                Ok(())
            }
        }
    }
}