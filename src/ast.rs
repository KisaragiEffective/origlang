use std::num::NonZeroUsize;
use derive_more::Display;
use crate::ast::after_parse::Expression;

pub mod after_parse;

/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
#[derive(Debug, Clone)]
pub struct RootAst {
    pub(crate) statement: Vec<Statement>
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

mod tests {
    use crate::ast::SourcePos;

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
        identifier: String,
        expression: Expression,
    },
    VariableAssignment {
        identifier: String,
        expression: Expression,
    },
    Block {
        inner_statements: Box<Vec<Self>>
    }
}
