use crate::ast::after_parse::Expression;

pub mod after_parse;

/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct RootAst {
    pub(crate) statement: Vec<Statement>
}

#[derive(Debug)]
pub enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Expression,
    },
    VariableDeclaration {
        identifier: String,
        expression: Expression,
    }
}
