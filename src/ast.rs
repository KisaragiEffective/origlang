/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
pub struct RootAst {
    pub(crate) statement: Vec<Statement>
}

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

/// 「項」を表す。
#[derive(Clone, Debug)]
pub enum Term {
    IntLiteral(i32),
    Variable {
        name: String,
    },
}

pub enum Expression {
    BinaryPlus {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    WrappedTerm(Term),
}

impl Expression {
    pub fn binary_plus(lhs: Expression, rhs: Expression) -> Self {
        Self::BinaryPlus {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn term(term: Term) -> Self {
        term.into()
    }
}

impl From<Term> for Expression {
    fn from(term: Term) -> Self {
        Self::WrappedTerm(term)
    }
}

impl From<i32> for Term {
    fn from(i: i32) -> Self {
        Self::IntLiteral(i)
    }
}
