/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
pub struct RootAst {
    pub(crate) statement: Vec<Statement>
}

pub enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Term,
    },
    VariableDeclaration {
        identifier: String,
        expression: Term,
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
        lhs: Box<Term>,
        rhs: Box<Term>,
    },
    WrappedTerm(Term),
}

impl Expression {
    fn binary_plus(lhs: Term, rhs: Term) -> Self {
        Self::BinaryPlus {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn term(term: Term) -> Self {
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
