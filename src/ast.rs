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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    IntLiteral(i32),
    Variable {
        name: String,
    },
    Parenthesized(Box<Expression>)
}

impl Term {
    pub fn parenthesized(expr: Expression) -> Self {
        Self::Parenthesized(Box::new(expr))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression {
    Binary {
        operator: BuiltinOperatorKind,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    WrappedTerm(Term),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuiltinOperatorKind {
    Plus,
    Minus,
}

impl Expression {
    pub fn binary(operator: BuiltinOperatorKind, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
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
