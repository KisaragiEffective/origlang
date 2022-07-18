/// 現時点のプログラムとは、プリントするべき式の列である
#[allow(clippy::module_name_repetitions)]
pub struct RootAst {
    pub(crate) statement: Vec<Statement>
}

pub enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Additive,
    },
    VariableDeclaration {
        identifier: String,
        expression: Additive,
    }
}

/// 「項」を表す。
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum First {
    IntLiteral(i32),
    Variable {
        name: String,
    },
    Parenthesized(Box<Additive>)
}

impl First {
    pub fn parenthesized(expr: Additive) -> Self {
        Self::Parenthesized(Box::new(expr))
    }
}

impl From<i32> for First {
    fn from(i: i32) -> Self {
        Self::IntLiteral(i)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Additive {
    Binary {
        operator: BuiltinOperatorKind,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    WrappedMultiplicative(Multiplicative),
}

impl Additive {
    pub fn binary(operator: BuiltinOperatorKind, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn term(term: First) -> Self {
        term.into()
    }
}

impl From<First> for Additive {
    fn from(term: First) -> Self {
        Self::WrappedMultiplicative(Multiplicative::WrappedFirst(term))
    }
}

impl From<Multiplicative> for Additive {
    fn from(multiplicative: Multiplicative) -> Self {
        Self::WrappedMultiplicative(multiplicative)
    }
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Multiplicative {
    Binary {
        operator: BuiltinOperatorKind,
        lhs: Box<Self>,
        rhs: Box<Self>
    },
    WrappedFirst(First)
}

impl Multiplicative {
    pub fn binary(operator: BuiltinOperatorKind, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub fn term(term: First) -> Self {
        term.into()
    }
}

impl From<First> for Multiplicative {
    fn from(from: First) -> Self {
        Self::WrappedFirst(from)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuiltinOperatorKind {
    Plus,
    Minus,
    Multiple,
    Divide,
}
