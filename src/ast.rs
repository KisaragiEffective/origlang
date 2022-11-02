
pub type LowestPrecedenceExpression = IfExpression;
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
        expression: LowestPrecedenceExpression,
    },
    VariableDeclaration {
        identifier: String,
        expression: LowestPrecedenceExpression,
    }
}

/// 「項」を表す。
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum First {
    IntLiteral(i32),
    True,
    False,
    Variable {
        name: String,
    },
    Parenthesized(Box<LowestPrecedenceExpression>)
}

impl First {
    pub fn parenthesized(expr: LowestPrecedenceExpression) -> Self {
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
        operator: AdditiveOperatorKind,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Unlifted(Multiplicative),
}

impl Additive {
    pub fn binary(operator: AdditiveOperatorKind, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<Multiplicative> for Additive {
    fn from(multiplicative: Multiplicative) -> Self {
        Self::Unlifted(multiplicative)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Multiplicative {
    Binary {
        operator: MultiplicativeOperatorKind,
        lhs: Box<Self>,
        rhs: Box<Self>
    },
    Unlifted(First),
}

impl From<First> for Multiplicative {
    fn from(f: First) -> Self {
        Self::Unlifted(f)
    }
}

impl Multiplicative {
    pub fn binary(operator: MultiplicativeOperatorKind, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum RelationExpression {
    Binary {
        operator: RelationExpressionOperator,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Unlifted(Additive),
}

impl RelationExpression {
    pub fn binary(operator: RelationExpressionOperator, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<Additive> for RelationExpression {
    fn from(a: Additive) -> Self {
        Self::Unlifted(a)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum RelationExpressionOperator {
    /// `<=`
    LessEqual,
    /// `<`
    Less,
    /// `>=`
    MoreEqual,
    /// `>`
    More,
    /// `<=>`
    // なんとなく面白そうなので
    SpaceShip,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum EqualityExpression {
    Binary {
        operator: EqualityExpressionOperator,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Unlifted(RelationExpression)
}

impl EqualityExpression {
    pub fn binary(operator: EqualityExpressionOperator, lhs: Self, rhs: Self) -> Self {
        Self::Binary {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<RelationExpression> for EqualityExpression {
    fn from(re: RelationExpression) -> Self {
        Self::Unlifted(re)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum EqualityExpressionOperator {
    Equal,
    NotEqual,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IfExpression {
    If {
        condition: Box<Self>,
        then_clause_value: Box<Self>,
        else_clause_value: Box<Self>,
    },
    Lifted(EqualityExpression)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ExpressionBox {
    Unary(First),
    Additive(Additive),
    Multiplicative(Multiplicative),
    RelationExpression(RelationExpression),
    EqualityExpression(EqualityExpression),
}

impl From<First> for ExpressionBox {
    fn from(x: First) -> Self {
        Self::Unary(x)
    }
}

impl From<Additive> for ExpressionBox {
    fn from(x: Additive) -> Self {
        Self::Additive(x)
    }
}

impl From<Multiplicative> for ExpressionBox {
    fn from(x: Multiplicative) -> Self {
        Self::Multiplicative(x)
    }
}

impl From<RelationExpression> for ExpressionBox {
    fn from(x: RelationExpression) -> Self {
        Self::RelationExpression(x)
    }
}

impl From<EqualityExpression> for ExpressionBox {
    fn from(x: EqualityExpression) -> Self {
        Self::EqualityExpression(x)
    }
}


#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BuiltinOperatorKind {
    Plus,
    Minus,
    Multiple,
    Divide,
}

impl From<AdditiveOperatorKind> for BuiltinOperatorKind {
    fn from(additive_op: AdditiveOperatorKind) -> Self {
        match additive_op {
            AdditiveOperatorKind::Plus => Self::Plus,
            AdditiveOperatorKind::Minus => Self::Minus,
        }
    }
}

impl From<MultiplicativeOperatorKind> for BuiltinOperatorKind {
    fn from(multiplicative_op: MultiplicativeOperatorKind) -> Self {
        match multiplicative_op {
            MultiplicativeOperatorKind::Multiple => Self::Multiple,
            MultiplicativeOperatorKind::Divide => Self::Divide,
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum AdditiveOperatorKind {
    Plus,
    Minus,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum MultiplicativeOperatorKind {
    Multiple,
    Divide,
}
