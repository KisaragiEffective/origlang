//! パース時の優先順位が消去された構造体の定義

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Expression {
    /// 整数リテラル
    IntLiteral(i32),
    /// 真偽値リテラル
    BooleanLiteral(bool),
    /// 変数
    Variable {
        ident: String
    },
    /// 四則演算、比較演算、等価性判定
    BinaryOperator {
        lhs: Box<Self>,
        rhs: Box<Self>,
        operator: BinaryOperatorKind,
    },
    /// if式
    If {
        condition: Box<Self>,
        then_clause_value: Box<Self>,
        else_clause_value: Box<Self>,
    }
}

impl Expression {
    pub fn binary<Operator: Into<BinaryOperatorKind>>(operator: Operator, lhs: Self, rhs: Self) -> Self {
        Self::BinaryOperator {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator: operator.into(),
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum BinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    More,
    MoreEqual,
    Less,
    LessEqual,
    ThreeWay,
    Equal,
    NotEqual,
}
