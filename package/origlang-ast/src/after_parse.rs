//! パース時の優先順位が消去された構造体の定義

use derive_more::Display;
use crate::Statement;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Expression {
    /// 整数リテラル
    IntLiteral {
        value: i64,
        suffix: Option<Box<str>>,
    },
    /// 真偽値リテラル
    BooleanLiteral(bool),
    /// 文字列リテラル
    StringLiteral(String),
    /// ユニットリテラル
    UnitLiteral,
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
    },
    Block {
        intermediate_statements: Vec<Statement>,
        final_expression: Box<Self>,
    },
    Tuple {
        expressions: Vec<Self>,
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

#[derive(Eq, PartialEq, Copy, Clone, Debug, Display)]
pub enum BinaryOperatorKind {
    #[display(fmt = "+")]
    Plus,
    #[display(fmt = "-")]
    Minus,
    #[display(fmt = "*")]
    Multiply,
    #[display(fmt = "/")]
    Divide,
    #[display(fmt = ">")]
    More,
    #[display(fmt = ">=")]
    MoreEqual,
    #[display(fmt = "<")]
    Less,
    #[display(fmt = "<=")]
    LessEqual,
    #[display(fmt = "<=>")]
    ThreeWay,
    #[display(fmt = "==")]
    Equal,
    #[display(fmt = "!=")]
    NotEqual,
}
