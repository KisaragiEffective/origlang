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

#[derive(Clone, Debug)]
pub enum Expression {
    IntLiteral(i32),
    Variable {
        name: String,
    },
    Plus {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    }
}

impl Expression {
    fn plus(lhs: Self, rhs: Self) -> Self {
        Self::Plus {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl From<i32> for Expression {
    fn from(i: i32) -> Self {
        Self::IntLiteral(i)
    }
}
