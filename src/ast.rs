/// 現時点のプログラムとは、プリントするべき式の列である
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
    }
}

impl Into<Expression> for i32 {
    fn into(self) -> Expression {
        Expression::IntLiteral(self)
    }
}
