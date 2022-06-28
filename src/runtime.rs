use std::cell::RefCell;
use std::collections::HashMap;
use crate::{Term, RootAst, Statement};
use crate::ast::Expression;

pub struct Runtime {
    /// すでに評価された値を格納しておく
    environment: RefCell<HashMap<String, i32>>,
    ast: RootAst,
}

impl Runtime {
    pub(crate) fn create(root_ast: RootAst) -> Self {
        Self {
            environment: RefCell::new(HashMap::new()),
            ast: root_ast,
        }
    }

    pub(crate) fn execute(&self) {
        for statement in &self.ast.statement {
            match statement {
                Statement::Print { expression } => {
                    println!("{value}", value = self.evaluate(expression).unwrap());
                }
                Statement::VariableDeclaration { identifier, expression } => {
                    // NOTE: please do not inline. it causes BorrowError.
                    let evaluated = self.evaluate(expression).expect("error happened during evaluating expression");
                    self.environment.borrow_mut().insert(identifier.clone(), evaluated);
                }
            }
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn evaluate(&self, expression: &Expression) -> Result<i32, String> {
        let eval_term = |term: &Term| {
            match term {
                Term::IntLiteral(inner) => *inner,
                Term::Variable {
                    name
                } => {
                    // temporary value
                    let read_view = self.environment.borrow();
                    let variable = read_view.get(name).expect("variable does not exist");
                    *variable
                }
            }
        };

        let result = match expression {
            Expression::BinaryPlus { lhs, rhs } => {
                self.evaluate(lhs)? + self.evaluate(rhs)?
            }
            Expression::WrappedTerm(term) => {
                eval_term(term)
            }
        };

        Ok(result)
    }
}
