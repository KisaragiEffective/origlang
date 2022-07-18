use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Add, Deref};
use crate::{First, RootAst, Statement};
use crate::ast::{BuiltinOperatorKind, Additive, Multiplicative, AdditiveOperatorKind, MultiplicativeOperatorKind};

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

    #[allow(dead_code)]
    pub(crate) fn execute(&self) {
        for statement in &self.ast.statement {
            match statement {
                Statement::Print { expression } => {
                    println!("{value}", value = self.evaluate(expression).unwrap());
                }
                Statement::VariableDeclaration { identifier, expression } => {
                    self.update_variable(identifier.as_str(), expression);
                }
            }
        }
    }

    pub(crate) fn yield_all_evaluated_expressions(&self) -> Vec<i32> {
        let mut buf = vec![];
        for statement in &self.ast.statement {
            match statement {
                Statement::Print { expression } => {
                    buf.push(self.evaluate(expression).unwrap());
                }
                Statement::VariableDeclaration { identifier, expression } => {
                    self.update_variable(identifier.as_str(), expression);
                }
            }
        }
        buf
    }

    fn update_variable(&self, identifier: &str, expression: &Additive) {
        // NOTE: please do not inline. it causes BorrowError.
        let evaluated = self.evaluate(expression).expect("error happened during evaluating expression");
        self.environment.borrow_mut().insert(identifier.to_string(), evaluated);
    }

    fn evaluate<E: CanBeEvaluated>(&self, expression: E) -> EvaluateResult {
        expression.evaluate(self)
    }
}



type EvaluateResult = Result<i32, String>;
trait CanBeEvaluated {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult;
}

impl CanBeEvaluated for &Additive {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            Additive::Binary { operator, lhs, rhs } => {
                Ok(match operator {
                    AdditiveOperatorKind::Plus => lhs.as_ref().evaluate(runtime)? + rhs.as_ref().evaluate(runtime)?,
                    AdditiveOperatorKind::Minus => lhs.as_ref().evaluate(runtime)? - rhs.as_ref().evaluate(runtime)?,
                })
            }
            Additive::WrappedMultiplicative(term) => {
                term.evaluate(runtime)
            }
        }
    }
}

impl CanBeEvaluated for &Multiplicative {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            Multiplicative::Binary { operator, lhs, rhs } => {
                Ok(match operator {
                    MultiplicativeOperatorKind::Multiple => {
                        lhs.as_ref().evaluate(runtime)? * rhs.as_ref().evaluate(runtime)?
                    }
                    MultiplicativeOperatorKind::Divide => {
                        lhs.as_ref().evaluate(runtime)? / rhs.as_ref().evaluate(runtime)?
                    }
                })
            }
            Multiplicative::WrappedFirst(term) => {
                term.evaluate(runtime)
            }
        }
    }
}

impl CanBeEvaluated for &First {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            First::IntLiteral(inner) => Ok(*inner),
            First::Variable {
                name
            } => {
                // temporary value
                let read_view = runtime.environment.borrow();
                let variable = read_view.get(name.as_str()).expect("variable does not exist");
                Ok(*variable)
            }
            First::Parenthesized(inner) => {
                inner.as_ref().evaluate(runtime)
            }
        }
    }
}