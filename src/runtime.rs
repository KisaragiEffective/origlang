use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use crate::ast::{EqualityExpression, EqualityExpressionOperator, ExpressionBox, First, RelationExpression, RelationExpressionOperator, RootAst, Statement};
use crate::ast::{Additive, Multiplicative, AdditiveOperatorKind, MultiplicativeOperatorKind};

pub struct Runtime {
    /// すでに評価された値を格納しておく
    environment: RefCell<HashMap<String, i32>>,
}

impl Runtime {
    pub(crate) fn create() -> Self {
        Self {
            environment: RefCell::new(HashMap::new()),
        }
    }

    #[allow(dead_code)]
    pub(crate) fn execute(&self, ast: &RootAst) {
        for statement in &ast.statement {
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

    pub(crate) fn yield_all_evaluated_expressions(&self, ast: &RootAst) -> Vec<i32> {
        let mut buf = vec![];
        for statement in &ast.statement {
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
            Additive::Unlifted(term) => {
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
            Multiplicative::Unlifted(term) => {
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
            First::Lifted(inner) => {
                inner.as_ref().evaluate(runtime)
            }
            // FIXME: 簡単な言語なのでここでは簡便さを優先
            First::True => Ok(1),
            First::False => Ok(0),
        }
    }
}

#[inline]
const fn zero_one_bool(b: bool) -> i32 {
    if b {
        1
    } else {
        0
    }
}

impl CanBeEvaluated for &RelationExpression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            RelationExpression::Binary { operator, lhs, rhs } => {
                let v = match operator {
                    RelationExpressionOperator::LessEqual => {
                        zero_one_bool(lhs.as_ref().evaluate(runtime)? <= rhs.as_ref().evaluate(runtime)?)
                    }
                    RelationExpressionOperator::Less => {
                        zero_one_bool(lhs.as_ref().evaluate(runtime)? < rhs.as_ref().evaluate(runtime)?)
                    }
                    RelationExpressionOperator::MoreEqual => {
                        zero_one_bool(lhs.as_ref().evaluate(runtime)? >= rhs.as_ref().evaluate(runtime)?)
                    }
                    RelationExpressionOperator::More => {
                        zero_one_bool(lhs.as_ref().evaluate(runtime)? > rhs.as_ref().evaluate(runtime)?)
                    }
                    RelationExpressionOperator::SpaceShip => {
                        match lhs.as_ref().evaluate(runtime)?.cmp(&rhs.as_ref().evaluate(runtime)?) {
                            Ordering::Less => -1,
                            Ordering::Equal => 0,
                            Ordering::Greater => 1,
                        }
                    }
                };
                Ok(v)
            }
            RelationExpression::Unlifted(a) => a.evaluate(runtime)
        }
    }
}

impl CanBeEvaluated for &EqualityExpression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            EqualityExpression::Binary { operator, lhs, rhs } => {
                let v = match operator {
                    EqualityExpressionOperator::Equal => {
                        lhs.as_ref().evaluate(runtime)? == rhs.as_ref().evaluate(runtime)?
                    }
                    EqualityExpressionOperator::NotEqual => {
                        lhs.as_ref().evaluate(runtime)? != rhs.as_ref().evaluate(runtime)?
                    }
                };

                Ok(zero_one_bool(v))
            }
            EqualityExpression::Unlifted(a) => a.evaluate(runtime)
        }
    }
}
impl CanBeEvaluated for &ExpressionBox {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            ExpressionBox::Unary(a) => a.evaluate(runtime),
            ExpressionBox::Additive(a) => a.evaluate(runtime),
            ExpressionBox::Multiplicative(a) => a.evaluate(runtime),
            ExpressionBox::RelationExpression(a) => a.evaluate(runtime),
            ExpressionBox::EqualityExpression(a) => a.evaluate(runtime),
        }
    }
}
