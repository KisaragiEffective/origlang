use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use crate::ast::{RootAst, Statement};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};

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

    fn update_variable(&self, identifier: &str, expression: &Expression) {
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

impl CanBeEvaluated for &Expression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            Expression::IntLiteral(i) => Ok(*i),
            Expression::BooleanLiteral(b) => Ok(zero_one_bool(*b)),
            Expression::Variable { ident } => {
                runtime.environment.borrow().get(ident).ok_or(format!("variable {ident} is not defined")).map(|a| *a)
            },
            Expression::BinaryOperator { lhs, rhs, operator } => {
                let lhs = lhs.as_ref().evaluate(runtime)?;
                let rhs = rhs.as_ref().evaluate(runtime)?;
                let value = match operator {
                    BinaryOperatorKind::Plus => lhs + rhs,
                    BinaryOperatorKind::Minus => lhs - rhs,
                    BinaryOperatorKind::Multiply => lhs * rhs,
                    BinaryOperatorKind::Divide => lhs / rhs,
                    BinaryOperatorKind::More => zero_one_bool(lhs > rhs),
                    BinaryOperatorKind::MoreEqual => zero_one_bool(lhs >= rhs),
                    BinaryOperatorKind::Less => zero_one_bool(lhs < rhs),
                    BinaryOperatorKind::LessEqual => zero_one_bool(lhs <= rhs),
                    BinaryOperatorKind::ThreeWay => {
                        match lhs.cmp(&rhs) {
                            Ordering::Less => -1,
                            Ordering::Equal => 0,
                            Ordering::Greater => 1,
                        }
                    }
                    BinaryOperatorKind::Equal => zero_one_bool(lhs == rhs),
                    BinaryOperatorKind::NotEqual => zero_one_bool(lhs != rhs),
                };

                Ok(value)
            }
            Expression::If { condition, then_clause_value, else_clause_value } => {
                if condition.as_ref().evaluate(runtime)? == 1 { // FIXME this should be `true` in the future
                    then_clause_value.as_ref().evaluate(runtime)
                } else {
                    else_clause_value.as_ref().evaluate(runtime)
                }
            }
        }
    }
}

#[inline]
const fn zero_one_bool(b: bool) -> i32 {
    b as i32
}
