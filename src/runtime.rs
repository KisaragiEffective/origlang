use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use derive_more::{Display, From};
use crate::ast::{RootAst, Statement};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use crate::type_check::Type;

#[derive(PartialEq, Clone, Debug, Display, From)]
pub enum TypeBox {
    #[display(fmt = "{_0}")]
    #[from]
    Integer(i32),
    #[display(fmt = "{_0}")]
    #[from]
    Boolean(bool),
}

impl TypeBox {
    fn get_type(&self) -> Type {
        match self {
            TypeBox::Integer(_) => Type::Integer,
            TypeBox::Boolean(_) => Type::Boolean,
        }
    }

    fn as_int(&self) -> Result<i32, String> {
        match self {
            TypeBox::Integer(i) => Ok(*i),
            _ => Err("It is not i32".to_string())
        }
    }
}

pub struct Runtime {
    /// すでに評価された値を格納しておく
    environment: RefCell<HashMap<String, TypeBox>>,
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

    pub(crate) fn yield_all_evaluated_expressions(&self, ast: &RootAst) -> Vec<TypeBox> {
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



type EvaluateResult = Result<TypeBox, String>;
trait CanBeEvaluated {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult;
}

impl CanBeEvaluated for &Expression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            Expression::IntLiteral(i) => Ok((*i).into()),
            Expression::BooleanLiteral(b) => Ok((*b).into()),
            Expression::Variable { ident } => {
                runtime.environment.borrow().get(ident).ok_or(format!("variable {ident} is not defined")).map(|a| a.clone())
            },
            Expression::BinaryOperator { lhs, rhs, operator } => {
                let lhs = lhs.as_ref().evaluate(runtime)?;
                let rhs = rhs.as_ref().evaluate(runtime)?;
                let value = match operator {
                    BinaryOperatorKind::Plus => (lhs.as_int()? + rhs.as_int()?).into(),
                    BinaryOperatorKind::Minus => (lhs.as_int()? - rhs.as_int()?).into(),
                    BinaryOperatorKind::Multiply => (lhs.as_int()? * rhs.as_int()?).into(),
                    BinaryOperatorKind::Divide => (lhs.as_int()? / rhs.as_int()?).into(),
                    BinaryOperatorKind::More => (lhs.as_int()? > rhs.as_int()?).into(),
                    BinaryOperatorKind::MoreEqual => (lhs.as_int()? >= rhs.as_int()?).into(),
                    BinaryOperatorKind::Less => (lhs.as_int()? < rhs.as_int()?).into(),
                    BinaryOperatorKind::LessEqual => (lhs.as_int()? <= rhs.as_int()?).into(),
                    BinaryOperatorKind::ThreeWay => {
                        match lhs.as_int()?.cmp(&rhs.as_int()?) {
                            Ordering::Less => -1,
                            Ordering::Equal => 0,
                            Ordering::Greater => 1,
                        }
                    }.into(),
                    BinaryOperatorKind::Equal => (lhs == rhs).into(),
                    BinaryOperatorKind::NotEqual => (lhs != rhs).into(),
                };

                Ok(value)
            }
            Expression::If { condition, then_clause_value, else_clause_value } => {
                let ret = condition.as_ref().evaluate(runtime)?;
                if let TypeBox::Boolean(b) = ret {
                    if b {
                        then_clause_value.as_ref().evaluate(runtime)
                    } else {
                        else_clause_value.as_ref().evaluate(runtime)
                    }
                } else {
                    Err(format!("if cond must be a boolean"))
                }
            }
        }
    }
}
