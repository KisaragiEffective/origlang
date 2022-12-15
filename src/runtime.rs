use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use derive_more::{Display, From};
use tap::Conv;
use crate::ast::{RootAst, Statement};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use crate::type_check::Type;

#[derive(From)]
pub struct Coerced(i64);

impl From<Coerced> for TypeBox {
    fn from(value: Coerced) -> Self {
        Self::Int64(value.0)
    }
}

#[derive(From)]
pub struct NonCoerced(i64);

impl From<NonCoerced> for TypeBox {
    fn from(value: NonCoerced) -> Self {
        Self::NonCoercedInteger(value.0)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Display, From)]
pub enum TypeBox {
    #[display(fmt = "{_0}")]
    NonCoercedInteger(i64),
    #[display(fmt = "{_0}")]
    #[from]
    Int8(i8),
    #[display(fmt = "{_0}")]
    #[from]
    Int16(i16),
    #[display(fmt = "{_0}")]
    #[from]
    Int32(i32),
    #[display(fmt = "{_0}")]
    Int64(i64),
    #[display(fmt = "{_0}")]
    #[from]
    Boolean(bool),
    #[display(fmt = "{_0}")]
    #[from]
    String(String),
    #[display(fmt = "()")]
    #[from]
    Unit(())
}

impl TypeBox {
    const fn get_type(&self) -> Type {
        match self {
            Self::NonCoercedInteger(_) => Type::GenericInteger,
            Self::Boolean(_) => Type::Boolean,
            Self::String(_) => Type::String,
            Self::Unit(_) => Type::Unit,
            Self::Int8(_) => Type::Int8,
            Self::Int16(_) => Type::Int16,
            Self::Int32(_) => Type::Int32,
            Self::Int64(_) => Type::Int64,
        }
    }

    fn as_int(&self) -> Result<i64, String> {
        match self {
            Self::NonCoercedInteger(i) => Ok(*i),
            _ => Err("It is not i64".to_string())
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

macro_rules! f {
    ($lhs:ident, $operator:expr, $rhs:ident) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let ret = match $operator {
            BinaryOperatorKind::Plus => (lhs.wrapping_add(rhs)).into(),
            BinaryOperatorKind::Minus => (lhs.wrapping_sub(rhs)).into(),
            BinaryOperatorKind::Multiply => (lhs.wrapping_mul(rhs)).into(),
            BinaryOperatorKind::Divide => (lhs.wrapping_div(rhs)).into(),
            BinaryOperatorKind::More => (lhs > rhs).into(),
            BinaryOperatorKind::MoreEqual => (lhs >= rhs).into(),
            BinaryOperatorKind::Less => (lhs < rhs).into(),
            BinaryOperatorKind::LessEqual => (lhs <= rhs).into(),
            BinaryOperatorKind::ThreeWay => {
                match lhs.cmp(&rhs) {
                    Ordering::Less => -1,
                    Ordering::Equal => 0,
                    Ordering::Greater => 1,
                }
            }.into(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => unreachable!()
        };

        Ok(ret)
    }};
    ($lhs:ident, $operator:expr, $rhs:ident as $intermediate:ty) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let ret = match $operator {
            BinaryOperatorKind::Plus => (lhs.wrapping_add(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Minus => (lhs.wrapping_sub(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Multiply => (lhs.wrapping_mul(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Divide => (lhs.wrapping_div(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::More => (lhs > rhs).into(),
            BinaryOperatorKind::MoreEqual => (lhs >= rhs).into(),
            BinaryOperatorKind::Less => (lhs < rhs).into(),
            BinaryOperatorKind::LessEqual => (lhs <= rhs).into(),
            BinaryOperatorKind::ThreeWay => {
                match lhs.cmp(&rhs) {
                    Ordering::Less => -1,
                    Ordering::Equal => 0,
                    Ordering::Greater => 1,
                }
            }.into(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => unreachable!()
        };

        Ok(ret)
    }};
}

impl CanBeEvaluated for &Expression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            Expression::IntLiteral { value: i, suffix } => if let Some(suffix) = suffix {
                match suffix.as_ref() {
                    "i8" => Ok(((*i) as i8).into()),
                    "i16" => Ok(((*i) as i16).into()),
                    "i32" => Ok(((*i) as i32).into()),
                    "i64" => Ok((*i).conv::<Coerced>().into()),
                    _ => unreachable!()
                }
            } else {
                Ok((*i).conv::<NonCoerced>().into())
            },
            Expression::BooleanLiteral(b) => Ok((*b).into()),
            Expression::StringLiteral(s) => Ok(s.clone().into()),
            Expression::UnitLiteral => Ok(().into()),
            Expression::Variable { ident } => {
                runtime.environment.borrow().get(ident).ok_or(format!("variable {ident} is not defined")).map(Clone::clone)
            },
            Expression::BinaryOperator { lhs, rhs, operator } => {
                let lhs = lhs.as_ref().evaluate(runtime)?;
                let rhs = rhs.as_ref().evaluate(runtime)?;
                if matches!(operator, BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual) {
                    return if lhs.get_type() == rhs.get_type() {
                        let ret = match operator {
                            BinaryOperatorKind::Equal => lhs == rhs,
                            BinaryOperatorKind::NotEqual => lhs != rhs,
                            _ => unreachable!(),
                        };
                        Ok(ret.into())
                    } else {
                        Err("Cannot compare between different types.".to_string())
                    }
                }

                return match (lhs, rhs) {
                    (TypeBox::NonCoercedInteger(lhs), TypeBox::NonCoercedInteger(rhs)) => {
                        f!(lhs, operator, rhs as NonCoerced)
                    },
                    (TypeBox::Int8(lhs), TypeBox::Int8(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int16(lhs), TypeBox::Int16(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int32(lhs), TypeBox::Int32(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int64(lhs), TypeBox::Int64(rhs)) => {
                        f!(lhs, operator, rhs as Coerced)
                    },
                    (TypeBox::String(lhs), TypeBox::String(rhs)) => {
                        let mut ret = lhs;
                        // give hint to compiler
                        ret.reserve_exact(rhs.len());
                        ret += rhs.as_str();
                        Ok(ret.into())
                    }
                    _ => Err("None of them are applicable".to_string())
                };
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
                    Err("if cond must be a boolean".to_string())
                }
            }
        }
    }
}
