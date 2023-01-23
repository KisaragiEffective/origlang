pub mod error;

use std::cell::RefCell;
use std::collections::HashMap;
use derive_more::Display;
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use crate::ast::{RootAst, Statement};

use crate::type_check::error::TypeCheckError;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Display)]
pub enum Type {
    #[display(fmt = "{{integer}}")]
    GenericInteger,
    #[display(fmt = "Bool")]
    Boolean,
    #[display(fmt = "String")]
    String,
    #[display(fmt = "Unit")]
    Unit,
    #[display(fmt = "Int8")]
    Int8,
    #[display(fmt = "Int16")]
    Int16,
    #[display(fmt = "Int32")]
    Int32,
    #[display(fmt = "Int64")]
    Int64,
}

impl Type {
    const fn is_int_family(&self) -> bool {
        matches!(self, Self::GenericInteger | Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }
}

pub trait TypeCheckTarget {
    type Ok: Sized;
    type Err: Sized;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err>;
}

impl TypeCheckTarget for &Expression {
    type Ok = Type;
    type Err = TypeCheckError;

    #[allow(clippy::too_many_lines)]
    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err> {
        match self {
            Expression::IntLiteral { suffix, .. } => suffix.as_ref().map_or_else(
                || Ok(Type::GenericInteger),
                |s| match s.as_ref() {
                    "i8"  => Ok(Type::Int8),
                    "i16" => Ok(Type::Int16),
                    "i32" => Ok(Type::Int32),
                    "i64" => Ok(Type::Int64),
                    _ => unreachable!()
                }
            ),
            Expression::BooleanLiteral(_) => Ok(Type::Boolean),
            Expression::StringLiteral(_) => Ok(Type::String),
            Expression::UnitLiteral => Ok(Type::Unit),
            Expression::Variable { ident } => {
                checker.ctx.borrow().lookup_variable_type(ident)
            },
            Expression::BinaryOperator { lhs, rhs, operator } => {
                // <---
                let lhs_type = checker.check(lhs.as_ref())?;
                let rhs_type = checker.check(rhs.as_ref())?;

                

                match operator {
                    BinaryOperatorKind::Plus => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::GenericInteger),
                            (Type::String, Type::String) => Ok(Type::String),
                            (x, y) if x == y && x.is_int_family() => Ok(x),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Minus => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::GenericInteger),
                            (x, y) if x == y && x.is_int_family() => Ok(x),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Multiply => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::GenericInteger),
                            (x, y) if x == y && x.is_int_family() => Ok(x),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Divide => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::GenericInteger),
                            (x, y) if x == y && x.is_int_family() => Ok(x),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::More => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::Boolean),
                            (x, y) if x == y && x.is_int_family() => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::MoreEqual => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::Boolean),
                            (x, y) if x == y && x.is_int_family() => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Less => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::Boolean),
                            (x, y) if x == y && x.is_int_family() => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::LessEqual => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::Boolean),
                            (x, y) if x == y && x.is_int_family() => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::ThreeWay => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(Type::GenericInteger),
                            (x, y) if x == y && x.is_int_family() => Ok(x),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, *operator, Type::GenericInteger, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Equal => {
                        if lhs_type == rhs_type {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeCheckError::UnableToUnifyEqualityQuery {
                                operator: BinaryOperatorKind::Equal,
                                got_lhs: lhs_type,
                                got_rhs: rhs_type
                            })
                        }
                    }
                    BinaryOperatorKind::NotEqual => {
                        if lhs_type == rhs_type {
                            Ok(Type::Boolean)
                        } else {
                            Err(TypeCheckError::UnableToUnifyEqualityQuery {
                                operator: BinaryOperatorKind::NotEqual,
                                got_lhs: lhs_type,
                                got_rhs: rhs_type
                            })
                        }
                    }
                }
            }
            Expression::If { condition, then_clause_value, else_clause_value } => {
                let cond_type = checker.check(condition.as_ref())?;
                let then_type = checker.check(then_clause_value.as_ref())?;
                let else_type = checker.check(else_clause_value.as_ref())?;

                if cond_type == Type::Boolean {
                    if then_type == else_type {
                        Ok(then_type)
                    } else {
                        Err(TypeCheckError::UnableToUnityIfExpression {
                            then_clause_type: then_type,
                            else_clause_type: else_type
                        })
                    }
                } else {
                    Err(TypeCheckError::GenericTypeMismatch {
                        context: "The condition of if-expression".to_string(),
                        expected_type: Type::Boolean,
                        actual_type: cond_type,
                    })
                }
            }
        }
    }
}

impl TypeCheckTarget for &Statement {
    type Ok = ();
    type Err = TypeCheckError;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err> {
        match self {
            Statement::Print { expression } => checker.check(expression).map(|_| ()),
            Statement::VariableDeclaration { identifier, expression } => {
                let tp = checker.check(expression)?;
                checker.ctx.borrow_mut().add_variable_type(identifier.clone(), tp);
                Ok(())
            }
            Statement::VariableAssignment { identifier, expression } => {
                let actual_type = checker.check(expression)?;
                let expected_type = checker.ctx.borrow().lookup_variable_type(identifier)?;
                if actual_type == expected_type {
                    Ok(())
                } else {
                    Err(TypeCheckError::GenericTypeMismatch {
                        context: "variable assignment".to_string(),
                        expected_type,
                        actual_type,
                    })
                }
            }
        }
    }
}

impl TypeCheckTarget for &RootAst {
    type Ok = ();
    type Err = TypeCheckError;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err> {
        for x in &self.statement {
            checker.check(x)?;
        }
        Ok(())
    }
}

pub struct TypeChecker {
    ctx: Box<RefCell<TypeCheckContext>>
}

impl TypeChecker {
    const fn invalid_combination_for_binary_operator(accepted_lhs: Type, operator: BinaryOperatorKind, accepted_rhs: Type, got_lhs: Type, got_rhs: Type) -> TypeCheckError {
        TypeCheckError::InvalidCombinationForBinaryOperator {
            accepted_lhs,
            operator,
            accepted_rhs,
            got_lhs,
            got_rhs
        }
    }

    pub fn new() -> Self {
        Self {
            ctx: Box::new(RefCell::new(TypeCheckContext::empty())),
        }
    }

    pub fn check<T: TypeCheckTarget>(&self, t: T) -> Result<T::Ok, T::Err> {
        t.check(self)
    }
}

pub struct TypeCheckContext {
    map: HashMap<String, Type>,
}

impl TypeCheckContext {
    pub fn empty() -> Self {
        Self {
            map: HashMap::new()
        }
    }

    fn lookup_variable_type(&self, ident: &String) -> Result<Type, TypeCheckError> {
        self.map.get(ident).copied().ok_or_else(|| TypeCheckError::UndefinedIdentifier(ident.clone()))
    }

    fn add_variable_type(&mut self, ident: String, tp: Type) {
        self.map.insert(ident, tp);
    }
}
