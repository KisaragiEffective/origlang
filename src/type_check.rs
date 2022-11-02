use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use crate::ast::{RootAst, Statement};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Integer,
    Boolean,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => f.write_str("Int"),
            Self::Boolean => f.write_str("Bool"),
        }
    }
}

pub trait TypeCheckTarget {
    type Ok: Sized;
    type Err: Sized;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err>;
}

impl TypeCheckTarget for &Expression {
    type Ok = Type;
    type Err = String;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err> {
        match self {
            Expression::IntLiteral(_) => Ok(Type::Integer),
            Expression::BooleanLiteral(_) => Ok(Type::Boolean),
            Expression::Variable { ident } => {
                checker.ctx.borrow().lookup_variable_type(ident)
            },
            Expression::BinaryOperator { lhs, rhs, operator } => {
                // <---
                let lhs_type = checker.check(lhs.as_ref())?;
                let rhs_type = checker.check(rhs.as_ref())?;

                let tp = match operator {
                    BinaryOperatorKind::Plus => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Integer),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Minus => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Integer),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Multiply => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Integer),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Divide => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Integer),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::More => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::MoreEqual => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Less => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::LessEqual => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Boolean),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::ThreeWay => {
                        match (lhs_type, rhs_type) {
                            (Type::Integer, Type::Integer) => Ok(Type::Integer),
                            (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::Integer, *operator, Type::Integer, lhs_type, rhs_type))
                        }
                    }
                    BinaryOperatorKind::Equal => {
                        if lhs_type == rhs_type {
                            Ok(Type::Boolean)
                        } else {
                            Err(format!("Cannot compare (==) between different types. lhs: {lhs_type}, rhs: {rhs_type}"))
                        }
                    }
                    BinaryOperatorKind::NotEqual => {
                        if lhs_type == rhs_type {
                            Ok(Type::Boolean)
                        } else {
                            Err(format!("Cannot compare (!=) between different types. lhs: {lhs_type}, rhs: {rhs_type}"))
                        }
                    }
                };

                tp
            }
            Expression::If { condition, then_clause_value, else_clause_value } => {
                let cond_type = checker.check(condition.as_ref())?;
                let then_type = checker.check(then_clause_value.as_ref())?;
                let else_type = checker.check(else_clause_value.as_ref())?;

                if cond_type == Type::Boolean {
                    if then_type == else_type {
                        Ok(then_type)
                    } else {
                        Err(format!("Cannot unify two different types in if-expression. `then`: {then_type}, `else`: {else_type}"))
                    }
                } else {
                    Err(format!("The condition of if-expression must be Bool, but got {cond_type}"))
                }
            }
        }
    }
}

impl TypeCheckTarget for &Statement {
    type Ok = ();
    type Err = String;

    fn check(&self, checker: &TypeChecker) -> Result<Self::Ok, Self::Err> {
        match self {
            Statement::Print { expression } => checker.check(expression).map(|_| ()),
            Statement::VariableDeclaration { identifier, expression } => {
                let tp = checker.check(expression)?;
                checker.ctx.borrow_mut().add_variable_type(identifier.clone(), tp);
                Ok(())
            }
        }
    }
}

impl TypeCheckTarget for &RootAst {
    type Ok = ();
    type Err = String;

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
    fn invalid_combination_for_binary_operator(accepted_lhs: Type, operator: BinaryOperatorKind, accepted_rhs: Type, got_lhs: Type, got_rhs: Type) -> String {
        format!("Only ({accepted_lhs}) {operator} ({accepted_rhs}) is defined, but got {got_lhs} {operator} {got_rhs}")
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

    fn lookup_variable_type(&self, ident: &String) -> Result<Type, String> {
        self.map.get(ident).copied().ok_or(format!("{ident} is undefined"))
    }

    fn add_variable_type(&mut self, ident: String, tp: Type) {
        self.map.insert(ident, tp);
    }
}
