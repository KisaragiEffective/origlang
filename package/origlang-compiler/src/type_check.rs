pub mod error;

use std::cell::RefCell;
use std::collections::HashMap;
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_ast::{Identifier, RootAst, Statement};
use origlang_typesystem_model::{Type, TypedExpression, TypedIntLiteral, TypedRootAst, TypedStatement};

use crate::type_check::error::TypeCheckError;

pub trait TryIntoTypeCheckedForm {
    type Success: Sized;
    type Err: Sized;

    /// # Errors
    /// 型チェックにしっぱいした場合はErr
    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err>;
}

impl TryIntoTypeCheckedForm for Expression {
    type Success = TypedExpression;
    type Err = TypeCheckError;

    #[allow(clippy::too_many_lines)]
    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err> {
        match self {
            Self::IntLiteral { value, suffix } => {
                let lit = suffix.as_ref().map_or_else(
                    || TypedIntLiteral::Generic(value),
                    |s| match &**s {
                        "i8"  => TypedIntLiteral::Bit8(value as i8),
                        "i16" => TypedIntLiteral::Bit16(value as i16),
                        "i32" => TypedIntLiteral::Bit32(value as i32),
                        "i64" => TypedIntLiteral::Bit64(value),
                        _ => unreachable!()
                    }
                );

                Ok(TypedExpression::IntLiteral(lit))
            },
            Self::BooleanLiteral(v) => Ok(TypedExpression::BooleanLiteral(v)),
            Self::StringLiteral(v) => Ok(TypedExpression::StringLiteral(v)),
            Self::UnitLiteral => Ok(TypedExpression::UnitLiteral),
            Self::Variable { ident } => {
                let tp = checker.ctx.borrow().lookup_variable_type(&ident)?;
                Ok(TypedExpression::Variable { ident, tp })
            },
            Self::BinaryOperator { lhs, rhs, operator } => {
                // <---
                let lhs_expr = checker.check(*lhs)?;
                let rhs_expr = checker.check(*rhs)?;
                let lhs_type = lhs_expr.actual_type();
                let rhs_type = rhs_expr.actual_type();

                match operator {
                    BinaryOperatorKind::Plus => {
                        match (lhs_type, rhs_type) {
                            (Type::GenericInteger, Type::GenericInteger) => Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: Type::GenericInteger,
                            }),
                            (Type::String, Type::String) => Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: Type::String
                            }),
                            (x, y) if x == y && x.is_int_family() => Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: x
                            }),
                            (other_lhs, other_rhs) =>
                                Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, operator, Type::GenericInteger, other_lhs, other_rhs))
                        }
                    }
                    | BinaryOperatorKind::Minus
                    | BinaryOperatorKind::Multiply
                    | BinaryOperatorKind::Divide
                    | BinaryOperatorKind::ThreeWay => {
                        {
                            match (&lhs_type, &rhs_type) {
                                (Type::GenericInteger, Type::GenericInteger) => Ok(TypedExpression::BinaryOperator {
                                    lhs: Box::new(lhs_expr),
                                    rhs: Box::new(rhs_expr),
                                    operator,
                                    return_type: Type::GenericInteger,
                                }),
                                (x, y) if x == y && x.is_int_family() => Ok(TypedExpression::BinaryOperator {
                                    lhs: Box::new(lhs_expr),
                                    rhs: Box::new(rhs_expr),
                                    operator,
                                    // it is effectively Copy
                                    return_type: x.clone(),
                                }),
                                (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, operator, Type::GenericInteger, lhs_type.clone(), rhs_type.clone()))
                            }
                        }
                    }
                    | BinaryOperatorKind::More
                    | BinaryOperatorKind::MoreEqual
                    | BinaryOperatorKind::Less
                    | BinaryOperatorKind::LessEqual => {
                        {
                            match (&lhs_type, &rhs_type) {
                                (Type::GenericInteger, Type::GenericInteger) => Ok(TypedExpression::BinaryOperator {
                                    lhs: Box::new(lhs_expr),
                                    rhs: Box::new(rhs_expr),
                                    operator,
                                    return_type: Type::GenericInteger,
                                }),
                                (x, y) if x == y && x.is_int_family() => Ok(TypedExpression::BinaryOperator {
                                    lhs: Box::new(lhs_expr),
                                    rhs: Box::new(rhs_expr),
                                    operator,
                                    return_type: x.clone()
                                }),
                                (_, _) => Err(TypeChecker::invalid_combination_for_binary_operator(Type::GenericInteger, operator, Type::GenericInteger, lhs_type.clone(), rhs_type.clone()))
                            }
                        }
                    }
                    BinaryOperatorKind::Equal => {
                        if lhs_type == rhs_type {
                            Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: Type::Boolean,
                            })
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
                            Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: Type::Boolean,
                            })
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
            Self::If { condition, then_clause_value, else_clause_value } => {
                let cond_expr = checker.check(*condition)?;
                let then_expr = checker.check(*then_clause_value)?;
                let else_expr = checker.check(*else_clause_value)?;
                let cond_type = cond_expr.actual_type();
                let then_type = then_expr.actual_type();
                let else_type = else_expr.actual_type();

                if cond_type == Type::Boolean {
                    if then_type == else_type {
                        Ok(TypedExpression::If {
                            condition: Box::new(cond_expr),
                            then: Box::new(then_expr),
                            els: Box::new(else_expr),
                            return_type: then_type,
                        })
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
            Self::Block { intermediate_statements, final_expression } => {
                // Please don't ignore intermediate statements.
                // TODO: add test for it
                let mut checked_intermediates = Vec::with_capacity(intermediate_statements.len());

                for is in intermediate_statements {
                    checked_intermediates.push(checker.check(is)?);
                }

                let checked_final = checker.check(*final_expression)?;

                Ok(TypedExpression::Block {
                    inner: checked_intermediates,
                    return_type: checked_final.actual_type(),
                    final_expression: Box::new(checked_final),
                })
            }
            Self::Tuple { expressions } => {
                let mut checked_expressions = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    checked_expressions.push(checker.check(expr)?);
                }

                Ok(TypedExpression::Tuple { expressions: checked_expressions })
            }
        }
    }
}

impl TryIntoTypeCheckedForm for Statement {
    type Success = TypedStatement;
    type Err = TypeCheckError;

    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err> {
        match self {
            Self::Print { expression } => checker.check(expression).map(|e| TypedStatement::Print { expression: e }),
            Self::VariableDeclaration { identifier, expression } => {
                let checked = checker.check(expression)?;
                checker.ctx.borrow_mut().add_variable_type(identifier.clone(), checked.actual_type());
                Ok(TypedStatement::VariableDeclaration {
                    identifier,
                    expression: checked,
                })
            }
            Self::VariableAssignment { identifier, expression } => {
                let checked = checker.check(expression)?;
                let expected_type = checker.ctx.borrow().lookup_variable_type(&identifier)?;
                if checked.actual_type() == expected_type {
                    Ok(TypedStatement::VariableAssignment {
                        identifier,
                        expression: checked,
                    })
                } else {
                    Err(TypeCheckError::GenericTypeMismatch {
                        context: "variable assignment".to_string(),
                        expected_type,
                        actual_type: checked.actual_type(),
                    })
                }
            }
            Self::Block { inner_statements } => {
                let mut checked = Vec::with_capacity(inner_statements.len());
                for inner_statement in inner_statements {
                    checked.push(checker.check(inner_statement)?);
                }

                Ok(TypedStatement::Block {
                    inner_statements: checked,
                })
            }
            Self::Comment { .. } => Ok(TypedStatement::Block {
                inner_statements: vec![]
            }),
        }
    }
}

impl TryIntoTypeCheckedForm for RootAst {
    type Success = TypedRootAst;
    type Err = TypeCheckError;

    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err> {
        let mut vec = Vec::with_capacity(self.statement.len());

        for x in self.statement {
            vec.push(checker.check(x)?);
        }

        Ok(TypedRootAst {
            statements: vec,
        })
    }
}

pub struct TypeChecker {
    ctx: Box<RefCell<Context>>
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

    #[must_use]
    pub fn new() -> Self {
        Self {
            ctx: Box::new(RefCell::new(Context::empty())),
        }
    }

    /// # Errors
    /// 型チェックが失敗した場合はErr
    pub fn check<T: TryIntoTypeCheckedForm>(&self, t: T) -> Result<T::Success, T::Err> {
        t.type_check(self)
    }
}

pub struct Context {
    typed_variables: HashMap<Identifier, Type>,
}

impl Context {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            typed_variables: HashMap::new()
        }
    }

    fn lookup_variable_type(&self, ident: &Identifier) -> Result<Type, TypeCheckError> {
        self.typed_variables.get(ident).cloned().ok_or_else(|| TypeCheckError::UndefinedIdentifier(ident.clone()))
    }

    fn add_variable_type(&mut self, ident: Identifier, tp: Type) {
        self.typed_variables.insert(ident, tp);
    }
}
