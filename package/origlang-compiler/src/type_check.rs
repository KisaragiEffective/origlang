pub mod error;

use std::cell::RefCell;
use std::collections::HashMap;
use log::debug;
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_ast::{AtomicPattern, Identifier, RootAst, Statement, TypeSignature};
use origlang_typesystem_model::{AssignableQueryAnswer, Type, TypedExpression, TypedIntLiteral, TypedRootAst, TypedStatement};

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
                    BinaryOperatorKind::ShiftLeft | BinaryOperatorKind::ShiftRight => {
                        if lhs_type == rhs_type {
                            Ok(TypedExpression::BinaryOperator {
                                lhs: Box::new(lhs_expr),
                                rhs: Box::new(rhs_expr),
                                operator,
                                return_type: lhs_type.clone()
                            })
                        } else {
                            Err(TypeCheckError::UnableToUnifyEqualityQuery {
                                operator,
                                got_lhs: lhs_type,
                                got_rhs: rhs_type,
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
                    checked_intermediates.extend(checker.check(is)?);
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

fn helper(
    expr: TypedExpression, element_binding: &AtomicPattern, checker: &TypeChecker,
) -> Result<Vec<TypedStatement>, TypeCheckError> {
    match element_binding {
        AtomicPattern::Discard => {
            Ok(vec![TypedStatement::EvalAndForget {
                expression: expr,
            }])
        },
        AtomicPattern::Bind(identifier) => {
            checker.ctx.borrow_mut().add_known_variable(identifier.clone(), expr.actual_type());

            Ok(vec![TypedStatement::VariableDeclaration {
                identifier: identifier.clone(),
                expression: expr,
            }])
        }
        AtomicPattern::Tuple(tp) => {
            desugar(tp.clone(), expr, checker)
        }
    }
}

fn desugar(
    outer_destruction: Vec<AtomicPattern>, rhs: TypedExpression, checker: &TypeChecker,
) -> Result<Vec<TypedStatement>, TypeCheckError> {
    debug!("check: {outer_destruction:?}");
    debug!("check: {rhs:?}");

    match rhs {
        TypedExpression::Variable { ident, tp } => {
            match tp {
                Type::Tuple(tuple_element_types) => {
                    let tuple_element_types = tuple_element_types.0;
                    if outer_destruction.len() == tuple_element_types.len() {
                        let m = tuple_element_types.iter().enumerate().map(|(i, t)| {
                            let element_binding = &outer_destruction[i];

                            let t = Type::tuple(tuple_element_types.clone());
                            // TODO: this method is not great; easy to forget to call add_known_variable
                            checker.ctx.borrow_mut().add_known_variable(ident.clone(), t.clone());
                            let v = TypedExpression::Variable {
                                ident: ident.clone(),
                                tp: t
                            };

                            let expr = TypedExpression::ExtractTuple {
                                expr: Box::new(v),
                                index: i,
                            };

                            helper(expr, element_binding, checker)
                        }).collect::<Vec<Result<Vec<TypedStatement>, TypeCheckError>>>();

                        let mut k = vec![];

                        for mx in m {
                            match mx {
                                Ok(y) => {
                                    k.extend(y);
                                }
                                Err(x) => return Err(x)
                            }
                        }

                        Ok(k)
                    } else {
                        debug!("tuple arity mismatch");
                        Err(TypeCheckError::UnsatisfiablePattern {
                            pattern: AtomicPattern::Tuple(outer_destruction),
                            expression: TypedExpression::Variable { ident, tp: Type::tuple(tuple_element_types.clone()) },
                            expr_type: Type::tuple(tuple_element_types.clone()),
                        })
                    }
                }
                other => {
                    debug!("non-tuple expression");
                    Err(TypeCheckError::UnsatisfiablePattern {
                        pattern: AtomicPattern::Tuple(outer_destruction),
                        expression: TypedExpression::Variable { ident, tp: other.clone() },
                        expr_type: other,
                    })
                }
            }
        }
        TypedExpression::Block { inner, final_expression, return_type } => {
            // TODO: how can we handle inner statement?
            desugar(outer_destruction, *final_expression, checker)
        }
        TypedExpression::Tuple { expressions } => {
            let m = outer_destruction.into_iter().enumerate().map(|(i, element_binding)| {
                helper(expressions[i].clone(), &element_binding, checker)
            }).collect::<Vec<Result<Vec<TypedStatement>, TypeCheckError>>>();

            let mut k = vec![];

            for mx in m {
                match mx {
                    Ok(y) => {
                        k.extend(y);
                    }
                    Err(x) => return Err(x)
                }
            }

            Ok(k)
        }
        TypedExpression::ExtractTuple { expr, index } => {
            desugar(outer_destruction, *expr, checker)
        }
        other => {
            debug!("unsupported expression");
            Err(TypeCheckError::UnsatisfiablePattern {
                pattern: AtomicPattern::Tuple(outer_destruction),
                expr_type: other.actual_type(),
                expression: other,
            })
        }
    }
}

impl TryIntoTypeCheckedForm for Statement {
    type Success = Vec<TypedStatement>;
    type Err = TypeCheckError;

    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err> {
        match self {
            Self::Print { expression } => checker.check(expression).map(|e| vec![TypedStatement::Print { expression: e }]),
            Self::VariableDeclaration { pattern, expression, type_annotation } => {
                let checked = checker.check(expression)?;
                return if let Some(type_name) = type_annotation {
                    if let Ok(dest) = checker.lower_type_signature_into_type(&type_name) {
                        match dest.is_assignable(&checked.actual_type()) {
                            AssignableQueryAnswer::Yes => {
                                match pattern {
                                    AtomicPattern::Discard => {
                                        Ok(vec![TypedStatement::EvalAndForget { expression: checked }])
                                    }
                                    AtomicPattern::Bind(identifier) => {
                                        checker.ctx.borrow_mut().add_known_variable(identifier.clone(), checked.actual_type());
                                        Ok(vec![TypedStatement::VariableDeclaration {
                                            identifier,
                                            expression: checked,
                                        }])

                                    }
                                    AtomicPattern::Tuple(x) => desugar(x, checked, checker)
                                }
                            },
                            AssignableQueryAnswer::PossibleIfCoerceSourceImplicitly => {

                                Err(TypeCheckError::UnassignableType {
                                    from: checked.actual_type(),
                                    to: dest,
                                })
                            }
                            AssignableQueryAnswer::No => {
                                Err(TypeCheckError::UnassignableType {
                                    from: checked.actual_type(),
                                    to: dest,
                                })
                            }
                        }
                    } else {
                        Err(TypeCheckError::UnknownType {
                            name: type_name
                        })
                    }
                } else {
                    // no annotations, just set its type (type-inference) from the expr
                    match pattern {
                        AtomicPattern::Discard => {
                            Ok(vec![TypedStatement::EvalAndForget { expression: checked }])
                        }
                        AtomicPattern::Bind(identifier) => {
                            checker.ctx.borrow_mut().add_known_variable(identifier.clone(), checked.actual_type());
                            Ok(vec![TypedStatement::VariableDeclaration {
                                identifier,
                                expression: checked,
                            }])

                        }
                        AtomicPattern::Tuple(x) => desugar(x, checked, checker)
                    }
                }
            }
            Self::VariableAssignment { identifier, expression } => {
                let checked = checker.check(expression)?;
                let expected_type = checker.ctx.borrow().lookup_variable_type(&identifier)?;
                if checked.actual_type() == expected_type {
                    Ok(vec![TypedStatement::VariableAssignment {
                        identifier,
                        expression: checked,
                    }])
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
                    checked.extend(checker.check(inner_statement)?);
                }

                Ok(vec![TypedStatement::Block {
                    inner_statements: checked,
                }])
            }
            Self::Comment { .. } => Ok(vec![TypedStatement::Block {
                inner_statements: vec![]
            }]),
            Self::Exit => Ok(vec![TypedStatement::Exit]),
            Self::TypeAliasDeclaration { new_name, replace_with } => {
                checker.ctx.borrow_mut().known_aliases.insert(new_name, checker.lower_type_signature_into_type(&replace_with).map_err(|_| TypeCheckError::UnknownType {
                    name: replace_with,
                })?);

                // TODO: replace this with vec![]
                Ok(vec![TypedStatement::Empty])
            }
        }
    }
}

impl TryIntoTypeCheckedForm for RootAst {
    type Success = TypedRootAst;
    type Err = TypeCheckError;

    fn type_check(self, checker: &TypeChecker) -> Result<Self::Success, Self::Err> {
        let mut vec = Vec::with_capacity(self.statement.len());

        for x in self.statement {
            vec.extend(checker.check(x)?);
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
    pub(crate) fn lower_type_signature_into_type(&self, p0: &TypeSignature) -> Result<Type, ()> {
        match p0 {
            TypeSignature::Simple(ident) => {
                match ident.as_name() {
                    "Bool" => Ok(Type::Boolean),
                    "String" => Ok(Type::String),
                    "Unit" => Ok(Type::Unit),
                    "Int8" => Ok(Type::Int8),
                    "Int16" => Ok(Type::Int16),
                    "Int32" => Ok(Type::Int32),
                    "Int64" => Ok(Type::Int64),
                    _other => self.ctx.borrow().known_aliases.get(ident).cloned().ok_or(())
                }
            }
            TypeSignature::Tuple(x) => {
                let mut types = Vec::with_capacity(x.capacity());
                for ts in x {
                    types.push(self.lower_type_signature_into_type(ts)?);
                }

                Ok(Type::tuple(types))
            }
        }
    }
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
    known_typed_variables: HashMap<Identifier, Type>,
    known_aliases: HashMap<Identifier, Type>,
}

impl Context {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            known_typed_variables: HashMap::new(),
            known_aliases: HashMap::new(),
        }
    }

    fn lookup_variable_type(&self, variable_name: &Identifier) -> Result<Type, TypeCheckError> {
        self.known_typed_variables.get(variable_name).cloned().ok_or_else(|| TypeCheckError::UndefinedIdentifier(variable_name.clone()))
    }

    fn add_known_variable(&mut self, variable_ident: Identifier, tp: Type) {
        debug!("proved: {variable_ident}: {tp}");
        self.known_typed_variables.insert(variable_ident, tp);
    }
}
