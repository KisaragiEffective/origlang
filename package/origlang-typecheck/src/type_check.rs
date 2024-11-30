pub mod error;


use std::cell::RefCell;
use std::collections::hash_map::RandomState;
use std::collections::{HashMap, VecDeque};
use std::hash::BuildHasher;
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

    #[expect(clippy::too_many_lines, clippy::cast_possible_truncation)]
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
                    | BinaryOperatorKind::ThreeWay
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
                                    // it is effectively Copy
                                    return_type: x.clone(),
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
                            Err(TypeCheckError::UnableToUnifyBinaryOperatorOutputType {
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
                            Err(TypeCheckError::UnableToUnifyBinaryOperatorOutputType {
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
                                return_type: lhs_type
                            })
                        } else {
                            Err(TypeCheckError::UnableToUnifyBinaryOperatorOutputType {
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

fn handle_atomic_pattern(
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
            desugar_recursive_pattern_match(tp, expr, checker)
        }
    }
}

enum RecursivePatternMatchDesugarStrategy {
    Simple(TypedExpression),
    InsertTemporary(TypedExpression),
}

fn desugar_recursive_pattern_match(
    outer_destruction: &[AtomicPattern], mut rhs: TypedExpression, checker: &TypeChecker,
) -> Result<Vec<TypedStatement>, TypeCheckError> {
    // this is intentionally written in loop way because deep recursion causes stack-overflow during type-checking.
    loop {
        debug!("check: {outer_destruction:?} = {rhs:?}");

        match rhs {
            TypedExpression::Variable { ident, tp } => {
                let Type::Tuple(tuple_element_types) = tp else {
                    debug!("non-tuple expression");
                    break Err(TypeCheckError::UnsatisfiablePattern {
                        pattern: AtomicPattern::Tuple(outer_destruction.to_vec()),
                        expression: TypedExpression::Variable { ident, tp: tp.clone() },
                        expr_type: tp,
                    })
                };

                let tuple_element_types = tuple_element_types.0;
                if outer_destruction.len() == tuple_element_types.len() {
                    rhs = TypedExpression::Tuple {
                        expressions: tuple_element_types.iter().enumerate().map(|(i, _)| TypedExpression::ExtractTuple {
                            expr: Box::new(
                                TypedExpression::Variable { ident: ident.clone(), tp: Type::tuple(tuple_element_types.clone()) }
                            ),
                            index: i,
                        }).collect(),
                    };
                    continue
                }

                debug!("tuple arity mismatch");
                break Err(TypeCheckError::UnsatisfiablePattern {
                    pattern: AtomicPattern::Tuple(outer_destruction.to_vec()),
                    expression: TypedExpression::Variable { ident, tp: Type::tuple(tuple_element_types.clone()) },
                    expr_type: Type::tuple(tuple_element_types),
                })
            }
            TypedExpression::Block { inner: _, final_expression, return_type: _ } => {
                // TODO: how can we handle inner statement?
                rhs = *final_expression;
                continue;
            }
            TypedExpression::Tuple { expressions } => {
                let m = outer_destruction.iter().zip(expressions).map(|(element_binding, expression)| {
                    handle_atomic_pattern(expression, element_binding, checker)
                }).collect::<Vec<_>>();

                let mut k = vec![];

                for mx in m {
                    let Ok(y) = mx else { return mx };
                    k.extend(y);
                }

                break Ok(k)
            }
            TypedExpression::ExtractTuple { expr, index } => {
                let expr = *expr;
                let expr = match expr {
                    TypedExpression::If { condition, then, els, return_type } => {
                        RecursivePatternMatchDesugarStrategy::Simple(TypedExpression::If { condition, then, els, return_type: return_type.as_tuple().expect("oops 15").0[index].clone() })
                    }
                    TypedExpression::Block { inner, final_expression, return_type } => {
                        RecursivePatternMatchDesugarStrategy::Simple(TypedExpression::Block { inner, final_expression, return_type: return_type.as_tuple().expect("oops 4").0[index].clone() })
                    }
                    TypedExpression::Tuple { expressions } => {
                        RecursivePatternMatchDesugarStrategy::Simple(expressions[index].clone())
                    }
                    TypedExpression::Variable { .. } => {
                        RecursivePatternMatchDesugarStrategy::InsertTemporary(expr)
                    }
                    other => RecursivePatternMatchDesugarStrategy::Simple(other),
                };

                match expr {
                    RecursivePatternMatchDesugarStrategy::Simple(expr) => {
                        debug!("recurse");
                        rhs = expr;
                        continue;
                    }
                    RecursivePatternMatchDesugarStrategy::InsertTemporary(expr) => {
                        let new_ident = checker.make_fresh_identifier();
                        let tp = expr.actual_type().as_tuple().expect("oh").0[index].clone();
                        let v = TypedExpression::Variable {
                            ident: new_ident.clone(), tp: tp.clone()
                        };

                        checker.ctx.borrow_mut().add_known_variable(new_ident.clone(), tp);
                        let v = desugar_recursive_pattern_match(outer_destruction, v, checker)?;
                        let mut r = VecDeque::from(v);

                        r.push_front(TypedStatement::VariableDeclaration {
                            identifier: new_ident,
                            expression: TypedExpression::ExtractTuple {
                                expr: Box::new(expr),
                                index
                            },
                        });

                        break Ok(r.into_iter().collect::<Vec<_>>())
                    }
                }
            }
            other => {
                debug!("unsupported expression");
                break Err(TypeCheckError::UnsatisfiablePattern {
                    pattern: AtomicPattern::Tuple(outer_destruction.to_vec()),
                    expr_type: other.actual_type(),
                    expression: other,
                })
            }
        }
    }
}

fn extract_pattern(expr: TypedExpression, pattern: &AtomicPattern, type_annotation: Option<TypeSignature>, checker: &TypeChecker) -> Result<Vec<TypedStatement>, TypeCheckError> {
    let Some(type_name) = type_annotation else {
        // no annotations, just set its type (type-inference) from the expr
        return handle_atomic_pattern(expr, pattern, checker)
    };

    let Ok(dest) = checker.lower_type_signature_into_type(&type_name) else {
        return Err(TypeCheckError::UnknownType {
            name: type_name
        })
    };

    match dest.is_assignable(&expr.actual_type()) {
        AssignableQueryAnswer::Yes => {
            handle_atomic_pattern(expr, pattern, checker)
        },
        AssignableQueryAnswer::PossibleIfCoerceSourceImplicitly => {
            Err(TypeCheckError::UnassignableType {
                from: expr.actual_type(),
                to: dest,
            })
        }
        AssignableQueryAnswer::No => {
            Err(TypeCheckError::UnassignableType {
                from: expr.actual_type(),
                to: dest,
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
                extract_pattern(checked, &pattern, type_annotation, checker)
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
                checker.ctx.borrow_mut().known_aliases.insert(new_name, checker.lower_type_signature_into_type(&replace_with).map_err(|()| TypeCheckError::UnknownType {
                    name: replace_with,
                })?);

                Ok(vec![])
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
    ctx: RefCell<Context>
}

impl TypeChecker {
    #[expect(clippy::result_unit_err)]
    pub fn lower_type_signature_into_type(&self, p0: &TypeSignature) -> Result<Type, ()> {
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

    #[must_use]
    #[expect(clippy::missing_panics_doc)] // do not panic
    pub fn make_fresh_identifier(&self) -> Identifier {
        // TODO: this implementation is poor. choice more elegant algorithm.
        let hello = RandomState::new().hash_one(());
        let m = hello.to_ne_bytes();
        let m = [
            b'_', b'_',
            m[0].clamp(b'a', b'z'), m[1].clamp(b'a', b'z'), m[2].clamp(b'a', b'z'), m[3].clamp(b'a', b'z'),
            m[4].clamp(b'a', b'z'), m[5].clamp(b'a', b'z'), m[6].clamp(b'a', b'z'), m[7].clamp(b'a', b'z'),
        ];

        Identifier::new(core::str::from_utf8(&m).expect("not panic").to_owned())
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
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
            ctx: RefCell::new(Context::empty()),
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
