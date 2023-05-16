use std::collections::VecDeque;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ast::Identifier;
use origlang_typesystem_model::{TypedExpression, TypedIntLiteral, TypedRootAst, TypedStatement};
use crate::{CanBeEvaluated, Runtime};

#[derive(Debug)]
pub enum IR1 {
    Output(TypedExpression),
    UpdateVariable {
        ident: Identifier,
        value: TypedExpression,
    },
    PushScope,
    PopScope,
}

impl IR1 {
    pub(super) fn create<T: IntoVerbatimSequencedIR>(from: T) -> Vec<Self> {
        from.into_ir()
    }

    pub fn invoke(&self, runtime: &Runtime) {
        match self {
            Self::Output(e) => {
                runtime.o.as_ref().borrow_mut().output(e.evaluate(runtime).expect("runtime exception"));
            }
            Self::UpdateVariable { ident, value } => {
                runtime.upsert_member_to_current_scope(
                    ident.clone(),
                    value.evaluate(runtime).expect("runtime exception")
                );
            }
            Self::PushScope => {
                runtime.push_scope();
            }
            Self::PopScope => {
                runtime.pop_scope();
            }
        }
    }
}

pub(super) trait IntoVerbatimSequencedIR {
    fn into_ir(self) -> Vec<IR1>;
}

impl IntoVerbatimSequencedIR for TypedStatement {
    fn into_ir(self) -> Vec<IR1> {
        let statement = self;

        match statement {
            Self::Print { expression } => {
                vec![
                    IR1::Output(expression)
                ]
            }
            Self::VariableDeclaration { identifier, expression } => {
                vec![
                    IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::VariableAssignment { identifier, expression } => {
                vec![
                    IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::Block { inner_statements } => {
                let mut vec = inner_statements.into_iter()
                    .flat_map(|x| x.into_ir())
                    .collect::<VecDeque<IR1>>();
                vec.push_front(IR1::PushScope);
                vec.push_back(IR1::PopScope);
                vec.into()
            }
        }
    }
}

impl IntoVerbatimSequencedIR for TypedRootAst {
    fn into_ir(self) -> Vec<IR1> {
        let value = self;

        pub fn what_will_happen(ast: TypedRootAst) -> Vec<IR1> {
            ast.statements.into_iter()
                .flat_map(what_will_happen1)
                .collect()
        }

        fn what_will_happen1(statement: TypedStatement) -> Vec<IR1> {
            match statement {
                TypedStatement::Print { expression } => {
                    vec![
                        IR1::Output(expression)
                    ]
                }
                TypedStatement::VariableDeclaration { identifier, expression } => {
                    vec![
                        IR1::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                TypedStatement::VariableAssignment { identifier, expression } => {
                    vec![
                        IR1::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                TypedStatement::Block { inner_statements } => {
                    let mut vec = inner_statements.into_iter()
                        .flat_map(what_will_happen1)
                        .collect::<VecDeque<_>>();
                    vec.push_front(IR1::PushScope);
                    vec.push_back(IR1::PopScope);
                    vec.into()
                }
            }
        }

        what_will_happen(value)
    }
}

impl<T: IntoVerbatimSequencedIR> IntoVerbatimSequencedIR for Vec<T> {
    fn into_ir(self) -> Vec<IR1> {
        self.into_iter().flat_map(|x| x.into_ir()).collect()
    }
}

/// 二項演算子についての定数畳み込みを行う。
pub struct FoldBinaryOperatorInvocationWithConstant(pub Vec<IR1>);

impl FoldBinaryOperatorInvocationWithConstant {
    pub fn optimize(self) -> Vec<IR1> {
        self.0.into_iter().map(|ir| {
            match ir {
                IR1::Output(value) => IR1::Output(Self::walk_expression(value)),
                IR1::UpdateVariable { ident, value } => IR1::UpdateVariable {
                    ident, value: Self::walk_expression(value),
                },
                other => other
            }
        }).collect()
    }

    fn walk_expression(expr: TypedExpression) -> TypedExpression {
        match expr {
            TypedExpression::BinaryOperator { lhs, rhs, operator, return_type } => {
                macro_rules! leave {
                        () => {
                            TypedExpression::BinaryOperator { lhs, rhs, operator, return_type }
                        }
                    }

                match (lhs.as_ref(), rhs.as_ref()) {
                    (TypedExpression::IntLiteral(lhs), TypedExpression::IntLiteral(rhs)) => {
                        macro_rules! checked_boilerplate {
                                ($method:ident) => {
                                    match (lhs, rhs) {
                                        (TypedIntLiteral::Generic(lhs), TypedIntLiteral::Generic(rhs)) => {
                                            if let Some(v) = (*lhs).$method(*rhs) {
                                                TypedExpression::IntLiteral(TypedIntLiteral::Generic(v))
                                            } else {
                                                leave!()
                                            }
                                        },
                                        (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                                            if let Some(v) = (*lhs).$method(*rhs) {
                                                TypedExpression::IntLiteral(TypedIntLiteral::Bit64(v))
                                            } else {
                                                leave!()
                                            }
                                        },
                                        (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                                            if let Some(v) = (*lhs).$method(*rhs) {
                                                TypedExpression::IntLiteral(TypedIntLiteral::Bit32(v))
                                            } else {
                                                leave!()
                                            }
                                        },
                                        (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                                            if let Some(v) = (*lhs).$method(*rhs) {
                                                TypedExpression::IntLiteral(TypedIntLiteral::Bit16(v))
                                            } else {
                                                leave!()
                                            }
                                        },
                                        (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                                            if let Some(v) = (*lhs).$method(*rhs) {
                                                TypedExpression::IntLiteral(TypedIntLiteral::Bit8(v))
                                            } else {
                                                leave!()
                                            }
                                        },
                                        _ => leave!(),
                                    }
                                };
                            }

                        macro_rules! compare_boilerplate {
                                ($compare_bin_op:tt) => {
                                    match (lhs, rhs) {
                                        (TypedIntLiteral::Generic(lhs), TypedIntLiteral::Generic(rhs)) => {
                                            TypedExpression::BooleanLiteral(lhs $compare_bin_op rhs)
                                        },
                                        (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                                            TypedExpression::BooleanLiteral(lhs $compare_bin_op rhs)
                                        },
                                        (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                                            TypedExpression::BooleanLiteral(lhs $compare_bin_op rhs)
                                        },
                                        (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                                            TypedExpression::BooleanLiteral(lhs $compare_bin_op rhs)
                                        },
                                        (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                                            TypedExpression::BooleanLiteral(lhs $compare_bin_op rhs)
                                        },
                                        _ => leave!()
                                    }
                                };
                            }

                        match operator {
                            BinaryOperatorKind::Plus => checked_boilerplate!(checked_add),
                            BinaryOperatorKind::Minus => checked_boilerplate!(checked_sub),
                            BinaryOperatorKind::Multiply => checked_boilerplate!(checked_mul),
                            BinaryOperatorKind::Divide => checked_boilerplate!(checked_div),
                            BinaryOperatorKind::More => compare_boilerplate!(>),
                            BinaryOperatorKind::MoreEqual => compare_boilerplate!(>=),
                            BinaryOperatorKind::Less => compare_boilerplate!(<),
                            BinaryOperatorKind::LessEqual => compare_boilerplate!(<=),
                            BinaryOperatorKind::ThreeWay => {
                                match (lhs, rhs) {
                                    (TypedIntLiteral::Generic(lhs), TypedIntLiteral::Generic(rhs)) => {
                                        TypedExpression::IntLiteral(TypedIntLiteral::Generic(lhs.cmp(rhs) as i64))
                                    },
                                    (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                                        TypedExpression::IntLiteral(TypedIntLiteral::Bit8(lhs.cmp(rhs) as i8))
                                    },
                                    (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                                        TypedExpression::IntLiteral(TypedIntLiteral::Bit16(lhs.cmp(rhs) as i16))
                                    },
                                    (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                                        TypedExpression::IntLiteral(TypedIntLiteral::Bit32(lhs.cmp(rhs) as i32))
                                    },
                                    (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                                        TypedExpression::IntLiteral(TypedIntLiteral::Bit64(lhs.cmp(rhs) as i64))
                                    },
                                    _ => leave!()
                                }
                            },
                            BinaryOperatorKind::Equal => compare_boilerplate!(==),
                            BinaryOperatorKind::NotEqual => compare_boilerplate!(!=),
                        }
                    },
                    (TypedExpression::BooleanLiteral(lhs), TypedExpression::BooleanLiteral(rhs)) => {
                        match operator {
                            BinaryOperatorKind::Equal => TypedExpression::BooleanLiteral(lhs == rhs),
                            BinaryOperatorKind::NotEqual => TypedExpression::BooleanLiteral(lhs != rhs),
                            _ => leave!(),
                        }
                    },
                    _ => leave!()
                }
            },
            other => other,
        }
    }
}

/// `if`文の条件式がリテラルの場合畳み込みを行う。
pub struct FoldIfWithConstantCondition(pub Vec<IR1>);

impl FoldIfWithConstantCondition {
    pub fn optimize(self) -> Vec<IR1> {
        self.0.into_iter().map(|x| {
            match x {
                IR1::Output(expression) => {
                    IR1::Output(Self::walk_expression(expression))
                }
                IR1::UpdateVariable { ident, value: expr } => {
                    IR1::UpdateVariable { ident, value: Self::walk_expression(expr) }
                }
                other => other
            }
        }).collect()
    }

    fn walk_expression(expr: TypedExpression) -> TypedExpression {
        if let TypedExpression::If { condition, then, els, return_type } = expr {
            let condition_lit = condition.as_ref();
            if let TypedExpression::BooleanLiteral(const_cond) = condition_lit {
                dbg!(&then);
                if *const_cond {
                    *then
                } else {
                    *els
                }
            } else {
                TypedExpression::If { condition, then, els, return_type }
            }
        } else {
            expr
        }
    }
}
