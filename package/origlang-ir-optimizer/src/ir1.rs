use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ir::IR1;
use origlang_typesystem_model::{TypedExpression, TypedIntLiteral};

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
                        // WONTFIX: avoid them
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
