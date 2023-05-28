use std::cmp::Ordering;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ir::IR1;
use origlang_typesystem_model::{TypedExpression, TypedIntLiteral};

// TODO: those implementations are not recursive
macro_rules! delegate {
    ($trait_:ident, $implementor:ty, $method:ident) => {
        impl $trait_<$implementor> for $implementor {
            fn $method(self, other: $implementor) -> Option<$implementor> {
                <$implementor>::$method(self, other)
            }
        }
    }
}

trait CheckedAdd<T> {
    fn checked_add(self, other: T) -> Option<T>;
}

delegate!(CheckedAdd, i8, checked_add);
delegate!(CheckedAdd, i16, checked_add);
delegate!(CheckedAdd, i32, checked_add);
delegate!(CheckedAdd, i64, checked_add);

trait CheckedSub<T> {
    fn checked_sub(self, other: T) -> Option<T>;
}

delegate!(CheckedSub, i8,  checked_sub);
delegate!(CheckedSub, i16, checked_sub);
delegate!(CheckedSub, i32, checked_sub);
delegate!(CheckedSub, i64, checked_sub);

trait CheckedMul<T> {
    fn checked_mul(self, other: T) -> Option<T>;
}

delegate!(CheckedMul, i8,  checked_mul);
delegate!(CheckedMul, i16, checked_mul);
delegate!(CheckedMul, i32, checked_mul);
delegate!(CheckedMul, i64, checked_mul);

trait CheckedDiv<T> {
    fn checked_div(self, other: T) -> Option<T>;
}

delegate!(CheckedDiv, i8,  checked_div);
delegate!(CheckedDiv, i16, checked_div);
delegate!(CheckedDiv, i32, checked_div);
delegate!(CheckedDiv, i64, checked_div);

trait OutputCompareResultAsSelf : Sized + PartialOrd + PartialEq {
    fn compare_self(self, other: Self) -> Option<Self>;
}

trait CastFrom<T>: Sized {
    fn cast_from(value: T) -> Self;
}

macro_rules! gen_cast_from {
    ($from:ty, $to:ty) => {
        impl CastFrom<$from> for $to {
            fn cast_from(value: $from) -> Self {
                value as _
            }
        }
    };
}

gen_cast_from!(Ordering, i8);
gen_cast_from!(Ordering, i16);
gen_cast_from!(Ordering, i32);
gen_cast_from!(Ordering, i64);

impl<T: PartialOrd + PartialEq + CastFrom<Ordering>> OutputCompareResultAsSelf for T {
    fn compare_self(self, other: Self) -> Option<Self> {
        self.partial_cmp(&other).map(T::cast_from)
    }
}

trait Continue<T> {
    fn continue_value(self) -> Option<T>;
}

impl<T> Continue<T> for Option<T> {
    #[allow(clippy::use_self)]
    fn continue_value(self) -> Option<T> {
        self
    }
}

impl Continue<bool> for bool {
    fn continue_value(self) -> Option<bool> {
        self.then_some(true)
    }
}

/// 二項演算子についての定数畳み込みを行う。
pub struct FoldBinaryOperatorInvocationWithConstant(pub Vec<IR1>);

impl FoldBinaryOperatorInvocationWithConstant {
    #[must_use = "Dropping return value implies drop optimized IRs"]
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

    #[allow(clippy::cast_lossless)]
    #[must_use]
    fn walk_expression(expr: TypedExpression) -> TypedExpression {
        let (lhs, rhs, operator, return_type) = match expr {
            TypedExpression::If { condition, then, els, return_type } => {
                return TypedExpression::If {
                    condition: Box::new(Self::walk_expression(*condition)),
                    then: Box::new(Self::walk_expression(*then)),
                    els: Box::new(Self::walk_expression(*els)),
                    return_type
                }
            }
            TypedExpression::Block { inner, final_expression, return_type } => {
                return TypedExpression::Block {
                    inner,
                    final_expression: Box::new(Self::walk_expression(*final_expression)),
                    return_type
                }
            }
            TypedExpression::Tuple { expressions } => {
                return TypedExpression::Tuple { expressions: expressions.into_iter().map(Self::walk_expression).collect() }
            }
            TypedExpression::BinaryOperator { lhs, rhs, operator, return_type } => (lhs, rhs, operator, return_type),
            // these expressions are optimal for this path
            other => {
                return other
            }
        };

        let lhs_ref = lhs.as_ref();
        let rhs_ref = rhs.as_ref();
        let operands = (lhs_ref, rhs_ref);

        if let (TypedExpression::IntLiteral(lhs_lit), TypedExpression::IntLiteral(rhs_lit)) = operands {
            if lhs_lit.actual_type() == rhs_lit.actual_type() {
                macro_rules! fold_opt_to_discriminator {
                    ($binary_function:expr, $arg1:expr, $arg2:expr, $into:ident) => {
                        $binary_function($arg1, $arg2).map_or_else(
                            || TypedExpression::BinaryOperator {
                                lhs,
                                rhs,
                                operator,
                                return_type
                            },
                            |v| TypedExpression::IntLiteral(TypedIntLiteral::$into(v))
                        )
                    }
                }

                macro_rules! fold_int_literals {
                    ($method:expr) => {
                        match (lhs_lit, rhs_lit) {
                            (TypedIntLiteral::Generic(lhs), TypedIntLiteral::Generic(rhs)) => {
                                fold_opt_to_discriminator!($method, *lhs, *rhs, Generic)
                            },
                            (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                                fold_opt_to_discriminator!($method, *lhs, *rhs, Bit64)
                            },
                            (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                                fold_opt_to_discriminator!($method, *lhs, *rhs, Bit32)
                            },
                            (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                                fold_opt_to_discriminator!($method, *lhs, *rhs, Bit16)
                            },
                            (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                                fold_opt_to_discriminator!($method, *lhs, *rhs, Bit8)
                            },
                            _ => unreachable!(),
                        }
                    }
                }

                match operator {
                    // If overflowed, exit constant propagation (the behavior is unspecified, but results in a runtime error for now)
                    BinaryOperatorKind::Plus => fold_int_literals!(CheckedAdd::checked_add),
                    BinaryOperatorKind::Minus => fold_int_literals!(CheckedSub::checked_sub),
                    BinaryOperatorKind::Multiply => fold_int_literals!(CheckedMul::checked_mul),
                    BinaryOperatorKind::Divide => fold_int_literals!(CheckedDiv::checked_div),
                    BinaryOperatorKind::More => fold_int_literals!(|a, b| Some((a > b) as _)),
                    BinaryOperatorKind::MoreEqual => fold_int_literals!(|a, b| Some((a >= b) as _)),
                    BinaryOperatorKind::Less => fold_int_literals!(|a, b| Some((a < b) as _)),
                    BinaryOperatorKind::LessEqual => fold_int_literals!(|a, b| Some((a <= b) as _)),
                    BinaryOperatorKind::ThreeWay => fold_int_literals!(OutputCompareResultAsSelf::compare_self),
                    BinaryOperatorKind::Equal => fold_int_literals!(|a, b| Some((a == b) as _)),
                    BinaryOperatorKind::NotEqual => fold_int_literals!(|a, b| Some((a != b) as _)),
                }
            } else {
                TypedExpression::BinaryOperator {
                    lhs,
                    rhs,
                    operator,
                    return_type
                }
            }
        } else if let (TypedExpression::BooleanLiteral(lhs_lit), TypedExpression::BooleanLiteral(rhs_lit)) = operands {
            match operator {
                BinaryOperatorKind::Equal => TypedExpression::BooleanLiteral(lhs_lit == rhs_lit),
                BinaryOperatorKind::NotEqual => TypedExpression::BooleanLiteral(lhs_lit != rhs_lit),
                _ => TypedExpression::BinaryOperator {
                    lhs,
                    rhs,
                    operator,
                    return_type
                },
            }
        } else {
            TypedExpression::BinaryOperator {
                lhs,
                rhs,
                operator,
                return_type
            }
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

/// 中間の文がない`block`をインライン化する。
pub struct InlineSimpleBlock(pub Vec<IR1>);

impl InlineSimpleBlock {
    pub fn optimize(self) -> Vec<IR1> {
        self.0.into_iter().map(|x| match x {
            IR1::Output(x) => IR1::Output(Self::walk_expression(x)),
            IR1::UpdateVariable { ident, value } => IR1::UpdateVariable { 
                ident,
                value: Self::walk_expression(value)
            },
            other => other,
        }).collect()
    }
    
    fn walk_expression(checked: TypedExpression) -> TypedExpression {
        match checked {
            TypedExpression::Block { inner, final_expression, return_type: _ } if inner.is_empty() => {
                *final_expression
            }
            other => other,
        }
    }
}