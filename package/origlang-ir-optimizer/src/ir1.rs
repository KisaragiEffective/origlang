#[cfg(test)]
mod tests;

use std::cmp::Ordering;
use std::num::NonZeroU8;
use std::ops::{Add, BitAnd, Mul};
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ir::IR1;
use origlang_typesystem_model::{TypedExpression, TypedIntLiteral};
pub use num_traits::{CheckedAdd, CheckedSub, CheckedDiv, CheckedMul};
use num_traits::{One, WrappingSub, Zero};
use tap::Conv;
use crate::expr::AsTypedIntLiteral;
use crate::num::{Coerced, IntegralLogarithm, NonCoerced, SizeOfLessThan256Bits, CreateWitness};

trait OutputCompareResultAsSelf : Sized + PartialOrd + PartialEq {
    fn compare_self(&self, other: &Self) -> Option<Self>;
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
    fn compare_self(&self, other: &Self) -> Option<Self> {
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

fn walk_rec(f: &mut impl FnMut(&TypedExpression) -> TypedExpression, s: TypedExpression) -> TypedExpression {
    match s {
        TypedExpression::If { condition, then, els, return_type } => {
            return TypedExpression::If {
                condition: Box::new(walk_rec(f, *condition)),
                then: Box::new(walk_rec(f, *then)),
                els: Box::new(walk_rec(f, *els)),
                return_type
            }
        }
        TypedExpression::Block { inner, final_expression, return_type } => {
            return TypedExpression::Block {
                inner,
                final_expression: Box::new(walk_rec(f, *final_expression)),
                return_type
            }
        }
        TypedExpression::Tuple { expressions } => {
            return TypedExpression::Tuple { expressions: expressions.into_iter().map(|x| walk_rec(f, x)).collect() }
        }
        other => f(&other)
    }
}

/// 二項演算子についての定数畳み込みを行う。
// TODO: 左端以外に畳み込みが行える定数項があっても、それらの項が畳み込まれない。
//       すなわち、オーバーフローがないものとして`a * 2 / 2`を最適化するとき、`a`に最適化出来ない。
//       これは`egg`クレートを導入することで解決することができる。しかし、現状の`IR1`の構成ではそれができない。
//       `egg::EGraph`を構築するためには`egg::Language`トレイトを`TypedExpression` (または、その親戚であるところの`IR1`) へ実装しなければならない。
//       しかし、`egg::Language`トレイトは`Ord`を要求する。`egg::SymbolLang`を見てみると、`Symbol` (私達で言うところの`Identifier`
//       だが、すべてのスコープを通じて一意な識別子を持つ) の内部表現が`usize`であることから、`Ord`がderiveされていた。
//       また、二項演算子においても、その両辺がノードのIDとして参照されていた。これは現状の`IR1`や`TypedExpression`では達成できない。
//       なぜならば、`IR1`と`TypedExpression`は`TypedExpression`をネストした形で保持するからだ。これはフラットな参照と相性が悪く、
//       追加の"lowering"を必要とする。
//       よって、そのケースにおける定数畳み込みを実装できるまでの間、ユーザーにはワークアラウンドとして、以下のことを要求する：
//       ```
//       print a * 2 / 2
//       ```
//       と書く代わりに、
//       ```
//       var x = 2 / 2
//       print a * x
//       ```
//       と書いて現状の不完全な実装においても同様の実装が得られるということを期待したい。
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
    #[deny(clippy::as_underscore)]
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

        // handle recursively
        let lhs = Self::walk_expression(*lhs);
        let lhs = Box::new(lhs);
        let rhs = Self::walk_expression(*rhs);
        let rhs = Box::new(rhs);

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
                                fold_opt_to_discriminator!($method, lhs, rhs, Generic)
                            },
                            (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                                fold_opt_to_discriminator!($method, lhs, rhs, Bit64)
                            },
                            (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                                fold_opt_to_discriminator!($method, lhs, rhs, Bit32)
                            },
                            (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                                fold_opt_to_discriminator!($method, lhs, rhs, Bit16)
                            },
                            (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                                fold_opt_to_discriminator!($method, lhs, rhs, Bit8)
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
                    BinaryOperatorKind::ThreeWay => fold_int_literals!(OutputCompareResultAsSelf::compare_self),
                    compare => Self::fold_compare_into_bool_literal(lhs_lit, rhs_lit, compare),
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

    fn fold_compare_into_bool_literal(lhs: &TypedIntLiteral, rhs: &TypedIntLiteral, compare: BinaryOperatorKind) -> TypedExpression {
        // [T, O] =>> (T, T) => O
        macro_rules! poly_input_lambda {
            ($closure:expr) => {
                match (lhs, rhs) {
                    (TypedIntLiteral::Generic(lhs), TypedIntLiteral::Generic(rhs)) => {
                        $closure(lhs, rhs)
                    },
                    (TypedIntLiteral::Bit64(lhs), TypedIntLiteral::Bit64(rhs)) => {
                        $closure(lhs, rhs)
                    },
                    (TypedIntLiteral::Bit32(lhs), TypedIntLiteral::Bit32(rhs)) => {
                        $closure(lhs, rhs)
                    },
                    (TypedIntLiteral::Bit16(lhs), TypedIntLiteral::Bit16(rhs)) => {
                        $closure(lhs, rhs)
                    },
                    (TypedIntLiteral::Bit8(lhs), TypedIntLiteral::Bit8(rhs)) => {
                        $closure(lhs, rhs)
                    },
                    _ => unreachable!(),
                }
            };
        }

        match compare {
            BinaryOperatorKind::More => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs > rhs)),
            BinaryOperatorKind::MoreEqual => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs >= rhs)),
            BinaryOperatorKind::Less => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs < rhs)),
            BinaryOperatorKind::LessEqual => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs <= rhs)),
            BinaryOperatorKind::Equal => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs == rhs)),
            BinaryOperatorKind::NotEqual => poly_input_lambda!(|lhs, rhs| TypedExpression::BooleanLiteral(lhs != rhs)),
            #[allow(clippy::panic)]
            other => panic!("operator {other} is not supported by this function"),
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

pub struct DecomposeDivideByConst(pub Vec<IR1>);

impl DecomposeDivideByConst {
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
        walk_rec(&mut |t| {
            let TypedExpression::BinaryOperator { lhs, rhs, operator, return_type } = &t else {
                return t.clone()
            };

            let TypedExpression::IntLiteral(rhs) = rhs.as_ref() else {
                return t.clone()
            };

            let BinaryOperatorKind::Divide = operator else {
                return t.clone()
            };

            enum DivisorPattern {
                Zero,
                PowerOfTwo {
                    exponent: NonZeroU8,
                },
                Normal(TypedIntLiteral)
            }

            fn compute_state<
                T: Eq + Zero + One + BitAnd<Output=T> + WrappingSub + IntegralLogarithm<2> + Into<TypedIntLiteral> + SizeOfLessThan256Bits + Copy
            >(t: T) -> DivisorPattern
            {
                if t == Zero::zero() {
                    DivisorPattern::Zero
                } else {
                    let k = t & (t.wrapping_sub(&One::one()));
                    if k != Zero::zero() {
                        DivisorPattern::PowerOfTwo {
                            // SAFETY: SizeOfLessThan256Bits enforces `core::mem::size_of::<T>() < 256usize`.
                            // Therefore, k.int_log() fits u8 (1 <= `k.int_log()` <= 255 = u8::MAX).
                            // `k.int_log()` never becomes zero, so using `NonZeroU8::new_unchecked` is fine.
                            exponent: unsafe { NonZeroU8::new_unchecked(k.int_log().try_into().unwrap_unchecked()) }
                        }
                    } else {
                        DivisorPattern::Normal(k.into())
                    }
                }
            }

            let state: DivisorPattern = match rhs {
                TypedIntLiteral::Generic(i) => compute_state(NonCoerced(*i)),
                TypedIntLiteral::Bit64(i) => compute_state(Coerced(*i)),
                TypedIntLiteral::Bit32(i) => compute_state(*i),
                TypedIntLiteral::Bit16(i) => compute_state(*i),
                TypedIntLiteral::Bit8(i) => compute_state(*i),
            };

            let after = match state {
                DivisorPattern::Zero => {
                    // hi zero division! we propagate your panic earlier!
                    TypedExpression::Panic
                }
                DivisorPattern::PowerOfTwo { exponent } => {
                    let bit_width = match rhs {
                        TypedIntLiteral::Generic(_) => 64,
                        TypedIntLiteral::Bit64(_) => 64,
                        TypedIntLiteral::Bit32(_) => 32,
                        TypedIntLiteral::Bit16(_) => 16,
                        TypedIntLiteral::Bit8(_) => 8,
                    };

                    if bit_width >= exponent.get() {
                        // by definition, we cannot have overflowed value.
                        TypedExpression::BinaryOperator {
                            lhs: lhs.clone(),
                            // SAFETY: Except Int8, their valid range covers u8 possible range.
                            // for Int8, the witness will not be more than 8.
                            rhs: Box::new(TypedExpression::IntLiteral(unsafe { rhs.create_witness(exponent.get() as i64).unwrap_unchecked() })),
                            operator: BinaryOperatorKind::ShiftRight,
                            return_type: lhs.actual_type(),
                        }
                    } else {
                        unsafe { TypedExpression::IntLiteral(rhs.create_witness(0).unwrap_unchecked()) }
                    }
                }
                DivisorPattern::Normal(x) => {
                    // leave as-is at this point.
                    TypedExpression::IntLiteral(x)
                }
            };

            after
        }, checked)
    }
}
