use std::ops::{Add, BitAnd, Mul, Sub};
use num_traits::{One, WrappingSub, Zero};
use origlang_typesystem_model::TypedIntLiteral;
use crate::sealed::Sealed;

pub trait IntegralLogarithm<const N: usize> {
    /// Returns the base N logarithm, with floored value.
    /// Panics if it is negative or zero.
    fn int_log(self) -> u32;
}

macro_rules! delegate_ilog2 {
                ($ty:ty) => {
                    impl IntegralLogarithm<2> for $ty {
                        fn int_log(self) -> u32 {
                            self.ilog2()
                        }
                    }
                };
            }

delegate_ilog2!(u8);
delegate_ilog2!(u16);
delegate_ilog2!(u32);
delegate_ilog2!(u64);
delegate_ilog2!(i8);
delegate_ilog2!(i16);
delegate_ilog2!(i32);
delegate_ilog2!(i64);

pub unsafe trait SizeOfLessThan256Bits {}

macro_rules! impl_less_than_256_bits {
                ($ty:ty) => {
                    const _: () = assert!(::core::mem::size_of::<$ty>() < 256, "Too large!");

                    // SAFETY: above "static" assertion enforces requirement.
                    unsafe impl SizeOfLessThan256Bits for $ty {}
                };
            }

impl_less_than_256_bits!(u8);
impl_less_than_256_bits!(u16);
impl_less_than_256_bits!(u32);
impl_less_than_256_bits!(u64);
impl_less_than_256_bits!(i8);
impl_less_than_256_bits!(i16);
impl_less_than_256_bits!(i32);
impl_less_than_256_bits!(i64);

struct Garbage<const X: usize>(());

pub trait CreateWitness: Sealed<Garbage<1249771348871263>> {
    fn create_witness(&self, b: i64) -> Option<Self> where Self: Sized;
}

impl Sealed<Garbage<1249771348871263>> for TypedIntLiteral {}

impl CreateWitness for TypedIntLiteral {
    /// Create same variant, with different value.
    fn create_witness(&self, b: i64) -> Option<Self> {
        match self {
            TypedIntLiteral::Generic(_) => Some(Self::Generic(b)),
            TypedIntLiteral::Bit64(_) => Some(Self::Bit64(b)),
            TypedIntLiteral::Bit32(_) => b.try_into().map(Self::Bit32).ok(),
            TypedIntLiteral::Bit16(_) => b.try_into().map(Self::Bit16).ok(),
            TypedIntLiteral::Bit8(_) => b.try_into().map(Self::Bit8).ok()
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct NonCoerced(pub i64);

impl Add<Self> for NonCoerced {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Zero for NonCoerced {
    fn zero() -> Self {
        Self(0)
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl Mul<Self> for NonCoerced {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl One for NonCoerced {
    fn one() -> Self {
        Self(1)
    }
}

impl Sub<Self> for NonCoerced {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl WrappingSub for NonCoerced {
    fn wrapping_sub(&self, v: &Self) -> Self {
        Self(self.0.wrapping_sub(v.0))
    }
}

impl_less_than_256_bits!(NonCoerced);

impl IntegralLogarithm<2> for NonCoerced {
    fn int_log(self) -> u32 {
        self.0.ilog2()
    }
}

impl Into<TypedIntLiteral> for NonCoerced {
    fn into(self) -> TypedIntLiteral {
        TypedIntLiteral::Generic(self.0)
    }
}

impl BitAnd for NonCoerced {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Coerced(pub i64);

impl Add<Self> for Coerced {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl Zero for Coerced {
    fn zero() -> Self {
        Self(0)
    }

    fn is_zero(&self) -> bool {
        self.0 == 0
    }
}

impl Mul<Self> for Coerced {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self(self.0 * rhs.0)
    }
}

impl One for Coerced {
    fn one() -> Self {
        Self(1)
    }
}

impl Sub<Self> for Coerced {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0 - rhs.0)
    }
}

impl WrappingSub for Coerced {
    fn wrapping_sub(&self, v: &Self) -> Self {
        Self(self.0.wrapping_sub(v.0))
    }
}

impl_less_than_256_bits!(Coerced);

impl IntegralLogarithm<2> for Coerced {
    fn int_log(self) -> u32 {
        self.0.ilog2()
    }
}

impl Into<TypedIntLiteral> for Coerced {
    fn into(self) -> TypedIntLiteral {
        TypedIntLiteral::Bit64(self.0)
    }
}

impl BitAnd for Coerced {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
