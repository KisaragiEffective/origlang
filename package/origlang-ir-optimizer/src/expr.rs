use origlang_typesystem_model::{TypedExpression, TypedIntLiteral};
use crate::sealed::Sealed;

pub trait AsTypedIntLiteral : Sealed<()> {
    fn as_int_literal(&self) -> Option<&TypedIntLiteral>;
}

impl Sealed<()> for TypedExpression {}

impl AsTypedIntLiteral for TypedExpression {
    fn as_int_literal(&self) -> Option<&TypedIntLiteral> {
        match self {
            TypedExpression::IntLiteral(x) => Some(x),
            _ => None
        }
    }
}