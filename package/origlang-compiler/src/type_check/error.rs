use thiserror::Error;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ast::{AtomicPattern, Identifier, TypeSignature};
use origlang_typesystem_model::TypedExpression;
use crate::type_check::Type;

#[derive(Debug, Eq, PartialEq, Clone, Error)]
#[allow(clippy::module_name_repetitions)]
pub enum TypeCheckError {
    #[error("Only ({accepted_lhs}) {operator} ({accepted_rhs}) is defined, but got {got_lhs} {operator} {got_rhs}")]
    InvalidCombinationForBinaryOperator {
        accepted_lhs: Type,
        operator: BinaryOperatorKind,
        accepted_rhs: Type,
        got_lhs: Type,
        got_rhs: Type,
    },
    #[error("Undefined identifier: {0}")]
    UndefinedIdentifier(Identifier),
    #[error("Cannot compare between two different types. lhs: {got_lhs}, rhs: {got_rhs}")]
    UnableToUnifyEqualityQuery {
        operator: BinaryOperatorKind,
        got_lhs: Type,
        got_rhs: Type,
    },
    #[error("Cannot unify two different types in if-expression. `then`-clause: {then_clause_type}, `else`-clause: {else_clause_type}")]
    UnableToUnityIfExpression {
        then_clause_type: Type,
        else_clause_type: Type,
    },
    #[error("{context} must be {expected_type}, got {actual_type}")]
    GenericTypeMismatch {
        context: String,
        expected_type: Type,
        actual_type: Type,
    },
    #[error("value of {from} cannot be assigned to {to}")]
    UnassignableType {
        from: Type,
        to: Type,
    },
    #[error("type {name} is not defined")]
    UnknownType {
        name: TypeSignature,
    },
    #[error("pattern {pattern} may not be satisfied where the expression has type of {expr_type}")]
    UnsatisfiablePattern {
        pattern: AtomicPattern,
        expression: TypedExpression,
        expr_type: Type,
    }
}
