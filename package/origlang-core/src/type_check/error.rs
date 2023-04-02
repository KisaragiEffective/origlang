use thiserror::Error;
use crate::ast::after_parse::BinaryOperatorKind;
use crate::type_check::Type;

#[derive(Debug, Eq, PartialEq, Clone, Error)]
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
    UndefinedIdentifier(String),
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
    }
}