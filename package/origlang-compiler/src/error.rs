use thiserror::Error;
use crate::parser::SimpleErrorWithPos;
use crate::type_check::error::TypeCheckError;

#[derive(Error, Debug)]
pub enum AllError {
    #[error("{0}")]
    Generic(#[from] SimpleErrorWithPos),
    #[error("{0}")]
    TypeCheck(#[from] TypeCheckError),
}