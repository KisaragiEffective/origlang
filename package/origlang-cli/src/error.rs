use crate::args::ReadSourceError;
use origlang_parser::error::ParserError;
use origlang_runtime::RuntimeError;
use origlang_typecheck::type_check::error::TypeCheckError;
use std::any::Any;
use thiserror::Error;

#[derive(Error, Debug)]
#[expect(clippy::module_name_repetitions)]
pub enum TaskExecutionError {
    #[error("Failed to read source: {0}")]
    Source(#[from] ReadSourceError),
    #[error("{0}")]
    Generic(#[from] ParserError),
    #[error("{0}")]
    TypeCheck(#[from] TypeCheckError),
    #[error("{0}")]
    Runtime(#[from] RuntimeError),
    #[error("Thread returned error")]
    ThreadJoin(Box<dyn Any + Send>),
}
