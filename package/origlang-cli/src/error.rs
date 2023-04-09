use thiserror::Error;
use origlang_compiler::parser::SimpleErrorWithPos;
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_runtime::RuntimeError;

#[derive(Error, Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum TaskExecutionError {
    #[error("{0}")]
    Generic(#[from] SimpleErrorWithPos),
    #[error("{0}")]
    TypeCheck(#[from] TypeCheckError),
    #[error("{0}")]
    Runtime(#[from] RuntimeError),
}
