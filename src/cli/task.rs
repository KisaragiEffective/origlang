pub mod test;
pub mod print_ast;
pub mod interpret;
pub mod repl;
pub mod lexer_dump;

pub trait Task {
    type Environment;
    type Error;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error>;
}