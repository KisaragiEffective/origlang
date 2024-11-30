pub mod emit;
pub mod interpret;
pub mod repl;

pub trait Task {
    type Environment;
    type Error;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error>;
}
