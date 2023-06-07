pub mod test;
pub mod interpret;
pub mod repl;
pub mod emit;

pub trait Task {
    type Environment;
    type Error;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error>;
}
