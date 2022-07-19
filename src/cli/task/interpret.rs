use crate::cli::task::print_ast::ParseSource;
use crate::cli::task::Task;
use crate::Runtime;

pub struct Interpret;

impl Task for Interpret {
    type Environment = ParseSource;
    type Error = String;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        use crate::parser::Parser;
        let source = environment.source();
        let parser = Parser::create(source.as_str());
        let root_ast = parser.parse()?;
        let runtime = Runtime::create(root_ast);
        Ok(runtime.execute())
    }
}
