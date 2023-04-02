use origlang_compiler::parser::Parser;
use origlang_runtime::{PrintToStdout, Runtime};
use crate::task::print_ast::ParseSource;
use crate::task::Task;
use crate::error::AllError;
use origlang_compiler::type_check::TypeChecker;

pub struct Interpret;

impl Task for Interpret {
    type Environment = ParseSource;
    type Error = AllError;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let source = environment.source();
        let parser = Parser::create(source.as_str());
        let root_ast = parser.parse()?;
        let type_checker = TypeChecker::new();
        type_checker.check(&root_ast)?;
        let runtime = Runtime::create(PrintToStdout);
        runtime.execute(root_ast);
        Ok(())
    }
}
