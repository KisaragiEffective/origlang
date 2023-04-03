use std::time::Instant;
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
        let i = Instant::now();
        eprintln!("init: {:?}", i.elapsed());
        let source = environment.source();
        eprintln!("source: {:?}", i.elapsed());
        let parser = Parser::create(source.as_str());
        eprintln!("parser.ctor: {:?}", i.elapsed());
        let root_ast = parser.parse()?;
        eprintln!("parser.parse: {:?}", i.elapsed());
        let type_checker = TypeChecker::new();
        eprintln!("typeck.ctor: {:?}", i.elapsed());
        type_checker.check(&root_ast)?;
        eprintln!("typeck.check: {:?}", i.elapsed());
        let runtime = Runtime::create(PrintToStdout);
        eprintln!("runtime.ctor: {:?}", i.elapsed());
        runtime.execute(root_ast);
        eprintln!("runtime.run: {:?}", i.elapsed());
        Ok(())
    }
}
