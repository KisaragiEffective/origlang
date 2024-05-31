use std::time::Instant;
use origlang_parser::parser::Parser;
use origlang_runtime::{PrintToStdout, Runtime};
use crate::task::Task;
use crate::error::TaskExecutionError;
use origlang_compiler::type_check::TypeChecker;
use origlang_ir::IntoVerbatimSequencedIR;
use origlang_ir_optimizer::lower::{LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::NoOptimization;
use crate::args::ParseSource;

pub struct Interpret;

impl Task for Interpret {
    type Environment = ParseSource;
    type Error = TaskExecutionError;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let i = Instant::now();
        eprintln!("init: {:?}", i.elapsed());
        let source = environment.source()?;
        eprintln!("source: {:?}", i.elapsed());
        let parser = Parser::create(source.as_str());
        eprintln!("parser.ctor: {:?}", i.elapsed());
        let root_ast = parser.parse()?;
        eprintln!("parser.parse: {:?}", i.elapsed());
        let type_checker = TypeChecker::new();
        eprintln!("typeck.ctor: {:?}", i.elapsed());
        let root_ast = type_checker.check(root_ast)?;
        eprintln!("typeck.check: {:?}", i.elapsed());
        let runtime = Runtime::create(PrintToStdout);
        eprintln!("runtime.ctor: {:?}", i.elapsed());
        let transpiler = TheTranspiler::new(&NoOptimization);
        let ir = root_ast.into_ir();
        let ir = transpiler.lower(ir);
        runtime.start(&ir);
        eprintln!("runtime.run: {:?}", i.elapsed());
        Ok(())
    }
}
