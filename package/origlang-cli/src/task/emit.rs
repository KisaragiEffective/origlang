use thiserror::Error;
use origlang_compiler::lexer::Lexer;
use origlang_compiler::parser::Parser;
use origlang_compiler::parser::error::ParserError;
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_ir::{IntoVerbatimSequencedIR, IR1, IR2};
use origlang_ir_optimizer::lower::{EachStep, LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::{NoOptimization, SimpleOptimization};
use crate::args::{EmitPhase, OptimizeLevel, ParseSource, ReadSourceError};
use crate::error::TaskExecutionError;
use crate::task::Task;

pub struct UnstableEmit {
    pub(crate) phase: EmitPhase,
}

#[derive(Error, Debug)]
pub enum EmitError {
    #[error("source: {0}")]
    Source(#[from] ReadSourceError),
    #[error("parser: {0}")]
    Parser(#[from] ParserError),
    #[error("type check: {0}")]
    TypeCheck(#[from] TypeCheckError),
}

impl From<EmitError> for TaskExecutionError {
    fn from(value: EmitError) -> Self {
        match value {
            EmitError::Parser(e) => Self::Generic(e),
            EmitError::TypeCheck(e) => Self::TypeCheck(e),
            EmitError::Source(e) => Self::Source(e),
        }
    }
}

impl Task for UnstableEmit {
    type Environment = (ParseSource, OptimizeLevel);
    type Error = EmitError;

    fn execute(&self, (environment, optimize_level): Self::Environment) -> Result<(), Self::Error> {
        let src = environment.source()?;
        if self.phase == EmitPhase::LexerToken {
            let lexer = Lexer::create(&src);
            loop {
                let a = lexer.next();
                println!("{a:?}");
                if a.data.is_error() || a.data.is_end() {
                    return Ok(())
                }
            }
        }

        let parser = Parser::create(&src);
        let root = parser.parse()?;

        if self.phase == EmitPhase::Ast {
            println!("{root:#?}");
            return Ok(())
        }

        let checker = TypeChecker::new();
        let expr = checker.check(root)?;

        if self.phase == EmitPhase::TypedAst {
            println!("{expr:#?}");
            return Ok(())
        }

        let ir_sequence = expr.into_ir();

        let optimizer = match optimize_level {
            OptimizeLevel::None => &NoOptimization as &dyn EachStep,
            OptimizeLevel::Basic => &SimpleOptimization as &dyn EachStep,
        };

        let the_lower = TheTranspiler::new(optimizer);

        if self.phase == EmitPhase::Ir1 {
            let ir_sequence: Vec<IR1> = the_lower.optimizer().optimize(ir_sequence);

            println!("{ir_sequence:#?}");
            return Ok(())
        }

        let ir_sequence = the_lower.lower(ir_sequence);

        assert_eq!(self.phase, EmitPhase::Ir2);

        let ir_sequence: Vec<IR2> = the_lower.optimizer().optimize(ir_sequence);

        println!("{ir_sequence:#?}");

        Ok(())
    }
}