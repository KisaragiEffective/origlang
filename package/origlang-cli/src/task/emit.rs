use tap::Pipe;
use thiserror::Error;
use origlang_compiler::lexer::Lexer;
use origlang_compiler::parser::{Parser, SimpleErrorWithPos};
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_ir_optimizer::ir1::{FoldBinaryOperatorInvocationWithConstant, FoldIfWithConstantCondition};
use origlang_ir_optimizer::lower::{LowerStep, LowerToIR1};
use crate::args::{EmitPhase, ParseSource};
use crate::error::TaskExecutionError;
use crate::task::Task;

pub struct UnstableEmit {
    pub(crate) phase: EmitPhase,
}

#[derive(Error, Debug)]
pub enum EmitError {
    #[error("parser: {0}")]
    Parser(#[from] SimpleErrorWithPos),
    #[error("type check: {0}")]
    TypeCheck(#[from] TypeCheckError),
}

impl From<EmitError> for TaskExecutionError {
    fn from(value: EmitError) -> Self {
        match value {
            EmitError::Parser(e) => Self::Generic(e),
            EmitError::TypeCheck(e) => Self::TypeCheck(e),
        }
    }
}

impl Task for UnstableEmit {
    type Environment = ParseSource;
    type Error = EmitError;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let src = environment.source();
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
        let checked = checker.check(root)?;

        if self.phase == EmitPhase::TypedAst {
            println!("{checked:#?}");
            return Ok(())
        }

        use origlang_ir::IntoVerbatimSequencedIR;
        let ir_sequence = checked.into_ir();

        if self.phase == EmitPhase::Ir0 {
            println!("{ir_sequence:#?}");
            return Ok(())
        }

        let ir_sequence = LowerToIR1::lower(ir_sequence);
        if self.phase == EmitPhase::OptimizedIr1 {
            let optimized_ir = ir_sequence
                .pipe(FoldBinaryOperatorInvocationWithConstant).pipe(|x| x.optimize())
                .pipe(FoldIfWithConstantCondition).pipe(|x| x.optimize());
            println!("{optimized_ir:#?}");
        } else {
            println!("{ir_sequence:#?}");
        }

        Ok(())
    }
}