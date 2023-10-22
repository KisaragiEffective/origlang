#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::any::type_name;

use log::debug;
use thiserror::Error;
use origlang_ast::Statement;
use origlang_compiler::lexer::token::{Token as LexerToken};
use origlang_compiler::parser::{Parser, SimpleErrorWithPos};
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_diagnostics::{Diagnostic, DiagnosticSink};
use origlang_ir::{IntoVerbatimSequencedIR, IR1, IR2};
use origlang_ir_optimizer::lower::{LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::{NoOptimization, OptimizationPreset};

pub struct TheCompiler {
    /// The linter, built-in diagnostics, etc... See [`ScannerRegistry`].
    scanner: ScannerRegistry,
    optimization_preset: OptimizationPresetCollection,
    diagnostic_receivers: Vec<Box<dyn DiagnosticSink>>,
}

impl TheCompiler {
    /// Creates new instance without any optimization, scanner, nor diagnostic receiver.
    #[must_use] pub fn new() -> Self {
        Self {
            scanner: ScannerRegistry::default(),
            optimization_preset: OptimizationPresetCollection::none(),
            diagnostic_receivers: vec![],
        }
    }

    #[must_use]
    pub fn scanners(mut self, modify: impl FnOnce(&mut ScannerRegistry)) -> Self {
        modify(&mut self.scanner);

        self
    }

    #[must_use] pub fn register_diagnostic_receiver<DS: DiagnosticSink + 'static>(mut self, receiver: Box<DS>) -> Self {
        debug!("registered {ds}", ds = type_name::<DS>());

        self.diagnostic_receivers.push(receiver as _);

        self
    }

    #[must_use]
    pub fn optimization_preset(mut self, modify: impl FnOnce(&mut OptimizationPresetCollection)) -> Self {
        modify(&mut self.optimization_preset);

        self
    }

    pub fn compile(&self, source: String) -> Result<Vec<IR1>, PartialCompilation> {
        let x = Parser::create(&source);
        let root = x.parse()?;
        let typeck = TypeChecker::new().check(root)?;
        let ir0_seq = typeck.into_ir();
        let the_transpiler = TheTranspiler {
            optimization_preset: &self.optimization_preset
        };

        let ir1_seq = ir0_seq;
        let (pre, post): (Vec<_>, Vec<_>) =
            self.scanner.ir1_scanner.iter().partition(|x| matches!(x, PreOrPost::Pre(..)));

        for s in pre {
            if let PreOrPost::Pre(s) = s {
                let diags = s.scan(&ir1_seq);
                let diagnostics = diags.iter();
                for diag in diagnostics {
                    for receiver in &self.diagnostic_receivers {
                        receiver.handle_diagnostic(&**diag);
                    }
                }
            }
        }
        let optimized = self.optimization_preset.ir1.optimize(ir1_seq);
        for s in post {
            if let PreOrPost::Post(s) = s {
                let diags = s.scan(&optimized);
                let diagnostics = diags.iter();
                for diag in diagnostics {
                    for receiver in &self.diagnostic_receivers {
                        receiver.handle_diagnostic(&**diag);
                    }
                }
            }
        }

        Ok(optimized)
    }
}

impl Default for TheCompiler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Eq, PartialEq, Error)]
pub enum PartialCompilation {
    #[error("syntax error: {0}")]
    Parser(#[from] SimpleErrorWithPos),
    #[error("type check error: {0}")]
    TypeChecker(#[from] TypeCheckError),
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum PreOrPost<T> {
    Pre(T),
    Post(T),
}

#[derive(Default)]
pub struct ScannerRegistry {
    pub source_scanner: Vec<Box<dyn Scanner<Statement>>>,
    pub ir1_scanner: Vec<PreOrPost<Box<dyn Scanner<IR1>>>>,
}

impl ScannerRegistry {
    fn register_source(&mut self, to_register: Box<dyn Scanner<Statement>>) {
        self.source_scanner.push(to_register);
    }

    fn register_ir1(&mut self, to_register: PreOrPost<Box<dyn Scanner<IR1>>>) {
        self.ir1_scanner.push(to_register);
    }
}

trait SupportedScanType {}

impl SupportedScanType for LexerToken {}

impl SupportedScanType for IR1 {}

pub trait Scanner<T> {
    /// This function should be deterministic, except memory allocation.
    fn scan(&self, values: &[T]) -> Vec<Box<dyn Diagnostic>>;

    /// Note: memory allocation may be ignored.
    fn is_deterministic(&self) -> bool;
}

pub struct OptimizationPresetCollection {
    pub ir1: Box<dyn OptimizationPreset<IR1>>,
    pub ir2: Box<dyn OptimizationPreset<IR2>>,
}

impl OptimizationPresetCollection {
    fn none() -> Self {
        Self {
            ir1: Box::new(NoOptimization) as Box<_>,
            ir2: Box::new(NoOptimization) as Box<_>,
        }
    }
}

macro_rules! delegate_optimization_to_field {
    ($field:ident, $step:ty) => {
        impl OptimizationPreset<$step> for OptimizationPresetCollection {
            fn optimize(&self, seq: Vec<$step>) -> Vec<$step> {
                self.$field.optimize(seq)
            }
        }
    };
}

delegate_optimization_to_field!(ir1, IR1);
delegate_optimization_to_field!(ir2, IR2);
