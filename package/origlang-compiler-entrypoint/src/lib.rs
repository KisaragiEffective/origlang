use std::any::{Any, type_name, TypeId};
use std::ops::Deref;
use log::debug;
use thiserror::Error;
use origlang_ast::Statement;
use origlang_compiler::parser::{Parser, SimpleErrorWithPos};
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_diagnostics::{Diagnostic, DiagnosticSink};
use origlang_ir::{IntoVerbatimSequencedIR, IR0, IR1};
use origlang_ir_optimizer::lower::{LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::{NoOptimization, OptimizationPreset};
use crate::lexer::{Token as LexerToken};
use crate::parser::{Parser, SimpleErrorWithPos};
use crate::type_check::error::TypeCheckError;
use crate::type_check::TypeChecker;

pub struct TheCompiler {
    /// The linter, built-in diagnostics, etc...
    scanner: ScannerRegistry,
    optimization_preset: OptimizationPresetCollection,
    diagnostic_receivers: Vec<Box<dyn DiagnosticSink>>,
}

impl TheCompiler {
    pub fn new() -> Self {
        Self {
            scanner: ScannerRegistry::default(),
            optimization_preset: OptimizationPresetCollection::none(),
            diagnostic_receivers: vec![],
        }
    }

    pub fn scanners(mut self, modify: impl FnOnce(&mut ScannerRegistry)) -> Self {
        modify(&mut self.scanner);

        self
    }

    pub fn register_diagnostic_receiver<DS: DiagnosticSink + 'static>(mut self, receiver: Box<DS>) -> Self {
        debug!("registered {ds}", ds = type_name::<DS>());

        self.diagnostic_receivers.push(receiver as _);

        self
    }

    pub fn optimization_preset(mut self, modify: impl FnOnce(&mut OptimizationPresetCollection)) -> Self {
        modify(&mut self.optimization_preset);

        self
    }

    pub fn compile(&self, source: String) -> Result<Vec<IR1>, PartialCompilation> {
        let x = Parser::create(&source);
        let root = x.parse()?;
        let typeck = TypeChecker::new().check(root)?;
        let ir0_seq = typeck.into_ir();
        let optimized = self.optimization_preset.ir0.optimize(ir0_seq);
        let ir1_seq: Vec<IR1> = TheTranspiler.lower(optimized);
        let (pre, post): (Vec<_>, Vec<_>) =
            self.scanner.ir1_scanner.into_iter().partition(|x| matches!(x, PreOrPost::Pre(..)));

        for s in pre {
            if let PreOrPost::Pre(s) = s {
                let diags = s.scan(&ir1_seq);
                let diagnostics = diags.iter();
                for diag in diagnostics {
                    for receiver in &self.diagnostic_receivers {
                        receiver.handle_diagnostic(diag.deref())
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
                        receiver.handle_diagnostic(diag.deref())
                    }
                }
            }
        }

        Ok(optimized)
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
    pub ir0: Box<dyn OptimizationPreset<IR0>>,
    pub ir1: Box<dyn OptimizationPreset<IR1>>,
}

impl OptimizationPresetCollection {
    fn none() -> Self {
        OptimizationPresetCollection {
            ir0: Box::new(NoOptimization) as Box<_>,
            ir1: Box::new(NoOptimization) as Box<_>,
        }
    }
}
