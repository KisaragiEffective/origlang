#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use origlang_source_span::CanonicalSourceSpan;

pub trait Diagnostic {
    fn human_readable_message(&self) -> &str;

    fn cause(&self) -> &[CauseTree];

    fn severity(&self) -> DiagnosticSeverity;

    fn span(&self) -> Option<CanonicalSourceSpan>;
}

pub enum CauseTree {
    Branch {
        reason: Box<dyn Diagnostic>,
        children: Vec<Self>
    },
    Leaf {
        reason: Box<dyn Diagnostic>
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum DiagnosticSeverity {
    Allowed,
    Warning,
    Error,
    HardError,
}

pub trait DiagnosticSink {
    fn emit_diagnostics<D: Diagnostic>(&self, diagnostics: D);
}
