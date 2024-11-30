#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use origlang_source_span::CanonicalSourceSpan;

pub trait Diagnostic {
    fn human_readable_message(&self) -> &str;

    fn cause(&self) -> &[CauseTree];

    fn severity(&self) -> DiagnosticSeverity;

    fn span(&self) -> Option<CanonicalSourceSpan>;
}

impl<D: Diagnostic + ?Sized> Diagnostic for Box<D> {
    fn human_readable_message(&self) -> &str {
        self.as_ref().human_readable_message()
    }

    fn cause(&self) -> &[CauseTree] {
        self.as_ref().cause()
    }

    fn severity(&self) -> DiagnosticSeverity {
        self.as_ref().severity()
    }

    fn span(&self) -> Option<CanonicalSourceSpan> {
        self.as_ref().span()
    }
}

pub enum CauseTree {
    Branch {
        reason: Box<dyn Diagnostic>,
        children: Vec<Self>,
    },
    Leaf {
        reason: Box<dyn Diagnostic>,
    },
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum DiagnosticSeverity {
    Allowed,
    Warning,
    Error,
    HardError,
}

pub trait DiagnosticSink {
    fn handle_diagnostic(&self, diagnostics: &dyn Diagnostic);
}
