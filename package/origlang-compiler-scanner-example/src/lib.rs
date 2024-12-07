#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

#[cfg(test)]
mod tests {
    use origlang_compiler_entrypoint::{PreOrPost, Scanner, TheCompiler};
    use origlang_diagnostics::{CauseTree, Diagnostic, DiagnosticSeverity, DiagnosticSink};
    use origlang_ir::IR1;
    use origlang_source_span::CanonicalSourceSpan;
    use std::cell::Cell;

    struct MyScanner;

    impl Scanner<IR1> for MyScanner {
        fn scan(&self, values: &[IR1]) -> Vec<Box<dyn Diagnostic>> {
            values
                .iter()
                .filter(|x| match x {
                    IR1::UpdateVariable { ident, .. } => ident.as_name() == "foo",
                    _ => false,
                })
                .map(|_| Box::new(GiveItMorePreciseName) as Box<_>)
                .collect()
        }

        fn is_deterministic(&self) -> bool {
            true
        }
    }

    struct GiveItMorePreciseName;

    impl Diagnostic for GiveItMorePreciseName {
        fn human_readable_message(&self) -> &str {
            "Please give it more precise name"
        }

        fn cause(&self) -> &[CauseTree] {
            &[]
        }

        fn severity(&self) -> DiagnosticSeverity {
            DiagnosticSeverity::Warning
        }

        fn span(&self) -> Option<CanonicalSourceSpan> {
            None
        }
    }

    struct TestDiagnosticReceiver {
        triggered: Cell<bool>,
    }

    impl DiagnosticSink for TestDiagnosticReceiver {
        fn handle_diagnostic(&self, _diagnostics: &dyn Diagnostic) {
            // panic!("Hello!");
            self.triggered.set(true);
        }
    }

    impl Drop for TestDiagnosticReceiver {
        fn drop(&mut self) {
            assert!(self.triggered.get());
        }
    }

    #[test]
    fn it_works() {
        let entry = TheCompiler::new();
        let tdr = TestDiagnosticReceiver {
            triggered: Cell::new(false),
        };

        let a = entry
            .scanners(|s| {
                s.ir1_scanner.push(PreOrPost::Pre(Box::new(MyScanner)));
            })
            .register_diagnostic_receiver(Box::new(tdr))
            .compile("var foo = 1i32\n");

        assert!(a.is_ok());
    }
}
