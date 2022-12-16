use std::fmt::{Debug, Display};
use std::io::{stdout, Write};
use std::ops::Range;
use ariadne::{Report, ReportKind, Source};
use crate::cli::task::Task;
use crate::platform::CTRL_D_NL;
use crate::parser::{Parser, ParserError};
use crate::runtime::Runtime;
use crate::type_check::error::TypeCheckError;
use crate::type_check::TypeChecker;

pub struct Repl;

impl Task for Repl {
    type Environment = ();
    type Error = TypeCheckError;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        let mut line_count = 1;
        let runtime = Runtime::create();
        println!("Welcome to REPL!");
        let checker = TypeChecker::new();
        loop {
            print!("REPL:{line_count:03}> ");
            stdout().flush().unwrap();
            let line = {
                let mut buf = String::new();
                std::io::stdin().read_line(&mut buf).expect("??");
                buf
            };
            dbg!(&line);
            // FIXME: this is buggy in Windows, causing infinite loop
            if line == CTRL_D_NL {
                break
            }
            let parser = Parser::create(line.as_str());
            match parser.parse() {
                Ok(ast) => {
                    checker.check(&ast)?;
                    runtime.execute(&ast);
                }
                Err(error_message) => {
                    let error = error_message.kind;
                    let error_offset = 0;
                    let error_code = match error {
                        ParserError::LexerError(_) => 1,
                        ParserError::UnconsumedToken { .. } => 2,
                        ParserError::StatementTerminationError => 3,
                        ParserError::EndOfFileError => 4,
                        ParserError::UnexpectedToken { .. } => 5,
                        ParserError::UnParsableIntLiteral { .. } => 6,
                        ParserError::OverflowedLiteral { .. } => 7,
                        ParserError::IfExpressionWithoutElseClause => 8,
                        ParserError::IfExpressionWithoutThenClauseAndElseClause => 9,
                    };

                    let d = Report::<Range<usize>>::build(ReportKind::Error, (), error_offset)
                        .with_code(format!("E{error_code}"))
                        .with_message(error.to_string())
                        .finish();

                    struct Dummy((), Source);

                    impl ariadne::Cache<()> for Dummy {
                        fn fetch(&mut self, id: &()) -> Result<&Source, Box<dyn Debug + '_>> {
                            Ok(&self.1)
                        }

                        fn display<'a>(&self, id: &'a ()) -> Option<Box<dyn Display + 'a>> {
                            None
                        }
                    }

                    d.write(Dummy((), Source::from(line)), std::io::stderr()).expect("TODO: panic message");
                }
            }
            line_count += 1;
        }

        Ok(())
    }
}