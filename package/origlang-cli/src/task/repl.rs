use std::fmt::{Debug, Display};
use std::io::{stdout, Write};
use std::ops::Range;
use ariadne::{Report, ReportKind, Source};
use crate::task::Task;
use crate::error::AllError;
use origlang_compiler::lexer::Token;
use origlang_platform::CTRL_D_NL;
use origlang_compiler::parser::{IntermediateStateCandidate, Parser, ParserError, PartiallyParseFixCandidate};
use origlang_runtime::{PrintToStdout, Runtime, TypeBox};
use origlang_compiler::type_check::TypeChecker;

struct Dummy((), Source);

impl ariadne::Cache<()> for Dummy {
    fn fetch(&mut self, _id: &()) -> Result<&Source, Box<dyn Debug + '_>> {
        Ok(&self.1)
    }

    fn display<'a>(&self, _id: &'a ()) -> Option<Box<dyn Display + 'a>> {
        None
    }
}

pub struct Repl;

impl Repl {
    fn type_box_to_final_evaluated_form(tb: &TypeBox) -> String {
        match tb {
            TypeBox::NonCoercedInteger(i) => i.to_string(),
            TypeBox::Int8(i) => i.to_string(),
            TypeBox::Int16(i) => i.to_string(),
            TypeBox::Int32(i) => i.to_string(),
            TypeBox::Int64(i) => i.to_string(),
            TypeBox::Boolean(b) => b.to_string(),
            TypeBox::String(s) => format!(r#""{s}""#),
            TypeBox::Unit(_) => "()".to_string(),
            TypeBox::Tuple(tp) => {
                let elements = tp.boxes.iter().map(Self::type_box_to_final_evaluated_form).collect::<Vec<_>>().join(", ");
                format!("({elements})")
            }
        }
    }
}

impl Task for Repl {
    type Environment = ();
    type Error = AllError;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        let mut line_count = 1;
        let runtime = Runtime::create(PrintToStdout);
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
                    runtime.execute(ast);
                }
                Err(error_message) => {
                    let error = error_message.kind;
                    let mut handled = false;
                    if let ParserError::PartiallyParsed { hint, intermediate_state } = &error {
                        // TODO: insta-expression eval
                        if hint.len() == 1 {
                            if let PartiallyParseFixCandidate::InsertBefore { tokens } = &hint[0] {
                                if tokens.len() == 1 && tokens[0] == Token::KeywordPrint && intermediate_state.len() == 1 {
                                    let IntermediateStateCandidate::Expression(expression) = &intermediate_state[0];
                                    checker.check(expression)?;
                                    let value = runtime.evaluate(expression)?;

                                    println!(
                                        "=> {value} : {t}",
                                        value = Repl::type_box_to_final_evaluated_form(&value),
                                        t = value.get_type());
                                    handled = true;
                                }
                            }
                        }
                    }

                    if !handled {
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
                            ParserError::PartiallyParsed { .. } => 10,
                            ParserError::InsufficientElementsForTupleLiteral(_) => 11,
                        };

                        let d = Report::<Range<usize>>::build(ReportKind::Error, (), error_offset)
                            .with_code(format!("E{error_code}"))
                            .with_message(error.to_string())
                            .finish();

                        d.write(Dummy((), Source::from(line)), std::io::stderr()).expect("TODO: panic message");
                    }
                }
            }
            line_count += 1;
        }

        Ok(())
    }
}