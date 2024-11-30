use crate::error::TaskExecutionError;
use crate::task::Task;
use ariadne::{Report, ReportKind, Source};
use origlang_ir::{IntoVerbatimSequencedIR, IR2};
use origlang_ir_optimizer::lower::{LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::NoOptimization;
use origlang_parser::error::ParserErrorInner;
use origlang_parser::parser::Parser;
use origlang_platform::CTRL_D_NL;
use origlang_runtime::{PrintToStdout, Runtime};
use origlang_typecheck::type_check::TypeChecker;
use origlang_typesystem_model::TypedRootAst;
use std::fmt::{Debug, Display};
use std::io::{stdout, Write};
use std::ops::Range;

struct Dummy((), Source);

impl ariadne::Cache<()> for Dummy {
    type Storage = String;

    fn fetch(&mut self, _id: &()) -> Result<&Source, Box<dyn Debug + '_>> {
        Ok(&self.1)
    }

    fn display<'a>(&self, _id: &'a ()) -> Option<Box<dyn Display + 'a>> {
        None
    }
}

pub struct Repl;

impl Repl {
    fn naive_lower(tra: TypedRootAst) -> Vec<IR2> {
        let ir = tra.into_ir();
        let trans = TheTranspiler::new(&NoOptimization);

        trans.lower(ir)
    }
}

impl Task for Repl {
    type Environment = ();
    type Error = TaskExecutionError;

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
                break;
            }
            let parser = Parser::create(line.as_str());
            match parser.parse() {
                Ok(ast) => {
                    let ast = checker.check(ast)?;
                    runtime.start(&Self::naive_lower(ast));
                }
                Err(error_message) => {
                    let error = error_message.kind();
                    let handled = false;
                    if let ParserErrorInner::PartiallyParsed {
                        hint: _,
                        intermediate_state: _,
                    } = &error
                    {
                        // TODO: insta-expression eval
                        // TODO: revisit this
                        /*
                        if hint.len() == 1 {
                            if let PartiallyParseFixCandidate::InsertBefore { tokens } = &hint[0] {
                                if tokens.len() == 1 && tokens[0] == Token::KeywordPrint && intermediate_state.len() == 1 {
                                    let IntermediateStateCandidate::Expression(expression) = &intermediate_state[0];
                                    let expression = checker.check(expression.clone())?;
                                    let value = runtime.evaluate(&expression)?;

                                    println!(
                                        "=> {value} : {t}",
                                        value = Self::type_box_to_final_evaluated_form(&value),
                                        t = value.get_type());
                                    handled = true;
                                }
                            }
                        }
                        */
                    }

                    if !handled {
                        let error_offset = 0;
                        let error_code = match error {
                            ParserErrorInner::LexerError(_) => 1,
                            ParserErrorInner::UnconsumedToken { .. } => 2,
                            ParserErrorInner::StatementTerminationError => 3,
                            ParserErrorInner::EndOfFileError => 4,
                            ParserErrorInner::UnexpectedToken { .. } => 5,
                            ParserErrorInner::UnParsableIntLiteral { .. } => 6,
                            ParserErrorInner::OverflowedLiteral { .. } => 7,
                            ParserErrorInner::IfExpressionWithoutElseClause => 8,
                            ParserErrorInner::IfExpressionWithoutThenClauseAndElseClause => 9,
                            ParserErrorInner::PartiallyParsed { .. } => 10,
                            ParserErrorInner::InsufficientElementsForTupleLiteral(_) => 11,
                            ParserErrorInner::UnderscoreCanNotBeRightHandExpression => 12,
                        };

                        let d = Report::<Range<usize>>::build(ReportKind::Error, (), error_offset)
                            .with_code(format!("E{error_code}"))
                            .with_message(error.to_string())
                            .finish();

                        d.write(Dummy((), Source::from(line)), std::io::stderr())
                            .expect("TODO: panic message");
                    }
                }
            }
            line_count += 1;
        }

        Ok(())
    }
}
