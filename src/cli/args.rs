use std::path::PathBuf;
use clap::{Parser, Subcommand};
use crate::cli::task::print_ast::*;
use crate::cli::task::test::Test;
use crate::cli::task::interpret::Interpret;
use crate::cli::task::Task;

#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    sub_command: SubCom
}

impl Args {
    fn execute(self) -> Result<(), String> {
        match self.sub_command {
            SubCom::Repl => {
                Err("currently not implemented".to_string())
            }
            SubCom::Execute { input_file, input_source } => {
                let task = Interpret;
                let source = if let Some(input_file) = input_file {
                    ParseSource::FromFile(input_file)
                } else if let Some(input_source) = input_source {
                    ParseSource::RawSource(input_source)
                } else {
                    unreachable!("oops")
                };

                task.execute(source)
            }
            SubCom::Ast { input_file, input_source } => {
                let task = PrintAst;
                let source = if let Some(input_file) = input_file {
                    ParseSource::FromFile(input_file)
                } else if let Some(input_source) = input_source {
                    ParseSource::RawSource(input_source)
                } else {
                    unreachable!("oops")
                };

                task.execute(source)
            }
            SubCom::Test => {
                let task = Test;
                task.execute(())
            }
        }
    }
}

#[derive(Subcommand)]
pub enum SubCom {
    Repl,
    Execute {
        #[clap(long, group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(long, group = "evaluate_source")]
        input_source: Option<String>
    },
    Ast {
        #[clap(long, group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(long, group = "evaluate_source")]
        input_source: Option<String>
    },
    Test
}
