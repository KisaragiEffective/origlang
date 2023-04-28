// clap issue?: https://github.com/clap-rs/clap/issues/4733
#![warn(clippy::almost_swapped)]

use std::path::PathBuf;
use clap::{Parser, Subcommand};
use crate::task::print_ast::{ParseSource, PrintAst};
use crate::task::test::Test;
use crate::task::interpret::Interpret;
use crate::task::lexer_dump::LexerDump;
use crate::task::repl::Repl;
use crate::task::Task;
use crate::error::TaskExecutionError;


#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    sub_command: SubCom
}

impl Args {
    pub fn execute(self) -> Result<(), TaskExecutionError> {

        let x = |input_file: Option<PathBuf>, input_source: Option<String>| {
            input_file.map_or_else(|| input_source.map_or_else(|| panic!("please specify file or source"), ParseSource::RawSource), ParseSource::FromFile)
        };

        match self.sub_command {
            SubCom::Repl => {
                let task = Repl;
                task.execute(())?;
                Ok(())
            }
            SubCom::Execute { input_file, input_source } => {
                let task = Interpret;
                let source = x(input_file, input_source);
                task.execute(source)?;
                Ok(())
            }
            SubCom::Ast { input_file, input_source } => {
                let task = PrintAst;
                let source = x(input_file, input_source);

                task.execute(source)?;
                Ok(())
            }
            SubCom::Test => {
                let task = Test;
                task.execute(())?;
                Ok(())
            }
            SubCom::LexerDump { input_file, input_source } => {
                let task = LexerDump;
                let source = x(input_file, input_source);

                task.execute(source)?;
                Ok(())
            }
        }
    }
}

#[derive(Subcommand)]
#[group(id = "evaluate_source", multiple = false, required = true)]
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
        input_source: Option<String>,
    },
    LexerDump {
        #[clap(long, group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(long, group = "evaluate_source")]
        input_source: Option<String>,
    },
    Test
}
