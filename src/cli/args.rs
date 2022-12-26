use std::path::PathBuf;
use clap::{Parser, Subcommand};
use crate::cli::task::print_ast::{ParseSource, PrintAst};
use crate::cli::task::test::Test;
use crate::cli::task::interpret::Interpret;
use crate::cli::task::lexer_dump::LexerDump;
use crate::cli::task::repl::Repl;
use crate::cli::task::Task;
use crate::error::AllError;


#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    sub_command: SubCom
}

impl Args {
    pub fn execute(self) -> Result<(), AllError> {
        match self.sub_command {
            SubCom::Repl => {
                let task = Repl;
                task.execute(())?;
                Ok(())
            }
            SubCom::Execute { input_file, input_source } => {
                let task = Interpret;
                let source = input_file.map_or_else(|| input_source.map_or_else(|| unreachable!("oops"), ParseSource::RawSource), ParseSource::FromFile);

                task.execute(source)?;
                Ok(())
            }
            SubCom::Ast { input_file, input_source } => {
                let task = PrintAst;
                let source = input_file.map_or_else(|| input_source.map_or_else(|| unreachable!("oops"), ParseSource::RawSource), ParseSource::FromFile);

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
                let source = input_file.map_or_else(|| if let Some(input_source) = input_source {
                    ParseSource::RawSource(input_source)
                } else {
                    unreachable!("oops")
                }, ParseSource::FromFile);

                task.execute(source)?;
                Ok(())
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
