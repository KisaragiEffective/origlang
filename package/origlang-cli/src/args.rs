// clap issue?: https://github.com/clap-rs/clap/issues/4733
#![warn(clippy::almost_swapped)]

use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;
use clap::{Parser, Subcommand};
use strum::EnumString;
use crate::task::test::Test;
use crate::task::interpret::Interpret;
use crate::task::repl::Repl;
use crate::task::Task;
use crate::error::TaskExecutionError;
use crate::task::emit::UnstableEmit;


#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    sub_command: SubCom
}

pub enum ParseSource {
    RawSource(String),
    FromFile(PathBuf)
}

impl ParseSource {
    pub fn source(&self) -> String {
        match self {
            Self::RawSource(a) => a.clone(),
            Self::FromFile(path) => {
                let mut buf = String::new();
                BufReader::new(File::open(path).unwrap()).read_to_string(&mut buf).unwrap();
                buf
            }
        }
    }
}


impl Args {
    pub fn execute(self) -> Result<(), TaskExecutionError> {

        let load_either = |input_file: Option<PathBuf>, input_source: Option<String>| {
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
                let source = load_either(input_file, input_source);
                task.execute(source)?;
                Ok(())
            }
            SubCom::Emit { emit, input_file, input_source } => {
                let task = UnstableEmit { phase: emit };
                let source = load_either(input_file, input_source);

                task.execute(source)?;
                Ok(())
            }
            SubCom::Test => {
                let task = Test;
                task.execute(()).map_err(TaskExecutionError::from)?;
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
    /// Emits unstable intermediate representation for only debug purposes.
    Emit {
        #[clap(long)]
        emit: EmitPhase,
        #[clap(long, group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(long, group = "evaluate_source")]
        input_source: Option<String>,
    },
    Test
}

#[derive(EnumString)]
#[strum(serialize_all = "snake_case")]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EmitPhase {
    LexerToken,
    Ast,
    TypedAst,
    Ir0,
    Ir1,
    OptimizedIr1,
}
