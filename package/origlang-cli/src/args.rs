// clap issue?: https://github.com/clap-rs/clap/issues/4733
#![warn(clippy::almost_swapped)]

use std::num::NonZeroUsize;
use std::path::PathBuf;
use std::string::FromUtf8Error;
use clap::{Parser, Subcommand};
use strum::EnumString;
use thiserror::Error;
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

#[derive(Debug, Error)]
pub enum ReadSourceError {
    #[error("Invalid UTF-8 was given: {0}")]
    MalformedUtf8Sequence(#[from] FromUtf8Error),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

impl ParseSource {
    pub fn source(&self) -> Result<String, ReadSourceError> {
        match self {
            Self::RawSource(a) => Ok(a.clone()),
            Self::FromFile(path) => {
                Ok(std::fs::read_to_string(path)?)
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
            SubCom::Execute { input_file, input_source, max_stack_size_on_main_thread } => {
                let task = Interpret;
                let source = load_either(input_file, input_source);
                if let Some(ssize) = max_stack_size_on_main_thread {
                    let a = std::thread::scope(move |a| {
                        std::thread::Builder::new().stack_size(ssize.get()).spawn_scoped(a, move || {
                            task.execute(source)
                        }).unwrap().join()
                    });
                    a.map_err(TaskExecutionError::ThreadJoin)??;
                } else {
                    task.execute(source)?;
                }
                Ok(())
            }
            SubCom::Emit { emit, input_file, input_source, optimize_level } => {
                let task = UnstableEmit { phase: emit };
                let source = load_either(input_file, input_source);

                task.execute((source, optimize_level))?;
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
        input_source: Option<String>,
        #[clap(long)]
        max_stack_size_on_main_thread: Option<NonZeroUsize>,
    },
    /// Emits unstable intermediate representation for only debug purposes.
    Emit {
        #[clap(long)]
        emit: EmitPhase,
        #[clap(long)]
        optimize_level: OptimizeLevel,
        #[clap(long, group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(long, group = "evaluate_source")]
        input_source: Option<String>,
    },
}

#[derive(EnumString)]
#[strum(serialize_all = "snake_case")]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum EmitPhase {
    LexerToken,
    Ast,
    TypedAst,
    Ir1,
    Ir2,
}

#[derive(EnumString)]
#[strum(serialize_all = "snake_case")]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum OptimizeLevel {
    None,
    Basic,
}
