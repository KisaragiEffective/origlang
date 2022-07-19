use std::path::PathBuf;
use clap::{Parser, Subcommand};
#[derive(Parser)]
pub struct Args {
    #[clap(subcommand)]
    sub_command: SubCom
}

#[derive(Subcommand)]
pub enum SubCom {
    Repl,
    Execute {
        #[clap(group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(group = "evaluate_source")]
        input_source: Option<String>
    },
    Ast {
        #[clap(group = "evaluate_source")]
        input_file: Option<PathBuf>,
        #[clap(group = "evaluate_source")]
        input_source: Option<String>
    },
    Test
}
