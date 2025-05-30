#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use crate::args::Args;
use crate::error::TaskExecutionError;
use clap::Parser;

mod args;
mod error;
mod task;

#[expect(clippy::result_large_err)] // #739 で急に出てきた
fn main() -> Result<(), TaskExecutionError> {
    let args = Args::parse();
    env_logger::init();

    if let Err(e) = args.execute() {
        log::error!("{e}", e = &e);
        return Err(e);
    }
    Ok(())
}
