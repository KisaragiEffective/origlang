#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use clap::Parser;
use crate::args::Args;
use crate::error::AllError;

mod task;
mod args;
mod error;

fn main() -> Result<(), AllError> {
    let args = Args::parse();
    env_logger::init();

    if let Err(e) = args.execute() {
        log::error!("{e}", e = &e);
        return Err(e)
    }
    Ok(())
}
