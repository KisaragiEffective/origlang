#![deny(clippy::all, clippy::panicking_unwrap, clippy::panic)]
#![warn(clippy::pedantic, clippy::nursery)]

pub mod ir0;
pub mod ir1;
pub mod preset;
pub mod lower;
