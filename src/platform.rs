mod windows;

#[cfg(windows)]
pub use windows::*;

#[cfg(not(windows))]
pub use non_windows::*;
