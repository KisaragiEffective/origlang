mod platform;

#[cfg(windows)]
pub use platform::windows::*;

#[cfg(not(windows))]
pub use platform::non_windows::*;
