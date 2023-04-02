#[cfg(windows)]
pub mod windows;

#[cfg(not(windows))]
pub mod non_windows;
