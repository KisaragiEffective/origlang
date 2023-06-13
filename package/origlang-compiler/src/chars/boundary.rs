use std::hint::unreachable_unchecked;
use thiserror::Error;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Utf8CharBoundaryStartByte(usize);

impl Utf8CharBoundaryStartByte {
    pub const fn new(byte: usize) -> Self {
        Self(byte)
    }

    pub const fn as_usize(self) -> usize {
        self.0
    }

    pub const fn stride(self, stride: Utf8CharStride) -> Utf8CharBoundaryStartByte {
        Self::new(self.as_usize() + stride.as_usize())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
#[repr(u8)]
pub enum Utf8CharStride {
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
}

impl Utf8CharStride {
    pub const fn as_usize(self) -> usize {
        self as u8 as usize
    }
}

impl From<Utf8CharStride> for u8 {
    fn from(value: Utf8CharStride) -> Self {
        value as Self
    }
}

/// Convert any unicode codepoint into its stride.
impl From<char> for Utf8CharStride {
    fn from(value: char) -> Self {
        // SAFETY: len_utf8 value is in 1..=4, fits u8.
        let res = unsafe { u8::try_from(value.len_utf8()).unwrap_unchecked() };
        // SAFETY: <Self as TryFrom<u8>>::try_from never returns error if the given value is in 1..=4.
        match res {
            1 => Self::One,
            2 => Self::Two,
            3 => Self::Three,
            4 => Self::Four,
            // SAFETY: this branch is actually not reachable because of res' range.
            _ => unsafe { unreachable_unchecked() }
        }
    }
}

#[derive(Error, Debug)]
#[error("invalid value for UTF-8 codepoint stride: {given_value}")]
pub struct InvalidUtf8CharStrideError {
    given_value: u8
}
