#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
#[repr(transparent)]
pub struct Utf8CharBoundaryStartByte(usize);

impl Utf8CharBoundaryStartByte {
    pub fn new(byte: usize) -> Self {
        Self(byte)
    }

    pub fn as_usize(&self) -> usize {
        self.0
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
    pub fn as_usize(&self) -> usize {
        *self as u8 as usize
    }
}

impl From<Utf8CharStride> for u8 {
    fn from(value: Utf8CharStride) -> Self {
        value as u8
    }
}

impl TryFrom<u8> for Utf8CharStride {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::One),
            2 => Ok(Self::Two),
            3 => Ok(Self::Three),
            4 => Ok(Self::Four),
            _ => Err(())
        }
    }
}
