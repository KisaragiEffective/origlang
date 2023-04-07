use std::ops::{Add, Index, Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive, Sub};
use std::slice::Iter;

/// efficient and fast alternative for caching `String::chars`.
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct MultiByteBoundaryAwareString {
    str: String,
    boundaries: Vec<(Utf8CharBoundaryStartByte, Utf8CharStride)>,
}

impl MultiByteBoundaryAwareString {
    pub fn new(s: String) -> Self {
        let intermediate = s.chars().enumerate().fold(
            // page fault, do not copy nor give dummy value to be copied (dummy zeroing does not have effect)
            (Utf8CharBoundaryStartByte(0), Vec::with_capacity(s.chars().count())),
            |(char_position_as_bytes, mut acc), (n, c)| {
                let char_byte_width = c.len_utf8();
                // SAFETY: len_utf8 always return 1..=4; u8 can hold all values, so this will always succeed.
                let char_byte_width = unsafe { u8::try_from(char_byte_width).unwrap_unchecked() };
                // SAFETY: len_utf8 always return 1..=4; u8 can hold all values, so this will always succeed.
                let char_byte_width = unsafe { Utf8CharStride::try_from(char_byte_width).unwrap_unchecked() };
                acc.push((char_position_as_bytes, char_byte_width));
                (Utf8CharBoundaryStartByte(char_position_as_bytes.0 + (char_byte_width as u8 as usize)), acc)
            }
        ).1;

        MultiByteBoundaryAwareString {
            str: s,
            boundaries: intermediate,
        }
    }

    pub fn nth_char(&self, index: PositionInChars) -> Option<char> {
        let byte_view = self.str.as_bytes();
        let (start, stride) = self.boundaries.get(index.0)?;
        let sub_slice = &byte_view[create_valid_range_for_utf8_boundary(*start, *start, *stride)];
        // SAFETY: above subslice is guaranteed to be a valid UTF-8 byte sequence.
        let tmp = unsafe { core::str::from_utf8_unchecked(sub_slice) };
        let c_opt = tmp.chars().next();

        c_opt
    }

    pub fn boundaries(&self) -> Iter<'_, (Utf8CharBoundaryStartByte, Utf8CharStride)> {
        self.boundaries.iter()
    }

    pub fn boundary_to_char_position(&self, boundary: Utf8CharBoundaryStartByte) -> Option<PositionInChars> {
        self.boundary_to_char_position_and_stride(boundary).map(|x| x.0)
    }

    pub fn boundary_to_char_position_and_stride(&self, boundary: Utf8CharBoundaryStartByte) -> Option<(PositionInChars, Utf8CharStride)> {
        // this method would be succeed :)
        let boundary_nth = self.boundaries.binary_search_by_key(&boundary, |cb| cb.0).ok()?;

        Some((PositionInChars(boundary_nth), self.boundaries[boundary_nth].1))
    }

    pub fn count_char(&self) -> usize {
        self.boundaries.len()
    }
}

fn create_valid_range_for_utf8_boundary(
    start: Utf8CharBoundaryStartByte,
    end_base: Utf8CharBoundaryStartByte,
    end_offset: Utf8CharStride
) -> Range<usize> {
    (start.0)..(end_base.0 + end_offset.as_usize())
}

// Index implementation, whose indexers is usize (or its range) and output is char (or str, respectively).
// those impls corresponds calling s.chars().nth(_).next() or
// taking its sub-slice.

// m[i..j]
impl Index<Range<PositionInChars>> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, index: Range<PositionInChars>) -> &Self::Output {
        let start = index.start;
        let end_inclusive = PositionInChars::new(index.end.as_usize() - 1);

        &self[start..=end_inclusive]
    }
}

// m[i..]
impl Index<RangeFrom<PositionInChars>> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, index: RangeFrom<PositionInChars>) -> &Self::Output {
        let start = index.start;
        let end_inclusive = PositionInChars(self.boundaries.len() - 1);

        &self[start..=end_inclusive]
    }
}

// m[..]
impl Index<RangeFull> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, _: RangeFull) -> &Self::Output {
        &self.str
    }
}

// m[i..=j] (mail implementation)
impl Index<RangeInclusive<PositionInChars>> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, index: RangeInclusive<PositionInChars>) -> &Self::Output {
        let start_char_pos = index.start().as_usize();
        let end_char_pos = index.end().as_usize();

        if start_char_pos == end_char_pos {
            let i = start_char_pos;
            let (byte_pos, stride) = self.boundaries[i];
            let sub_slice = &self.str.as_bytes()[create_valid_range_for_utf8_boundary(byte_pos, byte_pos, stride)];

            // SAFETY: above subslice is guaranteed to be a valid UTF-8 byte sequence.
            unsafe { core::str::from_utf8_unchecked(sub_slice) }
        } else {
            let boundaries = &self.boundaries[start_char_pos..=end_char_pos];
            if boundaries.is_empty() {
                // this is empty
                let sub_slice = &[];
                // SAFETY: empty sequence is valid UTF-8.
                unsafe { core::str::from_utf8_unchecked(sub_slice) }
            } else {
                // SAFETY: first returns None if and only if the underlying vector is empty.
                // However, we've checked if the vector is empty, so this call never fails.
                let (first_boundary, _) = unsafe { boundaries.first().unwrap_unchecked() };
                // SAFETY: last returns None if and only if the underlying vector is empty.
                // However, we've checked if the vector is empty, so this call never fails.
                let (last_boundary, last_stride) = unsafe { boundaries.last().unwrap_unchecked() };
                let sub_slice = &self.str.as_bytes()[create_valid_range_for_utf8_boundary(
                    *first_boundary, *last_boundary, *last_stride
                )];

                // SAFETY: above subslice is guaranteed to be a valid UTF-8 byte sequence.
                unsafe { core::str::from_utf8_unchecked(sub_slice) }
            }
        }
    }
}

// m[..j]
impl Index<RangeTo<PositionInChars>> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, index: RangeTo<PositionInChars>) -> &Self::Output {
        let start = PositionInChars::new(0);
        let end_inclusive = PositionInChars::new(index.end.as_usize() - 1);

        &self[start..=end_inclusive]
    }
}

// m[..=j]
impl Index<RangeToInclusive<PositionInChars>> for MultiByteBoundaryAwareString {
    type Output = str;

    fn index(&self, index: RangeToInclusive<PositionInChars>) -> &Self::Output {
        let start = PositionInChars::new(0);
        let end_inclusive = index.end;

        &self[start..=end_inclusive]
    }
}

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
    fn as_usize(&self) -> usize {
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
#[repr(transparent)]
pub struct PositionInChars(usize);

impl PositionInChars {
    pub fn new(byte: usize) -> Self {
        Self(byte)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl Add<PositionInChars> for PositionInChars {
    type Output = PositionInChars;

    fn add(self, rhs: PositionInChars) -> Self::Output {
        Self::new(self.as_usize() + rhs.as_usize())
    }
}

impl Add<PositionStepBetweenChars> for PositionInChars {
    type Output = PositionInChars;

    fn add(self, rhs: PositionStepBetweenChars) -> Self::Output {
        Self::new(self.as_usize() + rhs.as_usize())
    }
}

impl Sub<PositionInChars> for PositionInChars {
    type Output = PositionStepBetweenChars;

    fn sub(self, rhs: PositionInChars) -> Self::Output {
        PositionStepBetweenChars::new(self.as_usize() - rhs.as_usize())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Ord, PartialOrd)]
pub struct PositionStepBetweenChars(usize);

impl PositionStepBetweenChars {
    pub fn new(v: usize) -> Self {
        Self(v)
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use crate::chars::boundary::{MultiByteBoundaryAwareString, PositionInChars, Utf8CharBoundaryStartByte, Utf8CharStride};

    fn char_boundary_and_stride(s: &str) -> Vec<(usize, usize)> {
        MultiByteBoundaryAwareString::new(s.to_string()).boundaries()
            .map(|(a, b)| (a.as_usize(), b.as_usize())).collect()
    }

    #[test]
    fn empty() {
        assert_eq!(char_boundary_and_stride(""), vec![]);
    }

    #[test]
    fn byte_1_lower_bound() {
        assert_eq!(char_boundary_and_stride("\u{0000}"), vec![(0, 1)]);
    }

    #[test]
    fn byte_1_upper_bound() {
        assert_eq!(char_boundary_and_stride("\u{007F}"), vec![(0, 1)]);
    }

    #[test]
    fn byte_2_lower_bound() {
        assert_eq!(char_boundary_and_stride("\u{0080}"), vec![(0, 2)]);
    }

    #[test]
    fn byte_2_upper_bound() {
        assert_eq!(char_boundary_and_stride("\u{07FF}"), vec![(0, 2)]);
    }

    #[test]
    fn byte_3_lower_bound() {
        assert_eq!(char_boundary_and_stride("\u{0800}"), vec![(0, 3)]);
    }

    #[test]
    fn byte_3_upper_bound() {
        assert_eq!(char_boundary_and_stride("\u{FFFF}"), vec![(0, 3)]);
    }

    #[test]
    fn byte_4_lower_bound() {
        assert_eq!(char_boundary_and_stride("\u{10000}"), vec![(0, 4)]);
    }

    #[test]
    fn byte_4_upper_bound() {
        assert_eq!(char_boundary_and_stride("\u{10FFFF}"), vec![(0, 4)]);
    }

    #[test]
    fn typical_japanese() {
        assert_eq!(char_boundary_and_stride("あいうえお"), vec![(0, 3), (3, 3), (6, 3), (9, 3), (12, 3)])
    }

    fn find_boundary(s: &str, b: usize) -> Option<(usize, usize)> {
        let rope = MultiByteBoundaryAwareString::new(s.to_string());
        println!("{rope:?}");

        rope
            .boundary_to_char_position_and_stride(Utf8CharBoundaryStartByte(b))
            .map(|(b, s)| (b.as_usize(), s.as_usize()))
    }

    #[test]
    fn find_boundary_1_pos() {
        assert_eq!(find_boundary("123", 0), Some((0, 1)));
        assert_eq!(find_boundary("123", 1), Some((1, 1)));
        assert_eq!(find_boundary("123", 2), Some((2, 1)));
    }

    #[test]
    fn find_boundary_3_pos() {
        assert_eq!(find_boundary("あいう", 0), Some((0, 3)));
        assert_eq!(find_boundary("あいう", 3), Some((1, 3)));
        assert_eq!(find_boundary("あいう", 6), Some((2, 3)));
    }

    #[test]
    fn find_boundary_3_neg() {
        assert_eq!(find_boundary("あいう", 1), None);
        assert_eq!(find_boundary("あいう", 2), None);
        assert_eq!(find_boundary("あいう", 4), None);
        assert_eq!(find_boundary("あいう", 5), None);
        assert_eq!(find_boundary("あいう", 7), None);
        assert_eq!(find_boundary("あいう", 8), None);
    }

    #[test]
    fn find_boundary_mixed_1_pos() {
        assert_eq!(find_boundary("あいう123", 0), Some((0, 3)));
        assert_eq!(find_boundary("あいう123", 3), Some((1, 3)));
        assert_eq!(find_boundary("あいう123", 6), Some((2, 3)));
        assert_eq!(find_boundary("あいう123", 9), Some((3, 1)));
        assert_eq!(find_boundary("あいう123", 10), Some((4, 1)));
        assert_eq!(find_boundary("あいう123", 11), Some((5, 1)));
    }

    #[test]
    fn find_boundary_mixed_1_neg() {
        assert_eq!(find_boundary("あいう123", 1), None);
        assert_eq!(find_boundary("あいう123", 2), None);
        assert_eq!(find_boundary("あいう123", 4), None);
        assert_eq!(find_boundary("あいう123", 5), None);
        assert_eq!(find_boundary("あいう123", 7), None);
        assert_eq!(find_boundary("あいう123", 8), None);
    }
}
