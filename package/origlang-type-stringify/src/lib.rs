#![no_std]

extern crate core as std;
extern crate alloc;

use alloc::string::ToString;
use std::fmt::Formatter;

pub fn write_comma_separated_items<
    'formatter,
    'values,
    T: ToString + 'values
>(f: &mut Formatter<'formatter>, values: &'values [T]) -> std::fmt::Result {
    for value in values.iter().take(values.len().max(1) - 1) {
        f.write_str(&value.to_string())?;
        f.write_str(", ")?;
    }

    values.last().map_or(Ok(()), |last| f.write_str(&last.to_string()))
}