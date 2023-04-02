use std::fmt::{Display, Formatter};
use derive_more::Display;

// TODO: this is implementation detail, should be unreachable.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TupleDisplay(pub Vec<Type>);

impl Display for TupleDisplay {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let content = self.0.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let output = format!("({content})");

        f.write_str(&output)
    }
}

impl From<Vec<Type>> for TupleDisplay {
    fn from(value: Vec<Type>) -> Self {
        Self(value)
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Display)]
pub enum Type {
    #[display(fmt = "{{integer}}")]
    GenericInteger,
    #[display(fmt = "Bool")]
    Boolean,
    #[display(fmt = "String")]
    String,
    #[display(fmt = "Unit")]
    Unit,
    #[display(fmt = "Int8")]
    Int8,
    #[display(fmt = "Int16")]
    Int16,
    #[display(fmt = "Int32")]
    Int32,
    #[display(fmt = "Int64")]
    Int64,
    #[display(fmt = "{_0}")]
    Tuple(TupleDisplay),
}

impl Type {
    pub const fn is_int_family(&self) -> bool {
        matches!(self, Self::GenericInteger | Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }
}
