#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::fmt::{Display, Formatter};
use derive_more::Display;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ast::Identifier;

// TODO: this is implementation detail, should be unreachable.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct DisplayTupleType(pub Vec<Type>);

impl Display for DisplayTupleType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let content = self.0.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let output = format!("({content})");

        f.write_str(&output)
    }
}

impl From<Vec<Type>> for DisplayTupleType {
    fn from(value: Vec<Type>) -> Self {
        Self(value)
    }
}

// TODO: this is implementation detail, should be unreachable.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct DisplayRecordType {
    identifier: Identifier,
    components: Vec<Type>,
}

impl DisplayRecordType {
    #[must_use] pub fn new(identifier: Identifier, components: Vec<Type>) -> Self {
        Self {
            identifier, components
        }
    }
}
impl Display for DisplayRecordType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let serialized_component = self.components.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let identifier = &self.identifier;
        let output = format!("{identifier} {{{serialized_component}}}");

        f.write_str(&output)
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
    Tuple(DisplayTupleType),
    #[display(fmt = "{_0}")]
    Record(DisplayRecordType),
    #[display(fmt = "*const {_0}")]
    Ptr(Box<Self>),
    #[display(fmt = "*mut {_0}")]
    PtrMut(Box<Self>),
    /// same as [`Self::Ptr`], but requires to be properly aligned and not to be null even if
    /// value of this type is never dereferenced.
    #[display(fmt = "&{_0}")]
    Ref(Box<Self>),
    /// same as [`Self::PtrMut`], but requires to be properly aligned and not to be null even if
    /// value of this type is never dereferenced.
    #[display(fmt = "&mut {_0}")]
    RefMut(Box<Self>),
}

impl Type {
    #[must_use] pub const fn is_int_family(&self) -> bool {
        matches!(self, Self::GenericInteger | Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }

    #[must_use] pub fn tuple(tuple_elements: Vec<Self>) -> Self {
        Self::Tuple(DisplayTupleType(tuple_elements))
    }
    
    #[must_use] pub fn is_assignable(&self, from: &Self) -> AssignableQueryAnswer {
        if self.is_int_family() && from == &Self::GenericInteger {
            AssignableQueryAnswer::PossibleIfCoerceSourceImplicitly
        } else if self == from {
            AssignableQueryAnswer::Yes
        } else {
            AssignableQueryAnswer::No
        }
    }

    #[must_use] pub const fn as_tuple(&self) -> Option<&DisplayTupleType> {
        if let Self::Tuple(x) = self {
            Some(x)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum AssignableQueryAnswer {
    Yes,
    PossibleIfCoerceSourceImplicitly,
    No,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypedRootAst {
    pub statements: Vec<TypedStatement>
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TypedStatement {
    Print {
        expression: TypedExpression,
    },
    VariableDeclaration {
        identifier: Identifier,
        expression: TypedExpression,
    },
    VariableAssignment {
        identifier: Identifier,
        expression: TypedExpression,
    },
    Block {
        inner_statements: Vec<Self>
    },
    EvalAndForget {
        expression: TypedExpression,
    },
    Exit,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TypedExpression {
    IntLiteral(TypedIntLiteral),
    BooleanLiteral(bool),
    StringLiteral(String),
    UnitLiteral,
    Variable {
        ident: Identifier,
        tp: Type,
    },
    BinaryOperator {
        lhs: Box<Self>,
        rhs: Box<Self>,
        operator: BinaryOperatorKind,
        return_type: Type,
    },
    If {
        condition: Box<Self>,
        then: Box<Self>,
        els: Box<Self>,
        return_type: Type
    },
    Block {
        inner: Vec<TypedStatement>,
        final_expression: Box<Self>,
        return_type: Type,
    },
    Tuple {
        expressions: Vec<Self>
    },
    ExtractTuple {
        expr: Box<Self>,
        index: usize
    },
}

impl TypedExpression {
    #[must_use] pub fn actual_type(&self) -> Type {
        match self {
            Self::IntLiteral(i) => i.actual_type(),
            Self::BooleanLiteral(_) => Type::Boolean,
            Self::StringLiteral(_) => Type::String,
            Self::UnitLiteral => Type::Unit,
            Self::Variable { tp, .. } => tp.clone(),
            Self::BinaryOperator { return_type, .. } | Self::If { return_type, .. } | Self::Block { return_type, .. } => return_type.clone(),
            Self::Tuple { expressions } => Type::tuple(expressions.iter().map(Self::actual_type).collect()),
            Self::ExtractTuple { expr, index } => {
                expr.actual_type().as_tuple().map(|y| y.0[*index].clone()).expect("the underlying expression must be tuple and index must be within its bound")
            }
        }
    }

    #[must_use] pub fn tuple_arity(&self) -> Option<usize> {
        if let Self::Tuple { expressions } = self {
            Some(expressions.len())
        } else {
            None
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TypedIntLiteral {
    Generic(i64),
    Bit64(i64),
    Bit32(i32),
    Bit16(i16),
    Bit8(i8),
}

impl TypedIntLiteral {
    #[must_use] pub const fn actual_type(&self) -> Type {
        match self {
            Self::Generic(_) => Type::GenericInteger,
            Self::Bit64(_) => Type::Int64,
            Self::Bit32(_) => Type::Int32,
            Self::Bit16(_) => Type::Int16,
            Self::Bit8(_) => Type::Int8,
        }
    }
}
