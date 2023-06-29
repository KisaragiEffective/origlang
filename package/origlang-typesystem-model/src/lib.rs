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
    pub fn new(identifier: Identifier, components: Vec<Type>) -> Self {
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
}

impl Type {
    pub const fn is_int_family(&self) -> bool {
        matches!(self, Self::GenericInteger | Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }

    pub fn tuple(tuple_elements: Vec<Type>) -> Self {
        Self::Tuple(DisplayTupleType(tuple_elements))
    }
    
    pub fn is_assignable(&self, from: &Self) -> AssignableQueryAnswer {
        if self.is_int_family() && from == &Self::GenericInteger {
            AssignableQueryAnswer::PossibleIfCoerceSourceImplicitly
        } else if self == from {
            AssignableQueryAnswer::Yes
        } else {
            AssignableQueryAnswer::No
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
    }
}

impl TypedExpression {
    pub fn actual_type(&self) -> Type {
        match self {
            TypedExpression::IntLiteral(i) => i.actual_type(),
            TypedExpression::BooleanLiteral(_) => Type::Boolean,
            TypedExpression::StringLiteral(_) => Type::String,
            TypedExpression::UnitLiteral => Type::Unit,
            TypedExpression::Variable { tp, .. } => tp.clone(),
            TypedExpression::BinaryOperator { return_type, .. } => return_type.clone(),
            TypedExpression::If { return_type, .. } => return_type.clone(),
            TypedExpression::Block { return_type, .. } => return_type.clone(),
            TypedExpression::Tuple { expressions } => Type::tuple(expressions.iter().map(|x| x.actual_type()).collect())
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
    pub const fn actual_type(&self) -> Type {
        match self {
            TypedIntLiteral::Generic(_) => Type::GenericInteger,
            TypedIntLiteral::Bit64(_) => Type::Int64,
            TypedIntLiteral::Bit32(_) => Type::Int32,
            TypedIntLiteral::Bit16(_) => Type::Int16,
            TypedIntLiteral::Bit8(_) => Type::Int8,
        }
    }
}
