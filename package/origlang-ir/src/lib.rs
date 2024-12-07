#![deny(clippy::all, clippy::panicking_unwrap, clippy::panic)]
#![warn(clippy::pedantic, clippy::nursery)]

use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ast::Identifier;
use origlang_typesystem_model::{Type, TypedExpression, TypedFloatLiteral, TypedIntLiteral, TypedRootAst, TypedStatement};
use std::collections::VecDeque;

pub trait IntoVerbatimSequencedIR {
    fn into_ir(self) -> Vec<IR1>;
}

impl IntoVerbatimSequencedIR for TypedStatement {
    fn into_ir(self) -> Vec<IR1> {
        let statement = self;

        match statement {
            Self::Print { expression } => {
                vec![(IR1::Output(expression))]
            }
            Self::VariableDeclaration {
                identifier,
                expression,
            }
            | Self::VariableAssignment {
                identifier,
                expression,
            } => {
                vec![
                    (IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }),
                ]
            }
            Self::Block { inner_statements } => {
                let mut vec = inner_statements
                    .into_iter()
                    .flat_map(<Self as IntoVerbatimSequencedIR>::into_ir)
                    .collect::<VecDeque<_>>();
                vec.push_front(IR1::PushScope);
                vec.push_back(IR1::PopScope);
                vec.into()
            }
            Self::Exit => vec![(IR1::Exit)],
            Self::EvalAndForget { expression } => {
                vec![(IR1::EvalAndForget { expression })]
            }
        }
    }
}

impl IntoVerbatimSequencedIR for TypedRootAst {
    fn into_ir(self) -> Vec<IR1> {
        self.statements
            .into_iter()
            .flat_map(<TypedStatement as IntoVerbatimSequencedIR>::into_ir)
            .collect()
    }
}

impl<T: IntoVerbatimSequencedIR> IntoVerbatimSequencedIR for Vec<T> {
    fn into_ir(self) -> Vec<IR1> {
        self.into_iter()
            .flat_map(IntoVerbatimSequencedIR::into_ir)
            .collect()
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum IR1 {
    Output(TypedExpression),
    UpdateVariable {
        ident: Identifier,
        value: TypedExpression,
    },
    PushScope,
    PopScope,
    Exit,
    EvalAndForget {
        expression: TypedExpression,
    },
}

/// Same as [`IR1`], except that statements in blocks are lowered to this type.
#[derive(Eq, PartialEq, Debug, Clone)]
pub enum IR2 {
    Output(CompiledTypedExpression),
    UpdateVariable {
        ident: Identifier,
        value: CompiledTypedExpression,
    },
    PushScope,
    PopScope,
    Exit,
    EvalAndForget {
        expression: CompiledTypedExpression,
    },
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum CompiledTypedExpression {
    IntLiteral(TypedIntLiteral),
    BooleanLiteral(bool),
    FloatLiteral(TypedFloatLiteral),
    StringLiteral(Box<str>),
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
        return_type: Type,
    },
    Block {
        inner: Vec<IR2>,
        final_expression: Box<Self>,
        return_type: Type,
    },
    Tuple {
        expressions: Vec<Self>,
    },
    ExtractTuple {
        expr: Box<CompiledTypedExpression>,
        index: usize,
    },
}
