#![deny(clippy::all, clippy::panicking_unwrap, clippy::panic)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::collections::VecDeque;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ast::Identifier;
use origlang_typesystem_model::{Type, TypedExpression, TypedIntLiteral, TypedRootAst, TypedStatement};

/// The initial form of IR. This IR defines [`Self::Exit`] operation that aborts current execution.
/// However, IR commands after it is cut-down when lowering to [`IR1`].
#[derive(Eq, PartialEq, Debug)]
pub enum IR0 {
    Normal(IR1),
    Exit,
}

impl IR0 {
    pub fn create<T: IntoVerbatimSequencedIR>(from: T) -> Vec<Self> {
        from.into_ir()
    }
}

pub trait IntoVerbatimSequencedIR {
    fn into_ir(self) -> Vec<IR0>;
}

impl IntoVerbatimSequencedIR for TypedStatement {
    fn into_ir(self) -> Vec<IR0> {
        let statement = self;

        match statement {
            TypedStatement::Print { expression } => {
                vec![
                    IR0::Normal(IR1::Output(expression))
                ]
            }
            TypedStatement::VariableDeclaration { identifier, expression } => {
                vec![
                    IR0::Normal(IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    })
                ]
            }
            TypedStatement::VariableAssignment { identifier, expression } => {
                vec![
                    IR0::Normal(IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    })
                ]
            }
            TypedStatement::Block { inner_statements } => {
                let mut vec = inner_statements.into_iter()
                    .flat_map(<TypedStatement as IntoVerbatimSequencedIR>::into_ir)
                    .collect::<VecDeque<_>>();
                vec.push_front(IR0::Normal(IR1::PushScope));
                vec.push_back(IR0::Normal(IR1::PopScope));
                vec.into()
            }
            TypedStatement::Exit => vec![IR0::Exit],
            TypedStatement::EvalAndForget { expression } => {
                vec![IR0::Normal(IR1::EvalAndForget { expression })]
            },
            TypedStatement::Empty => vec![]
        }
    }
}

impl IntoVerbatimSequencedIR for TypedRootAst {
    fn into_ir(self) -> Vec<IR0> {
        self.statements.into_iter()
            .flat_map(<TypedStatement as IntoVerbatimSequencedIR>::into_ir)
            .collect()
    }
}

impl<T: IntoVerbatimSequencedIR> IntoVerbatimSequencedIR for Vec<T> {
    fn into_ir(self) -> Vec<IR0> {
        self.into_iter().flat_map(|x| x.into_ir()).collect()
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
        expression: TypedExpression
    }
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
        expression: CompiledTypedExpression
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum CompiledTypedExpression {
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
        inner: Vec<IR2>,
        final_expression: Box<Self>,
        return_type: Type,
    },
    Tuple {
        expressions: Vec<Self>
    },
    ExtractTuple { expr: Box<CompiledTypedExpression>, index: usize },
}
