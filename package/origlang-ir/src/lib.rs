#![deny(clippy::all, clippy::panicking_unwrap, clippy::panic)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::collections::VecDeque;
use origlang_ast::Identifier;
use origlang_typesystem_model::{TypedExpression, TypedRootAst, TypedStatement};

#[derive(Debug)]
pub enum IR1 {
    Output(TypedExpression),
    UpdateVariable {
        ident: Identifier,
        value: TypedExpression,
    },
    PushScope,
    PopScope,
}

impl IR1 {
    pub fn create<T: IntoVerbatimSequencedIR>(from: T) -> Vec<Self> {
        from.into_ir()
    }
}

pub trait IntoVerbatimSequencedIR {
    fn into_ir(self) -> Vec<IR1>;
}

impl IntoVerbatimSequencedIR for TypedStatement {
    fn into_ir(self) -> Vec<IR1> {
        let statement = self;

        match statement {
            Self::Print { expression } => {
                vec![
                    IR1::Output(expression)
                ]
            }
            Self::VariableDeclaration { identifier, expression } => {
                vec![
                    IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::VariableAssignment { identifier, expression } => {
                vec![
                    IR1::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::Block { inner_statements } => {
                let mut vec = inner_statements.into_iter()
                    .flat_map(|x| x.into_ir())
                    .collect::<VecDeque<IR1>>();
                vec.push_front(IR1::PushScope);
                vec.push_back(IR1::PopScope);
                vec.into()
            }
        }
    }
}

impl IntoVerbatimSequencedIR for TypedRootAst {
    fn into_ir(self) -> Vec<IR1> {
        let value = self;

        pub fn what_will_happen(ast: TypedRootAst) -> Vec<IR1> {
            ast.statements.into_iter()
                .flat_map(what_will_happen1)
                .collect()
        }

        fn what_will_happen1(statement: TypedStatement) -> Vec<IR1> {
            match statement {
                TypedStatement::Print { expression } => {
                    vec![
                        IR1::Output(expression)
                    ]
                }
                TypedStatement::VariableDeclaration { identifier, expression } => {
                    vec![
                        IR1::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                TypedStatement::VariableAssignment { identifier, expression } => {
                    vec![
                        IR1::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                TypedStatement::Block { inner_statements } => {
                    let mut vec = inner_statements.into_iter()
                        .flat_map(what_will_happen1)
                        .collect::<VecDeque<_>>();
                    vec.push_front(IR1::PushScope);
                    vec.push_back(IR1::PopScope);
                    vec.into()
                }
            }
        }

        what_will_happen(value)
    }
}

impl<T: IntoVerbatimSequencedIR> IntoVerbatimSequencedIR for Vec<T> {
    fn into_ir(self) -> Vec<IR1> {
        self.into_iter().flat_map(|x| x.into_ir()).collect()
    }
}
