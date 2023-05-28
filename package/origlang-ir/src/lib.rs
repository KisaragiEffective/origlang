#![deny(clippy::all, clippy::panicking_unwrap, clippy::panic)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::collections::VecDeque;
use origlang_ast::Identifier;
use origlang_typesystem_model::{TypedExpression, TypedRootAst, TypedStatement};

#[derive(Debug)]
pub enum IR1<'expr> {
    Output(&'expr TypedExpression<'expr>),
    UpdateVariable {
        ident: &'expr Identifier,
        value: &'expr TypedExpression<'expr>,
    },
    PushScope,
    PopScope,
}

impl<'expr> IR1<'expr> {
    pub fn create<T: IntoVerbatimSequencedIR<'expr>>(from: T) -> Vec<Self> {
        from.into_ir()
    }
}

pub trait IntoVerbatimSequencedIR<'i> {
    fn into_ir(&self) -> &[IR1<'i>];
}

impl<'ir1> IntoVerbatimSequencedIR<'ir1> for TypedStatement<'ir1> {
    fn into_ir(&self) -> &[IR1<'ir1>] {
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
                        ident: &identifier,
                        value: &expression,
                    }
                ]
            }
            Self::VariableAssignment { identifier, expression } => {
                vec![
                    IR1::UpdateVariable {
                        ident: &identifier,
                        value: &expression,
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

impl<'ir1> IntoVerbatimSequencedIR<'ir1> for TypedRootAst<'ir1> {
    fn into_ir(&self) -> &[IR1<'ir1>] {
        pub fn what_will_happen(ast: TypedRootAst) -> Vec<IR1> {
            ast.statements
                .into_iter()
                .flat_map(what_will_happen1)
                .collect()
        }

        fn what_will_happen1<'expr>(statement: &'expr TypedStatement<'expr>) -> Vec<IR1<'expr>> {
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

        what_will_happen(self)
    }
}

impl<'ir1, T: IntoVerbatimSequencedIR<'ir1>> IntoVerbatimSequencedIR<'ir1> for Vec<T> {
    fn into_ir(&self) -> &[IR1<'ir1>] {
        self.into_iter().flat_map(|x| x.into_ir()).collect()
    }
}
