use std::collections::VecDeque;
use derivative::Derivative;
use origlang_ast::after_parse::Expression;
use origlang_ast::{Identifier, RootAst, Statement};
use crate::{CanBeEvaluated, Runtime};

#[derive(Derivative)]
#[derivative(Debug)]
pub enum Verbatim {
    Output(Expression),
    UpdateVariable {
        ident: Identifier,
        #[derivative(Debug="ignore")]
        value: Expression,
    },
    PushScope,
    PopScope,
}

impl Verbatim {
    pub(super) fn create<T: IntoVerbatimSequencedIR>(from: T) -> Vec<Self> {
        from.into_ir()
    }

    pub fn invoke(&self, runtime: &Runtime) {
        match self {
            Self::Output(e) => {
                runtime.o.as_ref().borrow_mut().output(e.evaluate(runtime).expect("runtime exception"));
            }
            Self::UpdateVariable { ident, value } => {
                runtime.upsert_member_to_current_scope(
                    ident.clone(),
                    value.evaluate(runtime).expect("runtime exception")
                );
            }
            Self::PushScope => {
                runtime.push_scope();
            }
            Self::PopScope => {
                runtime.pop_scope();
            }
        }
    }
}

pub(super) trait IntoVerbatimSequencedIR {
    fn into_ir(self) -> Vec<Verbatim>;
}

impl IntoVerbatimSequencedIR for Statement {
    fn into_ir(self) -> Vec<Verbatim> {
        let statement = self;

        match statement {
            Self::Print { expression } => {
                vec![
                    Verbatim::Output(expression)
                ]
            }
            Self::VariableDeclaration { identifier, expression } => {
                vec![
                    Verbatim::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::VariableAssignment { identifier, expression } => {
                vec![
                    Verbatim::UpdateVariable {
                        ident: identifier,
                        value: expression,
                    }
                ]
            }
            Self::Block { inner_statements } => {
                let mut vec = inner_statements.into_iter()
                    .flat_map(|x| x.into_ir())
                    .collect::<VecDeque<Verbatim>>();
                vec.push_front(Verbatim::PushScope);
                vec.push_back(Verbatim::PopScope);
                vec.into()
            }
            Self::Comment { .. } => vec![],
        }
    }
}

impl IntoVerbatimSequencedIR for RootAst {
    fn into_ir(self) -> Vec<Verbatim> {
        let value = self;

        pub fn what_will_happen(ast: RootAst) -> Vec<Verbatim> {
            ast.statement.into_iter()
                .flat_map(what_will_happen1)
                .collect()
        }

        fn what_will_happen1(statement: Statement) -> Vec<Verbatim> {
            match statement {
                Statement::Print { expression } => {
                    vec![
                        Verbatim::Output(expression)
                    ]
                }
                Statement::VariableDeclaration { identifier, expression } => {
                    vec![
                        Verbatim::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                Statement::VariableAssignment { identifier, expression } => {
                    vec![
                        Verbatim::UpdateVariable {
                            ident: identifier,
                            value: expression,
                        }
                    ]
                }
                Statement::Block { inner_statements } => {
                    let mut vec = inner_statements.into_iter()
                        .flat_map(what_will_happen1)
                        .collect::<VecDeque<_>>();
                    vec.push_front(Verbatim::PushScope);
                    vec.push_back(Verbatim::PopScope);
                    vec.into()
                }
                Statement::Comment { .. } => vec![],
            }
        }

        what_will_happen(value)
    }
}

impl<T: IntoVerbatimSequencedIR> IntoVerbatimSequencedIR for Vec<T> {
    fn into_ir(self) -> Vec<Verbatim> {
        self.into_iter().flat_map(|x| x.into_ir()).collect()
    }
}
