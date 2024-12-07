use derive_more::Display;
use origlang_ast::after_parse::Expression;
use origlang_lexer::token::Token;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntermediateStateCandidate {
    Expression(Expression),
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum PartiallyParseFixCandidate {
    #[display(fmt = "No fixes available")]
    None,
    #[display(fmt = "Insert before")]
    InsertBefore { tokens: Box<[Token]> },
    #[display(fmt = "Insert after")]
    InsertAfter { tokens: Box<[Token]> },
}
