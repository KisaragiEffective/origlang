use derive_more::Display;
use origlang_ast::after_parse::Expression;
use origlang_lexer::token::Token;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntermediateStateCandidate {
    Expression(Expression),
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum PartiallyParseFixCandidate {
    #[display("No fixes available")]
    None,
    #[display("Insert before")]
    InsertBefore { tokens: Box<[Token]> },
    #[display("Insert after")]
    InsertAfter { tokens: Box<[Token]> },
}
