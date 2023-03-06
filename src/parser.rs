use std::num::ParseIntError;
use crate::ast::{RootAst, SourcePos, Statement, WithPosition};
use crate::lexer::{DisplayToken, Lexer, LexerError, Token};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use std::string::ToString;
use derive_more::Display;
use log::debug;
use thiserror::{Error as ThisError};
use crate::parser::ParserError::EndOfFileError;
use crate::parser::TokenKind::IntLiteral;

// TODO: これはWithPosition<ParserError>にできるかもしれないが一旦保留
#[derive(ThisError, Debug, Display, Eq, PartialEq)]
#[display(fmt = "{kind} ({position})")]
pub struct SimpleErrorWithPos {
    pub kind: ParserError,
    pub position: SourcePos,
}

impl SimpleErrorWithPos {}

#[derive(ThisError, Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum ParserError {
    #[error("lexer error: {_0}")]
    LexerError(#[from] LexerError),
    #[error("unconsumed token found: {token:?}")]
    UnconsumedToken {
        token: Token
    },
    #[error("statement must be terminated by a newline")]
    StatementTerminationError,
    #[error("EOF Error")]
    EndOfFileError,
    #[error("Expected {pat}, but got {unmatch:?}")]
    UnexpectedToken {
        pat: TokenKind,
        unmatch: Token,
    },
    #[error("Incomplete program snippet. Check hint for fix candidates. hint:{hint:?} state:{intermediate_state:?}")]
    PartiallyParsed {
        hint: Vec<PartiallyParseFixCandidate>,
        intermediate_state: Vec<IntermediateStateCandidate>,
    },
    #[error("input sequence cannot be parsed as a int literal: {error}")]
    UnParsableIntLiteral {
        error: ParseIntError
    },
    #[error("int literal type of {tp} must be in range ({min}..={max}), but its value is {value}")]
    OverflowedLiteral {
        tp: Box<str>,
        min: i64,
        max: i64,
        value: i64,
    },
    #[error("if expression requires `else` clause")]
    IfExpressionWithoutElseClause,
    #[error("if expression requires `then` clause and `else` clause")]
    IfExpressionWithoutThenClauseAndElseClause,
    #[error("tuple literal requires 2 or more elements, but got {_0}")]
    InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount),
}

#[repr(u8)]
#[derive(Eq, PartialEq, Copy, Clone, Debug, Display)]
pub enum UnexpectedTupleLiteralElementCount {
    #[display(fmt = "no elements")]
    Zero = 0,
    #[display(fmt = "only one element")]
    One = 1,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum IntermediateStateCandidate {
    Expression(Expression),
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum PartiallyParseFixCandidate {
    #[display(fmt = "No fixes available")]
    None,
    #[display(fmt = "Insert before")]
    InsertBefore {
        tokens: Vec<Token>,
    },
    #[display(fmt = "Insert after")]
    InsertAfter {
        tokens: Vec<Token>,
    },
}

#[derive(Display, Debug, Eq, PartialEq, Clone)]
pub enum TokenKind {
    #[display(fmt = "int literal, boolean literal, string literal, or identifier")]
    First,
    #[display(fmt = "`/` or `*`")]
    MultiplicativeOps,
    #[display(fmt = "`+` or `-`")]
    AdditiveOps,
    #[display(fmt = "`<=`, `>=`, `<`, `>` or `<=>`")]
    ComparisonOps,
    #[display(fmt = "`==` or `!=`")]
    EqualityOps,
    #[display(fmt = "int literal")]
    IntLiteral,
    #[display(fmt = "identifier")]
    Identifier,
    #[display(fmt = "keyword:`print`, keyword:`var`, or identifier")]
    Statement,
    #[display(fmt = "{_0}")]
    Only(DisplayToken)
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn create(source: &str) -> Self {
        Self {
            lexer: Lexer::create(source)
        }
    }

    /// プログラムが文の列とみなしてパースを試みる。
    /// 事前条件: プログラム全体が任意個の文として分解できる
    pub fn parse(&self) -> Result<RootAst, SimpleErrorWithPos> {
        let mut statements = vec![];
        while self.lexer.peek().data != Token::EndOfFile {
            let res = self.parse_statement()?;
            statements.push(res);
        }

        {
            let t = self.lexer.next();
            match t.data {
                Token::EndOfFile | Token::NewLine => Ok(RootAst {
                    statement: statements,
                }),
                other => Err(SimpleErrorWithPos {
                    position: t.position,
                    kind: ParserError::UnconsumedToken { token: other }
                })
            }
        }
    }

    fn parse_statement(&self) -> Result<Statement, SimpleErrorWithPos> {
        let head1 = self.lexer.peek();
        let head = head1.data;
        let pos = head1.position;
        // dbg!(&head);
        let s = match head {
            Token::Identifier { .. } => {
                self.parse_variable_assignment()?
            }
            Token::VarKeyword => {
                self.parse_variable_declaration()?
            }
            Token::KeywordPrint => {
                // assuming expression
                self.lexer.next();
                let expr = self.parse_lowest_precedence_expression()?;

                Statement::Print {
                    expression: expr
                }
            }
            Token::KeywordBlock => {
                self.parse_block_scope()
            }
            x => {
                return Err(SimpleErrorWithPos {
                    kind: ParserError::UnexpectedToken {
                        pat: TokenKind::Statement,
                        unmatch: x,
                    },
                    position: pos,
                })
            }
        };

        // 文は絶対に改行で終わる必要がある
        let next = self.lexer.next();
        if next.data != Token::NewLine {
            return Err(SimpleErrorWithPos {
                position: next.position,
                kind: ParserError::PartiallyParsed {
                    hint: vec![
                        PartiallyParseFixCandidate::InsertAfter {
                            tokens: vec![ Token::NewLine ]
                        }
                    ],
                    intermediate_state: vec![],
                },
            })
        }

        Ok(s)
    }

    /// 現在のトークン位置から基本式をパースしようと試みる。
    /// 事前条件: 現在のトークン位置が基本式として有効である必要がある
    /// 違反した場合はErr。
    fn parse_first(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:first");
        let token = self.lexer.peek();
        match token.data {
            Token::Identifier { inner } => {
                // consume
                self.lexer.next();
                Ok(Expression::Variable {
                    ident: inner
                })
            }
            Token::Digits { .. } => {
                self.parse_int_literal().map(|(parsed, suffix)| {
                    Expression::IntLiteral { value: parsed, suffix }
                })
            }
            Token::SymLeftPar => {
                assert_eq!(self.lexer.next().data, Token::SymLeftPar);
                // FIXME: (1 == 2)を受け付けない
                if self.lexer.peek().data == Token::SymRightPar {
                    self.lexer.next();
                    Ok(Expression::UnitLiteral)
                } else if let Ok(expr_tuple) = self.parse_tuple_expression() {
                    Ok(expr_tuple)
                } else {
                    let inner_expression = self.parse_lowest_precedence_expression()?;
                    assert_eq!(self.lexer.next().data, Token::SymRightPar);
                    Ok(inner_expression)
                }
            }
            Token::KeywordTrue => {
                self.lexer.next();
                Ok(Expression::BooleanLiteral(true))
            }
            Token::KeywordFalse => {
                self.lexer.next();
                Ok(Expression::BooleanLiteral(false))
            }
            Token::EndOfFile => {
                Err(SimpleErrorWithPos {
                    kind: EndOfFileError,
                    position: token.position,
                })
            }
            Token::StringLiteral(s) => {
                self.lexer.next();
                Ok(Expression::StringLiteral(s))
            }
            e => Err(SimpleErrorWithPos {
                kind: ParserError::UnexpectedToken {
                    pat: TokenKind::First,
                    unmatch: e,
                },
                position: token.position,
            })
        }
    }


    fn parse_tuple_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        self.lexer.parse_fallible(|| {
            debug!("expr.tuple");

            let mut buf = vec![];
            while let Ok(e) = self.parse_lowest_precedence_expression() {
                buf.push(e);
                let peek = self.lexer.peek();
                if peek.data == Token::SymRightPar {
                    self.lexer.next();
                    break
                } else if peek.data != Token::SymComma {
                    return Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::Only(Token::SymComma.display()),
                            unmatch: peek.data,
                        },
                        position: peek.position,
                    })
                }

                self.lexer.next();
            }

            let bl = buf.len();

            if bl == 0 {
                // disallow ()
                return Err(SimpleErrorWithPos {
                    kind: ParserError::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::Zero),
                    position: self.lexer.peek().position,
                })
            } else if bl == 1 {
                // disallow (expr)
                return Err(SimpleErrorWithPos {
                    kind: ParserError::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::One),
                    position: self.lexer.peek().position,
                })
            }

            Ok(Expression::Tuple {
                expressions: buf
            })
        })
    }
    /// 現在のトークン位置から乗除算をパースする。
    fn parse_multiplicative(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:mul");
        let first_term = self.parse_first()?;
        let next_token = self.lexer.peek();
        let asterisk_or_slash = |token: &Token| {
            token == &Token::SymAsterisk || token == &Token::SymSlash
        };

        if asterisk_or_slash(&next_token.data) {
            // SymAsterisk | SymSlash
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_first()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::SymAsterisk => Ok(BinaryOperatorKind::Multiply),
                    Token::SymSlash => Ok(BinaryOperatorKind::Divide),
                    e => Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::MultiplicativeOps,
                            unmatch: e.clone(),
                        },
                        position: token.position
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while asterisk_or_slash(&operator_token.data) {
                // SymAsterisk | SymSlash
                self.lexer.next();
                let new_rhs = self.parse_first()?;
                // 左結合になるように詰め替える
                // これは特に除算のときに欠かせない処理である
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary
            Ok(first_term)
        }
    }

    /// 現在のトークン位置から加減算をパースしようと試みる。
    /// 事前条件: 現在の位置が加減算として有効である必要がある
    /// 違反した場合はErr
    fn parse_additive(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:add");
        let first_term = self.parse_multiplicative()?;
        let next_token = self.lexer.peek();
        let plus_or_minus = |token: &Token| {
            token == &Token::SymPlus || token == &Token::SymMinus
        };

        if plus_or_minus(&next_token.data) {
            // SymPlus | SymMinus
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_multiplicative()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::SymPlus => Ok(BinaryOperatorKind::Plus),
                    Token::SymMinus => Ok(BinaryOperatorKind::Minus),
                    e => Err(SimpleErrorWithPos {
                        position: token.position,
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::AdditiveOps,
                            unmatch: e.clone(),
                        }
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while plus_or_minus(&operator_token.data) {
                // SymPlus | SymMinus
                self.lexer.next();
                let new_rhs = self.parse_multiplicative()?;
                // 左結合になるように詰め替える
                // これは特に減算のときに欠かせない処理である
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary or multiplicative
            Ok(first_term)
        }
    }

    /// 現在の位置から比較演算式をパースしようと試みる
    fn parse_relation_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:rel");
        let first_term = self.parse_additive()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessEq | Token::PartMoreEq | Token::SymLess | Token::SymMore | Token::PartLessEqMore)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_additive()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::PartLessEq => Ok(BinaryOperatorKind::LessEqual),
                    Token::PartMoreEq => Ok(BinaryOperatorKind::MoreEqual),
                    Token::SymLess => Ok(BinaryOperatorKind::Less),
                    Token::SymMore => Ok(BinaryOperatorKind::More),
                    Token::PartLessEqMore => Ok(BinaryOperatorKind::ThreeWay),
                    e => Err(SimpleErrorWithPos {
                        position: token.position,
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::ComparisonOps,
                            unmatch: e.clone(),
                        },
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_additive()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在の位置から等価性検査式をパースしようと試みる
    fn parse_equality_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:eq");
        let first_term = self.parse_relation_expression()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartEqEq | Token::PartBangEq)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_relation_expression()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::PartEqEq => Ok(BinaryOperatorKind::Equal),
                    Token::PartBangEq => Ok(BinaryOperatorKind::NotEqual),
                    e => Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::EqualityOps,
                            unmatch: e.clone(),
                        },
                        position: token.position,
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在のトークンを消費して整数リテラルの生成を試みる。
    /// 事前条件: 現在のトークンが整数として有効である必要がある
    /// 違反した場合はErrを返す。
    fn parse_int_literal(&self) -> Result<(i64, Option<Box<str>>), SimpleErrorWithPos> {
        debug!("expr:lit:int");
        let n = self.lexer.next();
        if let Token::Digits { sequence, suffix } = n.data {
            let x = sequence.as_str().parse::<i64>();
            if let Err(e) = x {
                return Err(SimpleErrorWithPos {
                    kind: ParserError::UnParsableIntLiteral {
                        error: e
                    },
                    position: n.position,
                })
            }

            let x = x.unwrap();
            if let Some(y) = suffix.as_ref() {
                macro_rules! lit_value {
                        ($t:ty, $lang_type:literal, $v:expr) => {{
                            if $v < i64::from(<$t>::MIN) || i64::from(<$t>::MAX) < $v {
                                return Err(SimpleErrorWithPos {
                                    kind: ParserError::OverflowedLiteral {
                                        tp: $lang_type.to_string().into_boxed_str(),
                                        min: (<$t>::MIN) as i64,
                                        max: (<$t>::MAX) as i64,
                                        value: $v
                                    },
                                    position: n.position
                                })
                            }
                        }};
                    }
                match y.as_ref() {
                    "i8"  => lit_value!(i8, "i8", x),
                    "i16" => lit_value!(i16, "i16", x),
                    "i32" => lit_value!(i32, "i32", x),
                    "i64" => {}
                    _ => unreachable!()
                }
            }
            Ok((x, suffix))
        } else {
            Err(SimpleErrorWithPos {
                kind: ParserError::UnexpectedToken {
                    pat: IntLiteral,
                    unmatch: n.data
                },
                position: n.position,
            })
        }
    }

    /// 現在の`Lexer`に積まれている`Token`と期待される`Token`を比較し、違っていた場合はpanicする。
    /// この関数は`Lexer`の`Token`を一つ消費するという副作用がある。
    fn assert_token_eq_with_consumed(&self, rhs: &Token) {
        let token = self.lexer.next().data;
        assert_eq!(&token, rhs, "expected: {rhs:?}, got: {token:?}");
    }

    fn parse_variable_declaration(&self) -> Result<Statement, SimpleErrorWithPos> {
        debug!("decl:var");
        self.assert_token_eq_with_consumed(&Token::VarKeyword);
        let ident_token = self.lexer.next();
        let Token::Identifier { inner: name } = ident_token.data else {
            return Err(SimpleErrorWithPos {
                position: ident_token.position,
                kind: ParserError::UnexpectedToken {
                    pat: TokenKind::Identifier,
                    unmatch: ident_token.data,
                }
            })
        };
        self.assert_token_eq_with_consumed(&Token::SymEq);
        let expression = self.parse_lowest_precedence_expression()?;
        Ok(Statement::VariableDeclaration {
            identifier: name,
            expression
        })
    }

    fn parse_variable_assignment(&self) -> Result<Statement, SimpleErrorWithPos> {
        debug!("assign:var");
        let ident_token = self.lexer.next();
        let Token::Identifier { inner: name } = ident_token.data else {
            return Err(SimpleErrorWithPos {
                position: ident_token.position,
                kind: ParserError::UnexpectedToken {
                    pat: TokenKind::Identifier,
                    unmatch: ident_token.data,
                }
            })
        };
        self.assert_token_eq_with_consumed(&Token::SymEq);
        let expression = self.parse_lowest_precedence_expression()?;
        Ok(Statement::VariableAssignment {
            identifier: name,
            expression
        })
    }

    fn parse_lowest_precedence_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:lowest");
        self.parse_block_expression()
    }

    fn parse_if_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:if");
        if self.lexer.peek().data == Token::KeywordIf {
            self.lexer.next();
            let condition = self.parse_lowest_precedence_expression()?;
            let WithPosition { position, data } = self.lexer.peek();
            if data == Token::KeywordThen {
                self.lexer.next();
                let then_clause_value = self.parse_lowest_precedence_expression()?;
                let WithPosition { position, data } = self.lexer.peek();
                if data == Token::KeywordElse {
                    self.lexer.next();
                    let else_clause_value = self.parse_lowest_precedence_expression()?;
                    Ok(Expression::If {
                        condition: Box::new(condition),
                        then_clause_value: Box::new(then_clause_value),
                        else_clause_value: Box::new(else_clause_value),
                    })
                } else {
                    Err(SimpleErrorWithPos {
                        kind: ParserError::IfExpressionWithoutElseClause,
                        position,
                    })
                }
            } else {
                Err(SimpleErrorWithPos {
                    kind: ParserError::IfExpressionWithoutThenClauseAndElseClause,
                    position,
                })
            }
        } else {
            self.parse_equality_expression()
        }
    }

    fn parse_block_scope(&self) -> Statement {
        debug!("parser:block:scope");
        self.assert_token_eq_with_consumed(&Token::KeywordBlock);
        if self.lexer.peek().data == Token::NewLine {
            self.lexer.next();
        }

        let mut statements = vec![];
        while let Ok(v) = self.parse_statement() {
            statements.push(v);
        }
        self.assert_token_eq_with_consumed(&Token::KeywordEnd);
        Statement::Block {
            inner_statements: (statements),
        }
    }

    fn parse_block_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("parser:block:expr");
        if self.lexer.peek().data == Token::KeywordBlock {
            self.lexer.next();
            self.assert_token_eq_with_consumed(&Token::NewLine);
            let mut statements = vec![];
            while let Ok(v) = self.parse_statement() {
                statements.push(v);
            }
            let final_expression = Box::new(self.parse_lowest_precedence_expression()?);
            if self.lexer.peek().data == Token::NewLine {
                self.lexer.next();
            }
            self.assert_token_eq_with_consumed(&Token::KeywordEnd);
            Ok(Expression::Block {
                intermediate_statements: statements,
                final_expression
            })
        } else {
            self.parse_if_expression()
        }
    }
}
