use std::num::ParseIntError;
use origlang_ast::{AtomicPattern, RootAst, Statement, TypeSignature};
use origlang_source_span::{SourcePosition as SourcePos, Pointed as WithPosition};
use crate::lexer::Lexer;
use crate::lexer::error::LexerError;
use crate::lexer::token::Token;
use crate::lexer::token::internal::DisplayToken;

use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use std::string::ToString;
use derive_more::Display;
use log::{debug, warn};
use num_traits::Bounded;
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
    #[error("`_` cannot used as right hand side expression")]
    UnderscoreCanNotBeRightHandExpression,
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
    #[display(fmt = "`<<` or `>>`")]
    ShiftOps,
    #[display(fmt = "int literal")]
    IntLiteral,
    #[display(fmt = "identifier")]
    Identifier,
    #[display(fmt = "keyword:`print`, keyword:`var`, or identifier")]
    Statement,
    #[display(fmt = "{_0}")]
    Only(DisplayToken),
    #[display(fmt = "`(` or identifier")]
    StartOfTypeSignature,
}

impl TokenKind {
    fn only(token: Token) -> Self {
        Self::Only(token.display())
    }
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
}

impl<'src> Parser<'src> {
    #[must_use = "Parser do nothing unless calling parsing function"]
    pub fn create(source: &'src str) -> Self {
        Self {
            lexer: Lexer::create(source)
        }
    }
}

impl Parser<'_> {
    /// プログラムが文の列とみなしてパースを試みる。
    /// 事前条件: プログラム全体が任意個の文として分解できる
    /// # Errors
    /// プログラムのパースに失敗したときErr。
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
                self.parse_block_scope()?
            }
            Token::Comment { content } => {
                self.lexer.next();
                Statement::Comment { content }
            }
            Token::KeywordExit => {
                self.lexer.next();
                Statement::Exit
            }
            Token::KeywordType => {
                self.lexer.next();
                let aliased = self.lexer.next();

                let Token::Identifier { inner: aliased } = aliased.data else {
                    return Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::Identifier,
                            unmatch: aliased.data,
                        },
                        position: aliased.position,
                    })

                };

                self.read_and_consume_or_report_unexpected_token(Token::SymEq)?;
                let Ok(replace_with) = self.lexer.parse_fallible(|| self.parse_type()) else {
                    return Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::StartOfTypeSignature,
                            unmatch: self.lexer.peek().data
                        },
                        position: self.lexer.peek().position
                    })
                };

                Statement::TypeAliasDeclaration {
                    new_name: aliased,
                    replace_with,
                }
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

        // 文は絶対に改行かEOFで終わる必要がある
        let next = self.lexer.next();
        if next.data != Token::NewLine && next.data != Token::EndOfFile {
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
            Token::SymUnderscore => {
                self.lexer.next();
                Err(SimpleErrorWithPos {
                    kind: ParserError::UnderscoreCanNotBeRightHandExpression,
                    position: token.position,
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
                    self.read_and_consume_or_report_unexpected_token(Token::SymRightPar)?;
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
            let get_operator_from_token = |token: WithPosition<Token>| {
                match token.data {
                    Token::SymPlus => Ok(BinaryOperatorKind::Plus),
                    Token::SymMinus => Ok(BinaryOperatorKind::Minus),
                    e => Err(SimpleErrorWithPos {
                        position: token.position,
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::AdditiveOps,
                            unmatch: e,
                        }
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while plus_or_minus(&operator_token.data) {
                // SymPlus | SymMinus
                self.lexer.next();
                let new_rhs = self.parse_multiplicative()?;
                // 左結合になるように詰め替える
                // これは特に減算のときに欠かせない処理である
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary or multiplicative
            Ok(first_term)
        }
    }

    fn parse_shift_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:shift");
        let first_term = self.parse_additive()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessLess | Token::PartMoreMore)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_relation_expression()?;
            let get_operator_from_token = |token: WithPosition<Token>| {
                match token.data {
                    Token::PartLessLess => Ok(BinaryOperatorKind::ShiftLeft),
                    Token::PartMoreMore => Ok(BinaryOperatorKind::ShiftRight),
                    e => Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::ShiftOps,
                            unmatch: e,
                        },
                        position: token.position,
                    }),
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek();
            }

            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在の位置から比較演算式をパースしようと試みる
    fn parse_relation_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("expr:rel");
        let first_term = self.parse_shift_expression()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessEq | Token::PartMoreEq | Token::SymLess | Token::SymMore | Token::PartLessEqMore)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_shift_expression()?;
            let get_operator_from_token = |token: WithPosition<Token>| {
                match token.data {
                    Token::PartLessEq => Ok(BinaryOperatorKind::LessEqual),
                    Token::PartMoreEq => Ok(BinaryOperatorKind::MoreEqual),
                    Token::SymLess => Ok(BinaryOperatorKind::Less),
                    Token::SymMore => Ok(BinaryOperatorKind::More),
                    Token::PartLessEqMore => Ok(BinaryOperatorKind::ThreeWay),
                    e => Err(SimpleErrorWithPos {
                        position: token.position,
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::ComparisonOps,
                            unmatch: e,
                        },
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_additive()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
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
            let get_operator_from_token = |token: WithPosition<Token>| {
                match token.data {
                    Token::PartEqEq => Ok(BinaryOperatorKind::Equal),
                    Token::PartBangEq => Ok(BinaryOperatorKind::NotEqual),
                    e => Err(SimpleErrorWithPos {
                        kind: ParserError::UnexpectedToken {
                            pat: TokenKind::EqualityOps,
                            unmatch: e,
                        },
                        position: token.position,
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
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
        let Token::Digits { sequence, suffix } = n.data else {
            return Err(SimpleErrorWithPos {
                kind: ParserError::UnexpectedToken {
                    pat: IntLiteral,
                    unmatch: n.data
                },
                position: n.position,
            })
        };

        let x = sequence.as_str().parse::<i64>().map_err(|e| SimpleErrorWithPos {
            kind: ParserError::UnParsableIntLiteral {
                error: e
            },
            position: n.position,
        })?;

        fn check_bounds<As: Bounded + Into<i64>>(ty: &str, token_pos: SourcePos, v: i64) -> Result<(i64, Option<Box<str>>), SimpleErrorWithPos> {
            let s = ty.to_string().into_boxed_str();
            if v < As::min_value().into() || As::max_value().into() < v {
                Err(SimpleErrorWithPos {
                    kind: ParserError::OverflowedLiteral {
                        tp: s,
                        min: As::min_value().into(),
                        max: As::max_value().into(),
                        value: v
                    },
                    position: token_pos
                })
            } else {
                Ok((v, Some(s)))
            }
        }

        let (i, suffix) = suffix.as_ref().map(|y| {
            match y.as_ref() {
                "i8"  => check_bounds::<i8>("i8", n.position, x),
                "i16" => check_bounds::<i16>("i16", n.position, x),
                "i32" => check_bounds::<i32>("i32", n.position, x),
                "i64" => check_bounds::<i64>("i64", n.position, x),
                _ => unreachable!()
            }
        }).unwrap_or(Ok((x, None)))?;

        Ok((i, suffix.map(|x| x.to_string().into_boxed_str())))
    }

    fn parse_type(&self) -> Result<TypeSignature, SimpleErrorWithPos> {
        let WithPosition { position, data: maybe_tp } = self.lexer.next();

        match maybe_tp {
            Token::Identifier { inner } => Ok(inner.into()),
            Token::SymLeftPar => {
                debug!("type:tuple");
                self.lexer.parse_fallible(|| {
                    let mut vec = vec![];

                    loop {
                        let x = self.parse_type()?;
                        debug!("`- {x:?}");
                        vec.push(x);
                        debug!("{:?}", self.lexer.peek().data);
                        if self.lexer.peek().data != Token::SymComma {
                            break
                        }

                        self.lexer.next();
                    }

                    debug!("type:tuple:accumulator = {vec:?}");

                    self.read_and_consume_or_report_unexpected_token(Token::SymRightPar)?;

                    if vec.len() < 2 {
                        let l = vec.len();
                        warn!("type:tuple = error (not enough length = {l})");
                        Err(SimpleErrorWithPos {
                            kind: ParserError::InsufficientElementsForTupleLiteral(match l {
                                0 => UnexpectedTupleLiteralElementCount::Zero,
                                1 => UnexpectedTupleLiteralElementCount::One,
                                _ => unreachable!(),
                            }),
                            position,
                        })
                    } else {
                        Ok(TypeSignature::Tuple(vec))
                    }
                })
            }
            other_token => Err(SimpleErrorWithPos {
                kind: ParserError::UnexpectedToken {
                    pat: TokenKind::StartOfTypeSignature,
                    unmatch: other_token,
                },
                position,
            })
        }
    }

    fn parse_variable_declaration(&self) -> Result<Statement, SimpleErrorWithPos> {
        debug!("decl:var");
        self.read_and_consume_or_report_unexpected_token(Token::VarKeyword)?;
        let pattern = self.parse_atomic_pattern()?;

        // optionally, allow type annotation
        let type_annotation = self.lexer.parse_fallible(|| {
            match self.lexer.next().data {
                Token::SymColon => {},
                _ => return Err(())
            }

            // FIXME: discarding error
            let x = self.parse_type().map_err(|e| {
                warn!("{e:?}");
            })?;

            Ok(x)
        }).ok();

        debug!("type annotation: {type_annotation:?}");

        self.read_and_consume_or_report_unexpected_token(Token::SymEq)?;
        let expression = self.parse_lowest_precedence_expression()?;

        Ok(Statement::VariableDeclaration {
            pattern,
            expression,
            type_annotation,
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
        self.read_and_consume_or_report_unexpected_token(Token::SymEq)?;
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
            self.read_and_consume_or_report_unexpected_token(Token::KeywordThen)?;
            let then_clause_value = self.parse_lowest_precedence_expression()?;
            self.read_and_consume_or_report_unexpected_token(Token::KeywordElse)?;
            let else_clause_value = self.parse_lowest_precedence_expression()?;
            Ok(Expression::If {
                condition: Box::new(condition),
                then_clause_value: Box::new(then_clause_value),
                else_clause_value: Box::new(else_clause_value),
            })
        } else {
            self.parse_equality_expression()
        }
    }

    fn parse_block_scope(&self) -> Result<Statement, SimpleErrorWithPos> {
        debug!("parser:block:scope");
        self.read_and_consume_or_report_unexpected_token(Token::KeywordBlock)?;
        if self.lexer.peek().data == Token::NewLine {
            self.lexer.next();
        }

        let mut statements = vec![];
        while let Ok(v) = self.parse_statement() {
            statements.push(v);
        }
        self.read_and_consume_or_report_unexpected_token(Token::KeywordEnd)?;

        Ok(Statement::Block {
            inner_statements: (statements),
        })
    }

    fn parse_block_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        debug!("parser:block:expr");
        if self.lexer.peek().data == Token::KeywordBlock {
            self.lexer.next();
            self.read_and_consume_or_report_unexpected_token(Token::NewLine)?;
            let mut statements = vec![];
            while let Ok(v) = self.parse_statement() {
                statements.push(v);
            }
            let final_expression = Box::new(self.parse_lowest_precedence_expression()?);
            if self.lexer.peek().data == Token::NewLine {
                self.lexer.next();
            }
            self.read_and_consume_or_report_unexpected_token(Token::KeywordEnd)?;
            Ok(Expression::Block {
                intermediate_statements: statements,
                final_expression
            })
        } else {
            self.parse_if_expression()
        }
    }

    fn parse_tuple_destruct_pattern(&self) -> Result<AtomicPattern, SimpleErrorWithPos> {
        let start = self.lexer.peek();
        self.read_and_consume_or_report_unexpected_token(Token::SymLeftPar)?;

        drop(start);

        let mut v = vec![];

        while let Ok(pattern) = self.parse_atomic_pattern() {
            v.push(pattern);

            if self.lexer.peek().data == Token::SymRightPar {
                break
            }

            self.read_and_consume_or_report_unexpected_token(Token::SymComma)?;
        }

        self.read_and_consume_or_report_unexpected_token(Token::SymRightPar)?;

        Ok(AtomicPattern::Tuple(v))
    }

    fn parse_atomic_pattern(&self) -> Result<AtomicPattern, SimpleErrorWithPos> {
        let it = self.lexer.peek();

        match it.data {
            Token::Identifier { inner: name } => {
                self.lexer.next();
                Ok(AtomicPattern::Bind(name))
            }
            Token::SymUnderscore => {
                self.lexer.next();
                Ok(AtomicPattern::Discard)
            }
            Token::SymLeftPar => {
                self.parse_tuple_destruct_pattern()
            }
            other_token => {
                Err(SimpleErrorWithPos {
                    position: it.position,
                    kind: ParserError::UnexpectedToken {
                        pat: TokenKind::Identifier,
                        unmatch: other_token,
                    }
                })
            }
        }
    }

    /// 現在のトークンが指定されたトークンならそのトークンをそのまま返した上でレキサーを1個進める。そうではないなら[`ParseError::UnexpectedToken`]を返す。
    fn read_and_consume_or_report_unexpected_token(&self, token: Token) -> Result<Token, SimpleErrorWithPos> {
        let peek = self.lexer.peek();
        if peek.data == token {
            self.lexer.next();
            Ok(token)
        } else {
            Err(SimpleErrorWithPos {
                kind: ParserError::UnexpectedToken {
                    pat: TokenKind::only(token),
                    unmatch: peek.data
                },
                position: peek.position
            })
        }
    }
}
