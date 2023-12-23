use origlang_ast::{AtomicPattern, Statement, TypeSignature};
use origlang_source_span::{Pointed as WithPosition, SourcePosition as SourcePos};
use crate::lexer::Lexer;
use crate::lexer::token::Token;
use crate::lexer::token::internal::DisplayToken;

use std::string::ToString;
use derive_more::Display;
use log::{debug, warn};
use num_traits::Bounded;
use crate::parser::recursive_descent::{BlockExpressionRule, IntoAbstractSyntaxTree, TryFromParser};
use self::error::{ParserError, ParserErrorInner, UnexpectedTupleLiteralElementCount};
use crate::parser::TokenKind::IntLiteral;

pub mod error;
pub mod recover;
pub mod recursive_descent;

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
    pub fn parse<E: TryFromParser>(&self) -> Result<E, E::Err> {
        E::parse(self)
    }

    /// 現在のトークンを消費して整数リテラルの生成を試みる。
    /// 事前条件: 現在のトークンが整数として有効である必要がある
    /// 違反した場合はErrを返す。
    fn parse_int_literal(&self) -> Result<(i64, Option<Box<str>>), ParserError> {
        debug!("expr:lit:int");
        let n = self.lexer.next();
        let Token::Digits { sequence, suffix } = n.data else {
            return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                    pat: IntLiteral,
                    unmatch: n.data
                }, n.position,))
        };

        let x = sequence.as_str().parse::<i64>().map_err(|e| ParserError::new(ParserErrorInner::UnParsableIntLiteral {
                error: e
            }, n.position,))?;

        fn check_bounds<As: Bounded + Into<i64>>(ty: &str, token_pos: SourcePos, v: i64) -> Result<(i64, Option<Box<str>>), ParserError> {
            let s = ty.to_string().into_boxed_str();
            if v < As::min_value().into() || As::max_value().into() < v {
                Err(ParserError::new(ParserErrorInner::OverflowedLiteral {
                        tp: s,
                        min: As::min_value().into(),
                        max: As::max_value().into(),
                        value: v
                    }, token_pos))
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

    fn parse_type(&self) -> Result<TypeSignature, ParserError> {
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
                        Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(match l {
                            0 => UnexpectedTupleLiteralElementCount::Zero,
                            1 => UnexpectedTupleLiteralElementCount::One,
                            _ => unreachable!(),
                        }), position))
                    } else {
                        Ok(TypeSignature::Tuple(vec))
                    }
                })
            }
            other_token => Err(ParserError::new(
                ParserErrorInner::UnexpectedToken {
                    pat: TokenKind::StartOfTypeSignature,
                    unmatch: other_token,
                },
                position,
            ))
        }
    }

    fn parse_variable_declaration(&self) -> Result<Statement, ParserError> {
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
        debug!("expr:lowest");
        let expression = self.parse::<BlockExpressionRule>().map(|x| x.into_ast())?;

        Ok(Statement::VariableDeclaration {
            pattern,
            expression,
            type_annotation,
        })
    }

    fn parse_variable_assignment(&self) -> Result<Statement, ParserError> {
        debug!("assign:var");
        let ident_token = self.lexer.next();
        let Token::Identifier { inner: name } = ident_token.data else {
            return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                    pat: TokenKind::Identifier,
                    unmatch: ident_token.data }, ident_token.position))
        };
        self.read_and_consume_or_report_unexpected_token(Token::SymEq)?;
        debug!("expr:lowest");
        let expression = self.parse::<BlockExpressionRule>().map(|x| x.into_ast())?;
        Ok(Statement::VariableAssignment {
            identifier: name,
            expression
        })
    }

    fn parse_block_scope(&self) -> Result<Statement, ParserError> {
        debug!("parser:block:scope");
        self.read_and_consume_or_report_unexpected_token(Token::KeywordBlock)?;
        if self.lexer.peek().data == Token::NewLine {
            self.lexer.next();
        }

        let mut statements = vec![];
        while let Ok(v) = self.parse() {
            statements.push(v);
        }
        self.read_and_consume_or_report_unexpected_token(Token::KeywordEnd)?;

        Ok(Statement::Block {
            inner_statements: (statements),
        })
    }

    fn parse_tuple_destruct_pattern(&self) -> Result<AtomicPattern, ParserError> {
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

    fn parse_atomic_pattern(&self) -> Result<AtomicPattern, ParserError> {
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
                Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                    pat: TokenKind::Identifier,
                    unmatch: other_token
                }, it.position))
            }
        }
    }

    /// 現在のトークンが指定されたトークンならそのトークンをそのまま返した上でレキサーを1個進める。そうではないなら[`ParseError::UnexpectedToken`]を返す。
    fn read_and_consume_or_report_unexpected_token(&self, token: Token) -> Result<Token, ParserError> {
        let peek = self.lexer.peek();
        if peek.data == token {
            self.lexer.next();
            Ok(token)
        } else {
            Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                    pat: TokenKind::only(token),
                    unmatch: peek.data
                }, peek.position))
        }
    }
}
