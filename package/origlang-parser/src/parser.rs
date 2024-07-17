use origlang_ast::{AtomicPattern, RootAst, Statement, TypeSignature};
use origlang_source_span::{Pointed as WithPosition, Pointed, SourcePosition as SourcePos, SourcePosition};
use origlang_lexer::Lexer;
use origlang_lexer::token::Token;
use origlang_lexer::token::internal::DisplayToken;

use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use std::string::ToString;
use derive_more::Display;
use log::{debug, warn};
use num_traits::Bounded;
use crate::error::{ParserError, ParserErrorInner, UnexpectedTupleLiteralElementCount};
use crate::error::ParserErrorInner::EndOfFileError;
use crate::recover::PartiallyParseFixCandidate;
use crate::parser::TokenKind::IntLiteral;
use origlang_token_stream::TokenStream;

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
    fn only(token: &Token) -> Self {
        Self::Only(token.display())
    }
}

pub struct Parser {
    lexer: TokenStream
}

impl Parser {
    #[must_use = "Parser do nothing unless calling parsing function"]
    #[deprecated]
    pub fn create(source: &str) -> Self {
        Self::new(Lexer::create(source).into())
    }
    
    pub const fn new(token_stream: TokenStream) -> Self {
        Self {
            lexer: token_stream
        }
    }
}

impl Parser {
    /// プログラムが文の列とみなしてパースを試みる。
    /// 事前条件: プログラム全体が任意個の文として分解できる
    /// # Errors
    /// プログラムのパースに失敗したときErr。
    pub fn parse(&self) -> Result<RootAst, ParserError> {
        let mut statements = vec![];
        while self.lexer.peek().is_some_and(|x| x.data != Token::EndOfFile) {
            let res = self.parse_statement()?;
            statements.push(res);
        }

        {
            let t = self.lexer.peek().unwrap_or(&self.lexer.end_of_file_token()).clone();
            self.lexer.next();
            match t.data {
                Token::EndOfFile | Token::NewLine => Ok(RootAst {
                    statement: statements,
                }),
                other => Err(ParserError::new(ParserErrorInner::UnconsumedToken { token: other }, t.position)),
            }
        }
    }

    fn parse_statement(&self) -> Result<Statement, ParserError> {
        // dbg!(&head);
        while self.lexer.peek().is_some_and(|x| x.data == Token::NewLine) {
            self.lexer.next();
        }

        if self.lexer.peek().is_none() {
            return Ok(Statement::Exit)
        }

        let head1 = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
        let head = &head1.data;
        let pos = head1.position;

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
                Statement::Comment { content: content.clone() }
            }
            Token::KeywordExit => {
                self.lexer.next();
                Statement::Exit
            }
            Token::KeywordType => {
                self.lexer.next();
                let aliased = self.lexer.peek();
                self.lexer.next();

                let Some(Token::Identifier { inner: aliased }) = aliased.map(|x| &x.data) else {
                    let unmatch = aliased.map(|x| &x.data).cloned().unwrap_or(Token::EndOfFile);
                    let position = aliased.map_or(self.lexer.last_position, |x| x.position);
                    return Err(Self::create_unexpected_token_error(TokenKind::Identifier, unmatch, position)); 
                };

                self.read_and_consume_or_report_unexpected_token(&Token::SymEq)?;
                let Ok(replace_with) = self.lexer.parse_fallible(|| self.parse_type()) else {
                    let p = self.lexer.peek().unwrap_or(&self.lexer.end_of_file_token()).clone();
                    return Err(Self::create_unexpected_token_error(TokenKind::StartOfTypeSignature, p.data, p.position));
                };

                Statement::TypeAliasDeclaration {
                    new_name: aliased.clone(),
                    replace_with,
                }
            }
            x => {
                return Err(Self::create_unexpected_token_error(TokenKind::Statement, x.clone(), pos))
            }
        };

        // 文は絶対に改行かEOFで終わる必要がある
        let next = self.lexer.peek();
        self.lexer.next();
        
        if next.map(|x| &x.data) != Some(&Token::NewLine) && next.is_some() {
            Err(ParserError::new(ParserErrorInner::PartiallyParsed {
                    hint: vec![
                        PartiallyParseFixCandidate::InsertAfter {
                            tokens: vec![ Token::NewLine ]
                        }
                    ],
                    intermediate_state: vec![],
            }, next.map_or(self.lexer.last_position, |x| x.position)))
        } else {
            Ok(s)
        }
    }

    /// 現在のトークン位置から基本式をパースしようと試みる。
    /// 事前条件: 現在のトークン位置が基本式として有効である必要がある
    /// 違反した場合はErr。
    fn parse_first(&self) -> Result<Expression, ParserError> {
        debug!("expr:first");
        let token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
        
        match &token.data {
            Token::Identifier { inner } => {
                // consume
                self.lexer.next();
                Ok(Expression::Variable {
                    ident: inner.clone()
                })
            }
            Token::SymUnderscore => {
                self.lexer.next();
                Err(ParserError::new(ParserErrorInner::UnderscoreCanNotBeRightHandExpression, token.position,))
            }
            Token::Digits { .. } => {
                self.parse_int_literal().map(|(parsed, suffix)| {
                    Expression::IntLiteral { value: parsed, suffix }
                })
            }
            Token::SymLeftPar => {
                
                assert_eq!(self.lexer.peek().map(|x| &x.data), Some(&Token::SymLeftPar));
                self.lexer.next();
                // FIXME: (1 == 2)を受け付けない
                if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::SymRightPar {
                    self.lexer.next();
                    Ok(Expression::UnitLiteral)
                } else if let Ok(expr_tuple) = self.lexer.parse_fallible(|| self.parse_tuple_expression()) {
                    Ok(expr_tuple)
                } else {
                    let inner_expression = self.parse_lowest_precedence_expression()?;
                    self.read_and_consume_or_report_unexpected_token(&Token::SymRightPar)?;
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
                Err(ParserError::new(EndOfFileError, token.position,))
            }
            Token::StringLiteral(s) => {
                self.lexer.next();
                Ok(Expression::StringLiteral(s.clone()))
            }
            e => Err(Self::create_unexpected_token_error(TokenKind::First, e.clone(), token.position))
        }
    }


    fn parse_tuple_expression(&self) -> Result<Expression, ParserError> {
        self.lexer.parse_fallible(|| {
            debug!("expr:tuple");

            let mut buf = vec![];
            while let Ok(e) = self.parse_lowest_precedence_expression() {
                buf.push(e);
                let peek = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
                if peek.data == Token::SymRightPar {
                    self.lexer.next();
                    break
                } else if peek.data != Token::SymComma {
                    return Err(Self::create_unexpected_token_error(TokenKind::Only(Token::SymComma.display()), peek.data.clone(), peek.position))
                }

                self.lexer.next();
            }

            let bl = buf.len();

            if bl == 0 {
                // disallow ()
                return Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::Zero), self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.position,))
            } else if bl == 1 {
                // disallow (expr)
                return Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::One), self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.position,))
            }

            Ok(Expression::Tuple {
                expressions: buf
            })
        })
    }
    /// 現在のトークン位置から乗除算をパースする。
    fn parse_multiplicative(&self) -> Result<Expression, ParserError> {
        debug!("expr:mul");
        let first_term = self.parse_first()?;
        if self.lexer.peek().is_none() {
            return Ok(first_term)
        }
        let next_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
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
                    e => Err(Self::create_unexpected_token_error(TokenKind::MultiplicativeOps, e.clone(), token.position))
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek()
                .ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            while asterisk_or_slash(&operator_token.data) {
                // SymAsterisk | SymSlash
                self.lexer.next();
                let new_rhs = self.parse_first()?;
                // 左結合になるように詰め替える
                // これは特に除算のときに欠かせない処理である
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
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
    fn parse_additive(&self) -> Result<Expression, ParserError> {
        debug!("expr:add");
        let first_term = self.parse_multiplicative()?;
        if self.lexer.peek().is_none() {
            return Ok(first_term)
        }
        let Some(next_token) = self.lexer.peek() else {
            return Err(ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))
        };
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
                    e => Err(Self::create_unexpected_token_error(TokenKind::AdditiveOps, e.clone(), token.position))
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            while plus_or_minus(&operator_token.data) {
                // SymPlus | SymMinus
                self.lexer.next();
                let new_rhs = self.parse_multiplicative()?;
                // 左結合になるように詰め替える
                // これは特に減算のときに欠かせない処理である
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            }
            Ok(acc)
        } else {
            // it is unary or multiplicative
            Ok(first_term)
        }
    }

    fn parse_shift_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:shift");
        let first_term = self.parse_additive()?;
        if self.lexer.peek().is_none() {
            return Ok(first_term)
        }
        let next_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
        
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessLess | Token::PartMoreMore)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_relation_expression()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::PartLessLess => Ok(BinaryOperatorKind::ShiftLeft),
                    Token::PartMoreMore => Ok(BinaryOperatorKind::ShiftRight),
                    e => Err(Self::create_unexpected_token_error(TokenKind::ShiftOps, e.clone(), token.position))
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            }

            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在の位置から比較演算式をパースしようと試みる
    fn parse_relation_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:rel");
        let first_term = self.parse_shift_expression()?;
        if self.lexer.peek().is_none() {
            return Ok(first_term)
        }
        let next_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessEq | Token::PartMoreEq | Token::SymLess | Token::SymMore | Token::PartLessEqMore)
        };

        if is_relation_operator(&next_token.data) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term;
            let rhs = self.parse_shift_expression()?;
            let get_operator_from_token = |token: &WithPosition<Token>| {
                match &token.data {
                    Token::PartLessEq => Ok(BinaryOperatorKind::LessEqual),
                    Token::PartMoreEq => Ok(BinaryOperatorKind::MoreEqual),
                    Token::SymLess => Ok(BinaryOperatorKind::Less),
                    Token::SymMore => Ok(BinaryOperatorKind::More),
                    Token::PartLessEqMore => Ok(BinaryOperatorKind::ThreeWay),
                    e => Err(Self::create_unexpected_token_error(TokenKind::ComparisonOps, e.clone(), token.position))
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_additive()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            }
            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在の位置から等価性検査式をパースしようと試みる
    fn parse_equality_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:eq");
        let first_term = self.parse_relation_expression()?;
        if self.lexer.peek().is_none() {
            return Ok(first_term)
        }
        let next_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
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
                    e => Err(Self::create_unexpected_token_error(TokenKind::EqualityOps, e.clone(), token.position))
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(operator_token)?, lhs, rhs);
            let mut operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(operator_token)?, acc, new_rhs);
                operator_token = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
            }
            Ok(acc)
        } else {
            Ok(first_term)
        }
    }

    /// 現在のトークンを消費して整数リテラルの生成を試みる。
    /// 事前条件: 現在のトークンが整数として有効である必要がある
    /// 違反した場合はErrを返す。
    fn parse_int_literal(&self) -> Result<(i64, Option<Box<str>>), ParserError> {
        debug!("expr:lit:int");
        let n = self.lexer.peek();
        self.lexer.next();
        let Some(Pointed { data: Token::Digits { sequence, suffix }, position }) = n else {
            let unmatch =n.map_or(&Token::EndOfFile, |x| &x.data).clone();
            let position = n.map_or(self.lexer.last_position, |x| x.position);
            return Err(Self::create_unexpected_token_error(TokenKind::IntLiteral, unmatch, position));
        };

        let x = sequence.as_str().parse::<i64>().map_err(|e| ParserError::new(ParserErrorInner::UnParsableIntLiteral {
                error: e
            }, n.map_or(self.lexer.last_position, |x| x.position) ))?;

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
                "i8"  => check_bounds::<i8>("i8", *position, x),
                "i16" => check_bounds::<i16>("i16", *position, x),
                "i32" => check_bounds::<i32>("i32", *position, x),
                "i64" => check_bounds::<i64>("i64", *position, x),
                _ => unreachable!()
            }
        }).unwrap_or(Ok((x, None)))?;

        Ok((i, suffix.map(|x| x.to_string().into_boxed_str())))
    }

    fn parse_type(&self) -> Result<TypeSignature, ParserError> {
        let Some(WithPosition { position, data: maybe_tp }) = self.lexer.peek() else {
            return Err(Self::create_unexpected_token_error(TokenKind::StartOfTypeSignature, Token::EndOfFile, self.lexer.last_position))
        };

        self.lexer.next();

        match maybe_tp {
            Token::Identifier { inner } => Ok(inner.clone().into()),
            Token::SymLeftPar => {
                debug!("type:tuple");
                self.lexer.parse_fallible(|| {
                    let mut vec = vec![];

                    loop {
                        let x = self.parse_type()?;
                        debug!("`- {x:?}");
                        vec.push(x);
                        debug!("{:?}", self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data);
                        if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data != Token::SymComma {
                            break
                        }

                        self.lexer.next();
                    }

                    debug!("type:tuple:accumulator = {vec:?}");

                    self.read_and_consume_or_report_unexpected_token(&Token::SymRightPar)?;

                    if vec.len() < 2 {
                        let l = vec.len();
                        warn!("type:tuple = error (not enough length = {l})");
                        Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(match l {
                            0 => UnexpectedTupleLiteralElementCount::Zero,
                            1 => UnexpectedTupleLiteralElementCount::One,
                            _ => unreachable!(),
                        }), *position))
                    } else {
                        Ok(TypeSignature::Tuple(vec))
                    }
                })
            }
            other_token => Err(Self::create_unexpected_token_error(TokenKind::StartOfTypeSignature, other_token.clone(), *position))
        }
    }

    fn parse_variable_declaration(&self) -> Result<Statement, ParserError> {
        debug!("decl:var");
        self.read_and_consume_or_report_unexpected_token(&Token::VarKeyword)?;
        let pattern = self.parse_atomic_pattern()?;

        // optionally, allow type annotation
        let type_annotation = self.lexer.parse_fallible(|| {
            match self.lexer.peek() {
                Some(Pointed { data: Token::SymColon, .. }) => {},
                _ => return Err(())
            }
            
            self.lexer.next();

            // FIXME: discarding error
            let x = self.parse_type().map_err(|e| {
                warn!("{e:?}");
            })?;

            Ok(x)
        }).ok();

        debug!("decl:var:annotation: {type_annotation:?}");

        self.read_and_consume_or_report_unexpected_token(&Token::SymEq)?;
        debug!("decl:var:expr");
        let expression = self.parse_lowest_precedence_expression()?;

        Ok(Statement::VariableDeclaration {
            pattern,
            expression,
            type_annotation,
        })
    }

    fn parse_variable_assignment(&self) -> Result<Statement, ParserError> {
        debug!("assign:var");
        let ident_token = self.lexer.peek().unwrap_or(&self.lexer.end_of_file_token()).clone();
        self.lexer.next();
        let Token::Identifier { inner: name } = ident_token.data else {
            return Err(Self::create_unexpected_token_error(TokenKind::Identifier, ident_token.data, ident_token.position))
        };
        
        self.read_and_consume_or_report_unexpected_token(&Token::SymEq)?;
        debug!("assign:var:expr");
        let expression = self.parse_lowest_precedence_expression()?;
        Ok(Statement::VariableAssignment {
            identifier: name,
            expression
        })
    }

    fn parse_lowest_precedence_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:lowest");
        self.parse_block_expression()
    }

    fn parse_if_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:if");
        if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::KeywordIf {
            self.lexer.next();
            let condition = self.parse_lowest_precedence_expression()?;
            self.read_and_consume_or_report_unexpected_token(&Token::KeywordThen)?;
            let then_clause_value = self.parse_lowest_precedence_expression()?;
            self.read_and_consume_or_report_unexpected_token(&Token::KeywordElse)?;
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

    fn parse_block_scope(&self) -> Result<Statement, ParserError> {
        debug!("statement:block");
        self.read_and_consume_or_report_unexpected_token(&Token::KeywordBlock)?;
        if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::NewLine {
            self.lexer.next();
        }

        let mut statements = vec![];
        while let Ok(v) = self.parse_statement() {
            statements.push(v);
        }
        self.read_and_consume_or_report_unexpected_token(&Token::KeywordEnd)?;

        Ok(Statement::Block {
            inner_statements: (statements),
        })
    }

    fn parse_block_expression(&self) -> Result<Expression, ParserError> {
        debug!("expr:block");
        if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::KeywordBlock {
            self.lexer.next();
            self.read_and_consume_or_report_unexpected_token(&Token::NewLine)?;
            let mut statements = vec![];
            while let Ok(v) = self.parse_statement() {
                statements.push(v);
            }
            let final_expression = Box::new(self.parse_lowest_precedence_expression()?);
            if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::NewLine {
                self.lexer.next();
            }
            self.read_and_consume_or_report_unexpected_token(&Token::KeywordEnd)?;
            Ok(Expression::Block {
                intermediate_statements: statements,
                final_expression
            })
        } else {
            self.parse_if_expression()
        }
    }

    fn parse_tuple_destruct_pattern(&self) -> Result<AtomicPattern, ParserError> {
        debug!("pattern:tuple");
        self.read_and_consume_or_report_unexpected_token(&Token::SymLeftPar)?;

        let mut v = vec![];

        while let Ok(pattern) = self.parse_atomic_pattern() {
            debug!("pattern:tuple[{}] = {pattern:?}", v.len());
            v.push(pattern);

            if self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?.data == Token::SymRightPar {
                debug!("pattern:tuple:end");
                break
            }

            self.read_and_consume_or_report_unexpected_token(&Token::SymComma)?;
        }

        self.read_and_consume_or_report_unexpected_token(&Token::SymRightPar)?;

        Ok(AtomicPattern::Tuple(v))
    }

    fn parse_atomic_pattern(&self) -> Result<AtomicPattern, ParserError> {
        debug!("pattern:atomic");
        let it = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;

        match &it.data {
            Token::Identifier { inner: name } => {
                self.lexer.next();
                Ok(AtomicPattern::Bind(name.clone()))
            }
            Token::SymUnderscore => {
                self.lexer.next();
                Ok(AtomicPattern::Discard)
            }
            Token::SymLeftPar => {
                self.parse_tuple_destruct_pattern()
            }
            other_token => Err(Self::create_unexpected_token_error(TokenKind::Identifier, other_token.clone(), it.position)),
        }
    }

    /// 現在のトークンが指定されたトークンならそのトークンをそのまま返した上でレキサーを1個進める。そうではないなら[`ParseError::UnexpectedToken`]を返す。
    fn read_and_consume_or_report_unexpected_token(&self, token: &Token) -> Result<(), ParserError> {
        let peek = self.lexer.peek().ok_or_else(|| ParserError::new(ParserErrorInner::EndOfFileError, self.lexer.last_position))?;
        if &peek.data == token {
            self.lexer.next();
            Ok(())
        } else {
            Err(Self::create_unexpected_token_error(TokenKind::only(token), peek.data.clone(), peek.position))
        }
    }
    
    const fn create_unexpected_token_error(expected_kind: TokenKind, token: Token, position: SourcePosition) -> ParserError {
        ParserError::new(
            ParserErrorInner::UnexpectedToken {
                pat: expected_kind,
                unmatch: token,
            },
            position
        )
    }
}
