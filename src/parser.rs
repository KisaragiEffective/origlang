use crate::ast::{RootAst, SourcePos, Statement, WithPosition};
use crate::lexer::{Lexer, Token};
use crate::ast::after_parse::{BinaryOperatorKind, Expression};
use std::string::ToString;
use derive_more::Display;
use thiserror::{Error as ThisError};

#[derive( ThisError, Debug, Display)]
#[display(fmt = "{error_message} ({position})")]
pub struct SimpleErrorWithPos {
    pub error_message: String,
    pub position: SourcePos,
}

impl SimpleErrorWithPos {}

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
                    error_message: format!("parse_statement assertion: unconsumed token found: {other:?}")
                })
            }
        }
    }

    fn parse_statement(&self) -> Result<Statement, SimpleErrorWithPos> {
        let head = self.lexer.peek().data;
        let result = if head == Token::VarKeyword {
            self.parse_variable_declaration()?
        } else {
            // assuming expression
            Statement::Print {
                expression: self.parse_lowest_precedence_expression()?
            }
        };
        // 文は絶対に改行で終わる必要がある
        {
            let next = self.lexer.next();
            if next.data != Token::NewLine {
                return Err(SimpleErrorWithPos {
                    position: next.position,
                    error_message: "Statement must be terminated by a newline".to_string(),
                })
            }
        }
        Ok(result)
    }

    /// 現在のトークン位置から基本式をパースしようと試みる。
    /// 事前条件: 現在のトークン位置が基本式として有効である必要がある
    /// 違反した場合はErr。
    fn parse_first(&self) -> Result<Expression, SimpleErrorWithPos> {
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
                self.parse_int_literal().map(|parsed| {
                    Expression::IntLiteral(parsed)
                })
            }
            Token::SymLeftPar => {
                assert_eq!(self.lexer.next().data, Token::SymLeftPar);
                // FIXME: (1 == 2)を受け付けない
                let inner_expression = self.parse_lowest_precedence_expression()?;
                assert_eq!(self.lexer.next().data, Token::SymRightPar);
                Ok(inner_expression)
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
                    error_message: "END OF FILE!!!!!!".to_string(),
                    position: token.position,
                })
            }
            e => Err(SimpleErrorWithPos {
                error_message: format!("int literal, boolean literal or identifier is expected, but got {e:?}"),
                position: token.position,
            })
        }
    }

    /// 現在のトークン位置から乗除算をパースする。
    fn parse_multiplicative(&self) -> Result<Expression, SimpleErrorWithPos> {
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
                        error_message: format!("SyntaxError: `*` or `/` is expected, but got {e:?}"),
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
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary
            Ok(first_term.into())
        }
    }

    /// 現在のトークン位置から加減算をパースしようと試みる。
    /// 事前条件: 現在の位置が加減算として有効である必要がある
    /// 違反した場合はErr
    fn parse_additive(&self) -> Result<Expression, SimpleErrorWithPos> {
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
                        error_message: format!("+ or - was expected, but got {e:?}")
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
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary or multiplicative
            Ok(first_term.into())
        }
    }

    /// 現在の位置から比較演算式をパースしようと試みる
    fn parse_relation_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
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
                        error_message: format!("excess token: {e:?}"),
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_additive()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            Ok(first_term.into())
        }
    }

    /// 現在の位置から等価性検査式をパースしようと試みる
    fn parse_equality_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
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
                        error_message: format!("excess token: {e:?}"),
                        position: token.position,
                    })
                }
            };

            let mut acc = Expression::binary(get_operator_from_token(&operator_token)?, lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token.data) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = Expression::binary(get_operator_from_token(&operator_token)?, acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            Ok(first_term.into())
        }
    }

    /// 現在のトークンを消費して整数リテラルの生成を試みる。
    /// 事前条件: 現在のトークンが整数として有効である必要がある
    /// 違反した場合はErrを返す。
    fn parse_int_literal(&self) -> Result<i32, SimpleErrorWithPos> {
        let n = self.lexer.next();
        match n.data {
            Token::Digits { sequence } => {
                sequence.as_str().parse::<i32>().map_err(|e| SimpleErrorWithPos {
                    error_message: format!("input sequence cannot be parsed as Int: {e}"),
                    position: n.position,
                })
            }
            _ => Err(SimpleErrorWithPos {
                error_message: format!("int literal is expected, but got {token:?}", token = n.data),
                position: n.position,
            })
        }
    }

    /// 現在の`Lexer`に積まれている`Token`と期待される`Token`を比較し、違っていた場合はpanicする。
    /// この関数は`Lexer`の`Token`を一つ消費するという副作用がある。
    fn assert_token_eq_with_consumed(&self, rhs: Token) {
        let token = self.lexer.next().data;
        assert_eq!(token, rhs, "expected: {rhs:?}, got: {token:?}");
    }

    fn parse_variable_declaration(&self) -> Result<Statement, SimpleErrorWithPos> {
        self.assert_token_eq_with_consumed(Token::VarKeyword);
        let ident_token = self.lexer.next();
        let name = match ident_token.data {
            Token::Identifier { inner } => {
                inner
            }
            e => return Err(SimpleErrorWithPos {
                position: ident_token.position,
                error_message: format!("identifier was expected, got {e:?}")
            })
        };
        self.assert_token_eq_with_consumed(Token::SymEq);
        let expression = self.parse_lowest_precedence_expression()?;
        Ok(Statement::VariableDeclaration {
            identifier: name,
            expression
        })
    }

    fn parse_lowest_precedence_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        self.parse_if_expression()
    }

    fn parse_if_expression(&self) -> Result<Expression, SimpleErrorWithPos> {
        if self.lexer.peek().data == Token::KeywordIf {
            self.lexer.next();
            let condition = self.parse_if_expression()?;
            let WithPosition { position, data } = self.lexer.peek();
            if data == Token::KeywordThen {
                self.lexer.next();
                let then_clause_value = self.parse_if_expression()?;
                let WithPosition { position, data } = self.lexer.peek();
                if data == Token::KeywordElse {
                    self.lexer.next();
                    let else_clause_value = self.parse_if_expression()?;
                    Ok(Expression::If {
                        condition: Box::new(condition),
                        then_clause_value: Box::new(then_clause_value),
                        else_clause_value: Box::new(else_clause_value),
                    })
                } else {
                    Err(SimpleErrorWithPos {
                        error_message: format!("if expression requires else clause"),
                        position,
                    })
                }
            } else {
                Err(SimpleErrorWithPos {
                    error_message: format!("if expression requires then clause and else clause"),
                    position,
                })
            }
        } else {
            self.parse_equality_expression()
        }
    }
}
