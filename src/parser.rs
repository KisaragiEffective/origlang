

use crate::{First, Lexer, RootAst, Statement};
use crate::ast::{BuiltinOperatorKind, Additive, Multiplicative, MultiplicativeOperatorKind, AdditiveOperatorKind, RelationExpression, RelationExpressionOperator, EqualityExpression, EqualityExpressionOperator, IfExpression, LowestPrecedenceExpression};
use crate::lexer::Token;

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
    pub fn parse(&self) -> Result<RootAst, String> {
        let mut statements = vec![];
        while self.lexer.peek() != Token::EndOfFile {
            let res = self.parse_statement()?;
            statements.push(res);
        }

        match self.lexer.next() {
            Token::EndOfFile | Token::NewLine => {}
            other => panic!("parse_statement assertion: unconsumed token found: {other:?}")
        }
        Ok(RootAst {
            statement: statements
        })
    }

    fn parse_statement(&self) -> Result<Statement, String> {
        let head = self.lexer.peek();
        let result = if head == Token::VarKeyword {
            self.parse_variable_declaration()?
        } else {
            // assuming expression
            Statement::Print {
                expression: self.parse_lowest_precedence_expression()?
            }
        };
        let result = Ok(result);
        // 文は絶対に改行で終わる必要がある
        assert_eq!(self.lexer.next(), Token::NewLine, "statement must be terminated by a newline");
        result
    }

    /// 現在のトークン位置から基本式をパースしようと試みる。
    /// 事前条件: 現在のトークン位置が基本式として有効である必要がある
    /// 違反した場合はErr。
    fn parse_first(&self) -> Result<First, String> {
        let token = self.lexer.peek();
        match token {
            Token::Identifier { inner } => {
                // consume
                self.lexer.next();
                Ok(First::Variable {
                    name: inner
                })
            }
            Token::Digits { .. } => {
                self.parse_int_literal().map(|parsed| {
                    First::IntLiteral(parsed)
                })
            }
            Token::SymLeftPar => {
                assert_eq!(self.lexer.next(), Token::SymLeftPar);
                let inner_expression = self.parse_additive()?;
                assert_eq!(self.lexer.next(), Token::SymRightPar);
                Ok(First::parenthesized(inner_expression.into()))
            }
            Token::KeywordTrue => {
                self.lexer.next();
                Ok(First::True)
            }
            Token::KeywordFalse => {
                self.lexer.next();
                Ok(First::False)
            }
            Token::EndOfFile => {
                Err("END OF FILE!!!".to_string())
            }
            e => Err(format!("int literal, boolean literal or identifier is expected, but got {e:?}"))
        }
    }

    /// 現在のトークン位置から乗除算をパースする。
    fn parse_multiplicative(&self) -> Result<Multiplicative, String> {
        let first_term = self.parse_first()?;
        let next_token = self.lexer.peek();
        let asterisk_or_slash = |token: &Token| {
            token == &Token::SymAsterisk || token == &Token::SymSlash
        };

        if asterisk_or_slash(&next_token) {
            // SymAsterisk | SymSlash
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term.into();
            let rhs = self.parse_first()?;
            let get_operator_from_token = |token: &Token| {
                match token {
                    Token::SymAsterisk => MultiplicativeOperatorKind::Multiple,
                    Token::SymSlash => MultiplicativeOperatorKind::Divide,
                    e => panic!("excess token: {e:?}")
                }
            };

            let mut acc = Multiplicative::binary(get_operator_from_token(&operator_token), lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while asterisk_or_slash(&operator_token) {
                // SymAsterisk | SymSlash
                self.lexer.next();
                let new_rhs = self.parse_first()?;
                // 左結合になるように詰め替える
                // これは特に除算のときに欠かせない処理である
                acc = Multiplicative::binary(get_operator_from_token(&operator_token), acc, new_rhs.into());
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
    fn parse_additive(&self) -> Result<Additive, String> {
        let first_term = self.parse_multiplicative()?;
        let next_token = self.lexer.peek();
        let plus_or_minus = |token: &Token| {
            token == &Token::SymPlus || token == &Token::SymMinus
        };

        if plus_or_minus(&next_token) {
            // SymPlus | SymMinus
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term.into();
            let rhs = self.parse_multiplicative()?;
            let get_operator_from_token = |token: &Token| {
                match token {
                    Token::SymPlus => AdditiveOperatorKind::Plus,
                    Token::SymMinus => AdditiveOperatorKind::Minus,
                    e => panic!("excess token: {e:?}")
                }
            };

            let mut acc = Additive::binary(get_operator_from_token(&operator_token), lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while plus_or_minus(&operator_token) {
                // SymPlus | SymMinus
                self.lexer.next();
                let new_rhs = self.parse_multiplicative()?;
                // 左結合になるように詰め替える
                // これは特に減算のときに欠かせない処理である
                acc = Additive::binary(get_operator_from_token(&operator_token), acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            // it is unary or multiplicative
            Ok(first_term.into())
        }
    }

    /// 現在の位置から比較演算式をパースしようと試みる
    fn parse_relation_expression(&self) -> Result<RelationExpression, String> {
        let first_term = self.parse_additive()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartLessEq | Token::PartMoreEq | Token::SymLess | Token::SymMore | Token::PartLessEqMore)
        };

        if is_relation_operator(&next_token) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term.into();
            let rhs = self.parse_additive()?;
            let get_operator_from_token = |token: &Token| {
                match token {
                    Token::PartLessEq => RelationExpressionOperator::LessEqual,
                    Token::PartMoreEq => RelationExpressionOperator::MoreEqual,
                    Token::SymLess => RelationExpressionOperator::Less,
                    Token::SymMore => RelationExpressionOperator::More,
                    Token::PartLessEqMore => RelationExpressionOperator::SpaceShip,
                    e => panic!("excess token: {e:?}")
                }
            };

            let mut acc = RelationExpression::binary(get_operator_from_token(&operator_token), lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token) {
                self.lexer.next();
                let new_rhs = self.parse_additive()?;
                // 左結合になるように詰め替える
                acc = RelationExpression::binary(get_operator_from_token(&operator_token), acc, new_rhs.into());
                operator_token = self.lexer.peek();
            }
            Ok(acc)
        } else {
            Ok(first_term.into())
        }
    }

    /// 現在の位置から等価性検査式をパースしようと試みる
    fn parse_equality_expression(&self) -> Result<EqualityExpression, String> {
        let first_term = self.parse_relation_expression()?;
        let next_token = self.lexer.peek();
        let is_relation_operator = |token: &Token| {
            matches!(token, Token::PartEqEq | Token::PartBangEq)
        };

        if is_relation_operator(&next_token) {
            self.lexer.next();
            let operator_token = next_token;
            let lhs = first_term.into();
            let rhs = self.parse_relation_expression()?;
            let get_operator_from_token = |token: &Token| {
                match token {
                    Token::PartEqEq => EqualityExpressionOperator::Equal,
                    Token::PartBangEq => EqualityExpressionOperator::NotEqual,
                    e => panic!("excess token: {e:?}")
                }
            };

            let mut acc = EqualityExpression::binary(get_operator_from_token(&operator_token), lhs, rhs.into());
            let mut operator_token = self.lexer.peek();
            while is_relation_operator(&operator_token) {
                self.lexer.next();
                let new_rhs = self.parse_relation_expression()?;
                // 左結合になるように詰め替える
                acc = EqualityExpression::binary(get_operator_from_token(&operator_token), acc, new_rhs.into());
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
    fn parse_int_literal(&self) -> Result<i32, String> {
        match self.lexer.next() {
            Token::Digits { sequence } => {
                sequence.as_str().parse::<i32>().map_err(|e| e.to_string())
            }
            _ => Err("int literal is expected".to_string())
        }
    }

    /// 現在の`Lexer`に積まれている`Token`と期待される`Token`を比較し、違っていた場合はpanicする。
    /// この関数は`Lexer`の`Token`を一つ消費するという副作用がある。
    fn assert_token_eq_with_consumed(&self, rhs: Token) {
        let token = self.lexer.next();
        assert_eq!(token, rhs, "expected: {rhs:?}, got: {token:?}");
    }

    fn parse_variable_declaration(&self) -> Result<Statement, String> {
        self.assert_token_eq_with_consumed(Token::VarKeyword);
        let ident_token = self.lexer.next();
        let name = match ident_token {
            Token::Identifier { inner } => {
                inner
            }
            _ => return Err("identifier expected".to_string())
        };
        self.assert_token_eq_with_consumed(Token::SymEq);
        let expression = self.parse_lowest_precedence_expression()?;
        Ok(Statement::VariableDeclaration {
            identifier: name,
            expression
        })
    }

    fn parse_lowest_precedence_expression(&self) -> Result<LowestPrecedenceExpression, String> {
        self.parse_if_expression()
    }

    fn parse_if_expression(&self) -> Result<IfExpression, String> {
        if self.lexer.peek() == Token::KeywordIf {
            self.lexer.next();
            let condition = self.parse_if_expression()?;
            if self.lexer.peek() == Token::KeywordThen {
                self.lexer.next();
                let then_clause_value = self.parse_if_expression()?;
                if self.lexer.peek() == Token::KeywordElse {
                    self.lexer.next();
                    let else_clause_value = self.parse_if_expression()?;
                    Ok(IfExpression::If {
                        condition: Box::new(condition),
                        then_clause_value: Box::new(then_clause_value),
                        else_clause_value: Box::new(else_clause_value),
                    })
                } else {
                    Err("if expression requires else clause".to_string())
                }
            } else {
                Err("if expression requires then clause and else clause".to_string())
            }
        } else {
            Ok(IfExpression::Lifted(self.parse_equality_expression()?))
        }
    }
}
