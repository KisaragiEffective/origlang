

use crate::{Expression, Lexer, RootAst, Statement};
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
                expression: self.parse_expression()?
            }
        };
        Ok(result)
    }

    /// 現在のトークン位置から式をパースしようと試みる。
    /// 事前条件: 現在の位置が式として有効である必要がある
    /// 違反した場合はpanic。
    fn parse_expression(&self) -> Result<Expression, String> {
        let token = self.lexer.peek();
        match token {
            Token::Identifier { inner } => {
                // consume
                self.lexer.next();
                Ok(Expression::Variable {
                    name: inner
                })
            }
            Token::Digits { .. } => {
                self.parse_int_literal().map(|parsed| {
                    Expression::IntLiteral(parsed)
                })
            }
            _ => Err("int literal or identifier is expected".to_string())
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
        let expression = self.parse_expression()?;
        Ok(Statement::VariableDeclaration {
            identifier: name,
            expression
        })
    }
}
