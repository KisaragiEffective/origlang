use std::cell::RefCell;
use std::ops::Deref;
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

    // TODO: ドキュメンテーションで事前条件をはっきりさせるべき
    pub(crate) fn parse(&self) -> Result<RootAst, String> {
        let mut statements = vec![];
        while self.lexer.peek() != Token::EndOfFile {
            let res = self.parse_statement();
            statements.push(res?)
        }
        Ok(RootAst {
            statement: statements
        })
    }

    fn parse_statement(&self) -> Result<Statement, String> {
        let head = self.lexer.peek();
        let result = match head {
            Token::VarKeyword => {
                self.parse_variable_declaration()
            }
            _other => {
                self.parse_expression().map(|expression| Statement::Print { expression })
            }
        };

        match self.lexer.next() {
            Token::EndOfFile | Token::NewLine => {}
            other => return Err(format!("parse_statement assertion: unexpected token found: {other:?}"))
        }
        result
    }

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
            Token::Digits { sequence } => {
                self.parse_int_literal().map(|parsed| {
                    Expression::IntLiteral(parsed)
                })
            }
            _ => Err("int literal or identifier is expected".to_string())
        }
    }

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
        if token != rhs {
            panic!("expected: {rhs:?}, got: {token:?}")
        }
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
