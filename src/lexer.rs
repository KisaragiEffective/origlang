use std::cell::RefCell;

use anyhow::{anyhow, Result};
use crate::char_list::{ASCII_LOWERS, ASCII_NUMERIC_CHARS};

static KEYWORDS: [&str; 5] =
    ["var", "if", "else", "then", "exit"];

pub struct Lexer {
    current_index: RefCell<usize>,
    current_source: String,
}

impl Lexer {
    pub fn create(source: &str) -> Self {
        Self {
            current_source: source.to_string(),
            current_index: RefCell::new(0)
        }
    }

    fn drain_space(&self) {
        while !self.reached_end() && self.current_char().expect("oops") == ' ' {
            *self.current_index.borrow_mut() += 1;
        }
    }

    pub fn next(&self) -> Token {
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile
        }

        let c = self.current_char().expect("oops");
        match c {
            '\n' => {
                self.advance();
                Token::NewLine
            },
            '=' => {
                self.advance();
                Token::SymEq
            },
            '+' => {
                self.advance();
                Token::SymPlus
            },
            '-' => {
                self.advance();
                Token::SymMinus
            },
            '(' => {
                self.advance();
                Token::SymLeftPar
            },
            ')' => {
                self.advance();
                Token::SymRightPar
            },
            c if ASCII_NUMERIC_CHARS.contains(&c) => self.scan_digits().expect("oops"),
            c if ASCII_LOWERS.contains(&c) => {
                let scan_result = self.scan_lowers().expect("oops");
                let is_keyword = KEYWORDS.contains(&scan_result.as_str());
                if is_keyword {
                    if scan_result == "var" {
                        Token::VarKeyword
                    } else {
                        Token::Reserved {
                            matched: scan_result
                        }
                    }
                } else {
                    Token::Identifier { inner: scan_result }
                }
            },
            other => Token::UnexpectedChar {
                index: *self.current_index.borrow(),
                char: other,
            }
        }
    }

    fn scan_digits(&self) -> Result<Token> {
        let mut buf = String::new();
        loop {
            if self.reached_end() {
                break
            }

            // DON'T CONSUME!!
            let c = self.current_char()?;
            if !ASCII_NUMERIC_CHARS.contains(&c) {
                break
            }
            let c = self.consume_char()?;

            buf.push(c);
        }

        Ok(Token::Digits {
            sequence: buf
        })
    }

    fn scan_lowers(&self) -> Result<String> {
        let mut buf = String::new();
        loop {
            if self.reached_end() {
                break
            }

            // DON'T CONSUME!!
            let c = self.current_char()?;
            if !ASCII_LOWERS.contains(&c) {
                break
            }
            let c = self.consume_char()?;

            buf.push(c);
        }

        Ok(buf)
    }
    
    pub fn peek(&self) -> Token {
        let current_index = *self.current_index.borrow();
        let token = self.next();
        *self.current_index.borrow_mut() = current_index;
        token
    }

    fn current_char(&self) -> Result<char> {
        self.current_source
            .as_str()
            .chars()
            .nth(*self.current_index.borrow())
            .ok_or_else(||
                anyhow!("index: out of range (idx={request}, max={max})",
                    request = *self.current_index.borrow(),
                    max = self.current_source.len()
                )
            )
    }

    fn consume_char(&self) -> Result<char> {
        let c = self.current_char()?;
        *self.current_index.borrow_mut() += 1;
        Ok(c)
    }

    fn reached_end(&self) -> bool {
        *self.current_index.borrow() >= self.current_source.len()
    }

    fn advance(&self) {
        self.advance_by(1);
    }

    fn advance_by(&self, step: usize) {
        *self.current_index.borrow_mut() += step;
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Identifier {
        inner: String,
    },
    Digits {
        sequence: String,
    },
    UnexpectedChar {
        index: usize,
        char: char,
    },
    EndOfFile,
    /// `"\n"`
    NewLine,
    /// `"var"`
    VarKeyword,
    /// `"="`
    SymEq,
    /// `"+"`
    SymPlus,
    /// `"-"`
    SymMinus,
    /// `"("`
    SymLeftPar,
    /// `")"`
    SymRightPar,
    /// reserved for future use.
    Reserved {
        matched: String,
    },

}