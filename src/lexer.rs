use std::cell::{Cell, RefCell};

use anyhow::{anyhow, Result};
use crate::ast::{SourcePos, WithPosition};
use crate::char_list::{ASCII_LOWERS, ASCII_NUMERIC_CHARS};

static KEYWORDS: [&str; 7] =
    ["var", "if", "else", "then", "exit", "true", "false"];

// FIXME: 行番号、列番号がおかしい
pub struct Lexer {
    current_index: RefCell<usize>,
    current_source: String,
    current_line: Cell<usize>,
    current_column: Cell<usize>,
}

trait AssociateWithPos {
    fn with_pos(self, lexer: &Lexer) -> WithPosition<Self> where Self: Sized;
}

impl<T> AssociateWithPos for T {
    fn with_pos(self, lexer: &Lexer) -> WithPosition<Self> where Self: Sized {
        WithPosition {
            position: lexer.current_pos(),
            data: self
        }
    }
}

impl Lexer {
    pub fn create(source: &str) -> Self {
        let src = if cfg!(windows) {
            source.replace("\r\n", "\n")
        } else {
            source.to_string()
        };

        Self {
            current_source: src,
            current_index: RefCell::new(0),
            current_line: Cell::new(1),
            current_column: Cell::new(1),
        }
    }

    fn drain_space(&self) {
        while !self.reached_end() && self.current_char().expect("oops") == ' ' {
            self.consume_char().unwrap();
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn next(&self) -> WithPosition<Token> {
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile.with_pos(self)
        }

        let c = self.current_char().expect("oops");
        let t = match c {
            '\n' => {
                self.advance();
                Token::NewLine
            },
            '=' => {
                self.advance();
                if self.current_char().expect("oops") == '=' {
                    self.advance();
                    Token::PartEqEq
                } else {
                    Token::SymEq
                }
            },
            '+' => {
                self.advance();
                Token::SymPlus
            },
            '-' => {
                self.advance();
                Token::SymMinus
            },
            '*' => {
                self.advance();
                Token::SymAsterisk
            },
            '/' => {
                self.advance();
                Token::SymSlash
            },
            '(' => {
                self.advance();
                Token::SymLeftPar
            },
            ')' => {
                self.advance();
                Token::SymRightPar
            },
            '<' => {
                self.advance();
                if self.current_char().expect("oops") == '=' {
                    self.advance();
                    if self.current_char().expect("oops") == '>' {
                        self.advance();
                        Token::PartLessEqMore
                    } else {
                        Token::PartLessEq
                    }
                } else {
                    Token::SymLess
                }
            },
            '>' => {
                self.advance();
                if self.current_char().expect("oops") == '=' {
                    self.advance();
                    Token::PartMoreEq
                } else {
                    Token::SymMore
                }
            },
            '!' => {
                self.advance();
                if self.current_char().expect("oops") == '=' {
                    self.advance();
                    Token::PartBangEq
                } else {
                    Token::SymBang
                }
            },
            '"' => {
                self.scan_string_literal().expect("unable to parse string literal")
            },
            c if ASCII_NUMERIC_CHARS.contains(&c) => self.scan_digits().expect("oops"),
            c if ASCII_LOWERS.contains(&c) => {
                let scan_result = self.scan_lowers().expect("oops");
                let is_keyword = KEYWORDS.contains(&scan_result.as_str());
                if is_keyword {
                    match scan_result.as_str() {
                        "var" => Token::VarKeyword,
                        "true" => Token::KeywordTrue,
                        "false" => Token::KeywordFalse,
                        "if" => Token::KeywordIf,
                        "then" => Token::KeywordThen,
                        "else" => Token::KeywordElse,
                        other => Token::Reserved {
                            matched: other.to_string(),
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
        };
        t.with_pos(self)
    }

    fn with_position<T>(&self, data: T) -> WithPosition<T> {
        WithPosition {
            position: self.current_pos(),
            data,
        }
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.current_line.get().try_into().expect("INTERNAL ERROR - PLEASE REPORT THIS BUG"),
            column: self.current_column.get().try_into().expect("INTERNAL ERROR - PLEASE REPORT THIS BUG"),
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

    fn scan_string_literal(&self) -> Result<Token> {
        let mut buf = String::new();
        assert_eq!(self.consume_char()?, '"');
        loop {
            if self.reached_end() {
                break
            }

            let c = self.current_char()?;
            if c == '"' {
                // 終わりのダブルクォーテーションは捨てる
                self.consume_char()?;
                break
            }
            let c = self.consume_char()?;
            buf.push(c);
        }
        Ok(Token::StringLiteral(buf))
    }
    
    pub fn peek(&self) -> WithPosition<Token> {
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
        self.advance();
        Ok(c)
    }

    fn reached_end(&self) -> bool {
        *self.current_index.borrow() >= self.current_source.len()
    }

    fn advance(&self) {
        if self.current_char().unwrap() == '\n' {
            self.current_line.set(self.current_line.get() + 1);
            self.current_column.set(1);
        } else {
            self.current_column.set(self.current_column.get() + 1);
        }
        *self.current_index.borrow_mut() += 1;
    }

    fn advance_by(&self, step: usize) {
        for _ in 1..=step {
            self.advance();
        }
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
    KeywordTrue,
    KeywordFalse,
    /// `"="`
    SymEq,
    /// `"+"`
    SymPlus,
    /// `"-"`
    SymMinus,
    /// `*`
    SymAsterisk,
    /// `/`
    SymSlash,
    /// `"("`
    SymLeftPar,
    /// `")"`
    SymRightPar,
    /// `>`
    SymMore,
    /// `<`
    SymLess,
    /// `!`
    SymBang,
    /// `==`
    PartEqEq,
    /// `!=`
    PartBangEq,
    /// `<=`
    PartLessEq,
    /// `>=`
    PartMoreEq,
    /// `<=>`
    PartLessEqMore,
    /// `if`
    KeywordIf,
    /// `then`
    KeywordThen,
    /// `else`
    KeywordElse,
    /// `"`
    SymDoubleQuote,
    StringLiteral(String),
    /// reserved for future use.
    Reserved {
        matched: String,
    },

}