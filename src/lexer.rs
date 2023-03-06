use std::cell::Cell;
use std::fmt::{Display, Formatter};
use log::{debug, trace};
use thiserror::Error;

use crate::ast::{SourcePos, WithPosition};
use crate::char_list::{ASCII_LOWERS, ASCII_NUMERIC_CHARS};

static KEYWORDS: [&str; 10] =
    ["var", "if", "else", "then", "exit", "true", "false", "print", "block", "end"];

#[derive(Error, Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum LexerError {
    #[error("Invalid suffix for integer literal. Supported suffixes are [`i8`, `i16`, `i32`, `i64`]")]
    InvalidSuffix,
    #[error("Internal compiler error: lexer index overflow: {current} > {max}")]
    OutOfRange {
        current: usize,
        max: usize,
    }
}

// FIXME: 行番号、列番号がおかしい
#[derive(Debug)]
pub struct Lexer {
    current_index: Cell<usize>,
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
            current_index: Cell::new(0),
            current_line: Cell::new(1),
            current_column: Cell::new(1),
        }
    }

    fn drain_space(&self) {
        while !self.reached_end() && self.current_char().expect("drain_space") == ' ' {
            self.consume_char().unwrap();
        }
    }

    fn try_char(&self, t: char) -> Result<Option<char>, LexerError> {
        trace!("lexer:try:{t}");
        if !self.reached_end() && self.current_char()? == t {
            self.consume_char()?;
            Ok(Some(t))
        } else {
            Ok(None)
        }
    }

    fn try_char_peek(&self, t: char) -> Result<Option<char>, LexerError> {
        trace!("lexer:try:{t}");
        if !self.reached_end() && self.current_char()? == t {
            Ok(Some(t))
        } else {
            Ok(None)
        }
    }

    fn try_any(&self, t: &[char]) -> Result<Option<char>, LexerError> {
        for c in t {
            if let Some(x) = self.try_char_peek(*c)? {
                return Ok(Some(x))
            }
        }

        Ok(None)
    }

    #[allow(clippy::too_many_lines, clippy::unnecessary_wraps)]
    fn next_inner(&self) -> Result<Token, LexerError> {
        macro_rules! fold {
            ($e:expr, $t:expr, $f:expr) => {
                if $e.is_some() {
                    $t
                } else {
                    $f
                }
            };
        }
        let v =
            if self.reached_end() {
                Some(Token::EndOfFile)
            } else {
                None
            }
            .or_else(|| self.try_char('\n').expect("huh?").map(|_| Token::NewLine))
            .or_else(||
                fold!(
                    self.try_char('=').expect("huh?"),
                    {
                        let double_eq = self.try_char('=').expect("huh?");
                        if double_eq.is_some() {
                            Some(Token::PartEqEq)
                        } else {
                            Some(Token::SymEq)
                        }
                    },
                    None
                )
            )
            .or_else(|| self.try_char('+').expect("huh?").map(|_| Token::SymPlus))
            .or_else(|| self.try_char('-').expect("huh?").map(|_| Token::SymMinus))
            .or_else(|| self.try_char('*').expect("huh?").map(|_| Token::SymAsterisk))
            .or_else(|| self.try_char('/').expect("huh?").map(|_| Token::SymSlash))
            .or_else(|| self.try_char('(').expect("huh?").map(|_| Token::SymLeftPar))
            .or_else(|| self.try_char(')').expect("huh?").map(|_| Token::SymRightPar))
            .or_else(|| 
                fold!(
                    self.try_char('<').expect("huh?").map(|_| Token::SymPlus),
                    fold!(
                        self.try_char('=').expect("huh?"),
                        fold!(
                            self.try_char('>').expect("huh?"),
                            Some(Token::PartLessEqMore),
                            Some(Token::PartLessEq)
                        ),
                        Some(Token::SymLess)
                    ),
                    None
                )
            )
            .or_else(|| 
                fold!(
                    self.try_char('>').expect("huh?"),
                    fold!(
                        self.try_char('=').expect("huh?"),
                        Some(Token::PartMoreEq),
                        Some(Token::SymMore)
                    ),
                    None
                )
            )
            .or_else(|| 
                fold!(
                    self.try_char('!').expect("huh?"),
                    fold!(
                        self.try_char('=').expect("huh?"),
                        Some(Token::PartBangEq),
                        Some(Token::SymBang)
                    ),
                    None
                )
            )
            .or_else(|| 
                fold!(
                    self.try_char('"').expect("huh?"),
                    Some(self.scan_string_literal().expect("unable to parse string literal")),
                    None
                )
            )
            .or_else(|| 
                fold!(
                    self.try_any(&ASCII_NUMERIC_CHARS).expect("huh?"),
                    Some(self.scan_digits().expect("huh?")),
                    None
                )
            )
            .or_else(||
                fold!(
                    self.try_char(',').expect("huh?"),
                    Some(Token::SymComma),
                    None
                )
            )
            .or_else(|| {
                fold!(
                    self.try_any(&ASCII_LOWERS).expect("huh?"),
                    {
                        let v = {
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
                                    "print" => Token::KeywordPrint,
                                    "block" => Token::KeywordBlock,
                                    "end" => Token::KeywordEnd,
                                    other => Token::Reserved {
                                        matched: other.to_string(),
                                    }
                                }
                            } else {
                                Token::Identifier { inner: scan_result }
                            }
                        };

                        Some(v)
                    },
                    None
                )
            })
            // dont eager evaluate
            .unwrap_or_else(|| Token::UnexpectedChar {
                index: self.current_index.get(),
                char: self.current_char().expect("unexpected_char"),
            });
        Ok(v)
    }

    pub fn next(&self) -> WithPosition<Token> {
        debug!("lexer:next");
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile.with_pos(self)
        }

        self.next_inner()
            .expect("Lexer phase error")
            .with_pos(self)
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.current_line.get().try_into().expect("INTERNAL ERROR - PLEASE REPORT THIS BUG"),
            column: self.current_column.get().try_into().expect("INTERNAL ERROR - PLEASE REPORT THIS BUG"),
        }
    }

    fn scan_by_predicate(&self, scan_while: impl Fn(char) -> bool, drop_on_exit: bool) -> Result<String, LexerError> {
        let mut buf = String::new();
        loop {
            if self.reached_end() {
                break
            }

            let c = self.current_char()?;
            if !scan_while(c) {
                if drop_on_exit {
                    self.consume_char()?;
                }

                break
            }
            let c = self.consume_char()?;

            buf.push(c);
        }

        Ok(buf)
    }

    fn scan_digit_suffix_opt(&self) -> Result<Option<Box<str>>, LexerError> {
        let v = if self.current_char()? == 'i' {
            self.consume_char()?;
            if self.current_char()? == '8' {
                self.consume_char()?;
                Some("i8".to_string().into_boxed_str())
            } else if self.current_char()? == '1' {
                self.consume_char()?;
                if self.current_char()? == '6' {
                    self.consume_char()?;
                    Some("i16".to_string().into_boxed_str())
                } else {
                    return Err(LexerError::InvalidSuffix);
                }
            } else if self.current_char()? == '3' {
                self.consume_char()?;
                if self.current_char()? == '2' {
                    self.consume_char()?;
                    Some("i32".to_string().into_boxed_str())
                } else {
                    return Err(LexerError::InvalidSuffix);
                }
            } else if self.current_char()? == '6' {
                self.consume_char()?;
                if self.current_char()? == '4' {
                    self.consume_char()?;
                    Some("i64".to_string().into_boxed_str())
                } else {
                    return Err(LexerError::InvalidSuffix);
                }
            } else {
                return Err(LexerError::InvalidSuffix);
            }
        } else {
            None
        };

        Ok(v)
    }

    fn scan_digits(&self) -> Result<Token, LexerError> {
        debug!("lexer:digit");
        let buf = self.scan_by_predicate(|c| ASCII_NUMERIC_CHARS.contains(&c), false)?;
        let builtin_suffix = self.scan_digit_suffix_opt()?;

        Ok(Token::Digits {
            sequence: buf,
            suffix: builtin_suffix,
        })
    }

    fn scan_lowers(&self) -> Result<String, LexerError> {
        debug!("lexer:lower");
        let buf = self.scan_by_predicate(|c| ASCII_LOWERS.contains(&c), false)?;
        Ok(buf)
    }

    fn scan_string_literal(&self) -> Result<Token, LexerError> {
        debug!("lexer:lit:string");
        let mut buf = String::new();
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

    fn set_current_index(&self, new_index: usize) {
        trace!("set index to: {new_index}");
        self.current_index.set(new_index);
        let future_line = self.current_source.chars().take(new_index).filter(|c| *c == '\n').count() + 1;
        let future_line_start_index: usize = self
            .current_source
            .chars()
            .enumerate()
            .filter(|(_, c)| *c == '\n')
            .filter(|(i, _)| *i < new_index)
            .map(|(i, _)| i)
            .max()
            .unwrap_or(0);

        let future_line_column = self.current_index.get() - future_line_start_index;
        self.current_line.set(future_line);
        self.current_column.set(future_line_column);
    }

    /// Get n-step away token without consume it.
    pub fn peek_n(&self, advance_step: usize) -> WithPosition<Token> {
        debug!("peek_n:{advance_step}");
        let to_rollback = self.current_index.get();
        if advance_step == 0 {
            let token = self.next();
            self.set_current_index(to_rollback);
            token
        } else {
            let mut token: Option<WithPosition<Token>> = None;
            for _ in 1..=advance_step {
                token = Some(self.next());
            }
            self.set_current_index(to_rollback);
            // SAFETY: we already initialize it.
            unsafe { token.unwrap_unchecked() }
        }
    }

    /// Get current token without consume it.
    pub fn peek(&self) -> WithPosition<Token> {
        self.peek_n(1)
    }

    fn current_char(&self) -> Result<char, LexerError> {
        self.current_source
            .as_str()
            .chars()
            .nth(self.current_index.get())
            .ok_or_else(||
                LexerError::OutOfRange {
                    current: self.current_index.get(),
                    max: self.current_source.len(),
                }
            )
    }

    fn consume_char(&self) -> Result<char, LexerError> {
        let c = self.current_char()?;
        trace!("consume: `{c}` (\\U{{{k:06X}}})", k = c as u32);
        self.advance();
        Ok(c)
    }

    fn reached_end(&self) -> bool {
        self.current_index.get() >= self.current_source.len()
    }

    fn advance(&self) {
        trace!("lexer:advance");
        self.set_current_index(self.current_index.get() + 1);
    }

    /// パースに失敗するかも知れないものをパースしようと試みる。
    /// 成功したならパースした値
    /// 失敗したならNoneを返しつつ内部インデックスをこの関数を呼び出したときの値に戻す:
    ///   これによってコパーサがどれだけ壊れていたとしても失敗時にもとのインデックスに戻ることが保証される
    pub fn parse_fallible<T, E>(&self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        let t = self.create_reset_token();
        match f() {
            Ok(t) => Ok(t),
            Err(e) => {
                t.reset(self);
                Err(e)
            }
        }
    }

    fn create_reset_token(&self) -> TemporalLexerUnwindToken {
        TemporalLexerUnwindToken {
            unwind_index: self.current_index.get()
        }
    }
}

struct TemporalLexerUnwindToken {
    unwind_index: usize,
}

impl TemporalLexerUnwindToken {
    fn reset(self, lexer: &Lexer) {
        lexer.current_index.set(self.unwind_index);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Identifier {
        inner: String,
    },
    Digits {
        sequence: String,
        suffix: Option<Box<str>>
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
    /// `print $expr`
    KeywordPrint,
    KeywordBlock,
    KeywordEnd,
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
    /// `,`
    SymComma,
    StringLiteral(String),
    /// reserved for future use.
    Reserved {
        matched: String,
    },

}

impl Token {
    pub const fn display(&self) -> DisplayToken {
        DisplayToken(self.kind0())
    }

    const fn kind0(&self) -> &'static str {
        match self {
            Self::Identifier { .. } => "identifier",
            Self::Digits { .. } => "literal:int",
            Self::UnexpectedChar { .. } => "unexpected_char",
            Self::EndOfFile => "EOF",
            Self::NewLine => "new_line",
            Self::VarKeyword => "keyword:var",
            Self::KeywordTrue => "keyword:true",
            Self::KeywordFalse => "keyword:false",
            Self::KeywordPrint => "keyword:print",
            Self::KeywordBlock => "keyword:block",
            Self::KeywordEnd => "keyword:end",
            Self::SymEq => "sym:eq",
            Self::SymPlus => "sym:plus",
            Self::SymMinus => "sym:minus",
            Self::SymAsterisk => "sym:asterisk",
            Self::SymSlash => "sym:slash",
            Self::SymLeftPar => "sym:left_par",
            Self::SymRightPar => "sym:right_par",
            Self::SymMore => "sym:more",
            Self::SymLess => "sym:less",
            Self::SymBang => "sym:bang",
            Self::PartEqEq => "part:eq_eq",
            Self::PartBangEq => "part:bang_eq",
            Self::PartLessEq => "part:less_eq",
            Self::PartMoreEq => "part:more_eq",
            Self::PartLessEqMore => "part:less_eq_more",
            Self::KeywordIf => "keyword:if",
            Self::KeywordThen => "keyword:then",
            Self::KeywordElse => "keyword:else",
            Self::SymDoubleQuote => "sym:double_quote",
            Self::SymComma => "sym:comma",
            Self::StringLiteral(_) => "literal:string",
            Self::Reserved { .. } => "reserved_token",
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct DisplayToken(&'static str);

impl Display for DisplayToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}
