use std::borrow::Cow;
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use log::{debug, trace, warn};
use thiserror::Error;
use origlang_ast::{Comment, SourcePos, WithPosition};
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

#[derive(Clone, Eq, PartialEq, Debug, Hash, Default)]
struct OccurrenceSet<T: Ord>(Vec<T>);

impl OccurrenceSet<usize> {
    fn new(v: Vec<usize>) -> Option<Self> {
        if v.len() <= 1 {
            Some(Self(v))
        } else {
            if Self::invariant_was_satisfied(&v) {
                // SAFETY: we've checked precondition.
                unsafe {
                    Some(Self::new_unchecked(v))
                }
            } else {
                None
            }
        }
    }

    const fn invariant_was_satisfied(v: &[usize]) -> bool {
        if v.len() <= 1 {
            return true
        }

        Self::invariant_was_satisfied_inner(0, v)
    }

    const fn invariant_was_satisfied_inner(start: usize, p: &[usize]) -> bool {
        if start == p.len() - 2 - 1 {
            true
        } else {
            if p[start] < p[start + 1] {
                Self::invariant_was_satisfied_inner(start + 1, p)
            } else {
                false
            }
        }
    }

    unsafe fn new_unchecked(v: Vec<usize>) -> Self {
        assert!(Self::invariant_was_satisfied(&v), "invariant was violated");

        Self(v)
    }

    fn count_lowers_exclusive(&self, upper: usize) -> usize {
        let mut i = 0;
        let values: &[usize] = &self.0;
        let mut run_rest = true;
        if values.len() >= 6400 {
            // if values are too many to being cached in L1 storage,
            // switch strategy to binary_search.
            return values.binary_search(&upper).map_or_else(|x| x, |x| x);
        } else if values.len() >= 8 {
            while i < values.len() - 8 {
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v1 = unsafe { *values.get_unchecked(i) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v2 = unsafe { *values.get_unchecked(i + 1) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v3 = unsafe { *values.get_unchecked(i + 2) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v4 = unsafe { *values.get_unchecked(i + 3) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v5 = unsafe { *values.get_unchecked(i + 4) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v6 = unsafe { *values.get_unchecked(i + 5) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v7 = unsafe { *values.get_unchecked(i + 6) };
                // SAFETY: above condition ensures that no OOB-reads happen.
                let v8 = unsafe { *values.get_unchecked(i + 7) };


                if v8 < upper {
                    // let CPU to guess what is going on, manual _mm_prefetch is inefficient
                    i += 8;
                } else {
                    // v8 >= upper
                    // partition point must be in v1..v8
                    if v8 < upper {
                        i += 8;
                    } else if v7 < upper {
                        i += 7;
                    } else if v6 < upper {
                        i += 6;
                    } else if v5 < upper {
                        i += 5;
                    } else if v4 < upper {
                        i += 4;
                    } else if v3 < upper {
                        i += 3;
                    } else if v2 < upper {
                        i += 2;
                    } else if v1 < upper {
                        i += 1;
                    }

                    run_rest = false;
                    break
                }
            }
        }

        if run_rest {
            let j = i;
            for x in &values[j..] {
                if *x < upper {
                    i += 1;
                }
            }
        }

        i
    }

    fn max_upper_bounded_exclusive(&self, upper: usize) -> Option<usize> {
        let values: &[usize] = &self.0;

        let k = self.count_lowers_exclusive(upper);
        if k == 0 {
            None
        } else {
            Some(*values.get(k - 1).expect("!"))
        }
    }
}

type SortedSet<T> = OccurrenceSet<T>;

// FIXME: 行番号、列番号がおかしい
#[derive(Debug)]
pub struct Lexer {
    current_index: Cell<usize>,
    current_source: String,
    current_line: Cell<NonZeroUsize>,
    current_column: Cell<NonZeroUsize>,
    newline_codepoint_nth_index: SortedSet<usize>,
    char_cache: Vec<char>,
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
        let src: Cow<'_, str> = if cfg!(windows) {
            source.replace("\r\n", "\n").into()
        } else {
            Cow::Borrowed(source)
        };

        let newline_codepoint_nth_index = src.char_indices()
            .filter(|(_, x)| *x == '\n').map(|(i, _)| i)
            .collect::<Vec<_>>();

        // SAFETY: inner value has sorted, because:
        //     char_indices yields sorted index.
        let newline_codepoint_nth_index = unsafe {
            OccurrenceSet::new_unchecked(newline_codepoint_nth_index)
        };

        Self {
            newline_codepoint_nth_index,
            char_cache: src.chars().collect(),
            current_source: src.to_string(),
            current_index: Cell::new(0),
            current_line: Cell::new(
                // SAFETY: 1 != 0
                unsafe { NonZeroUsize::new_unchecked(1) }
            ),
            current_column: Cell::new(
                // SAFETY: 1 != 0
                unsafe { NonZeroUsize::new_unchecked(1) }
            ),
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
            .or_else(||
                fold!(
                    self.try_char('/').expect("huh?"),
                    fold!(
                        self.try_char('/').expect("huh?"),
                        Some(self.scan_line_comment().expect("unable to parse comment")),
                        Some(Token::SymSlash)
                    ),
                    None
                )
            )
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
            line: self.current_line.get(),
            column: self.current_column.get(),
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
        let head = self.current_index.get();
        let preallocate_len = self.char_cache[head..].iter().enumerate().find(|(_, c)| **c == '"').map(|x| x.0);
        let mut buf = if let Some(pre_alloc) = preallocate_len {
            let mut buf = String::with_capacity(preallocate_len.unwrap_or(65535));
            let next = head + pre_alloc;
            {
                let mut s = String::with_capacity(pre_alloc);
                for c in &self.char_cache[head..next] {
                    s.push(*c)
                }
                buf.push_str(&s);
            }
            self.current_index.set(next);
            buf
        } else {
            String::new()
        };
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

    #[inline(never)]
    fn set_current_index(&self, future_index: usize) -> Result<(), LineComputationError> {
        // trace!("set index to: {future_index}");
        let SourcePos { line, column } =
            LineComputation::compute(future_index + 1, &self.newline_codepoint_nth_index)?;

        // trace!("compute: {line}:{column}");
        self.current_index.set(future_index);
        self.current_line.set(line);
        self.current_column.set(column);

        Ok(())
    }

    fn scan_line_comment(&self) -> Result<Token, LexerError> {
        let content = self.scan_by_predicate(|c| c != '\n', false)?;

        Ok(Token::Comment {
            content: Comment {
                content,
            },
        })
    }

    /// Get n-step away token without consume it.
    pub fn peek_n(&self, advance_step: usize) -> WithPosition<Token> {
        debug!("peek_n:{advance_step}");
        let to_rollback = self.current_index.get();
        if advance_step == 0 {
            let token = self.next();
            self.set_current_index(to_rollback).map_err(|e| {
                warn!("discarding error: {e}");
            }).unwrap_or_default();
            token
        } else {
            let mut token: Option<WithPosition<Token>> = None;
            for _ in 1..=advance_step {
                token = Some(self.next());
            }
            self.set_current_index(to_rollback).map_err(|e| {
                warn!("discarding error: {e}");
            }).unwrap_or_default();
            // SAFETY: we already initialize it.
            unsafe { token.unwrap_unchecked() }
        }
    }

    /// Get current token without consume it.
    pub fn peek(&self) -> WithPosition<Token> {
        self.peek_n(1)
    }

    fn current_char(&self) -> Result<char, LexerError> {
        self.char_cache
            .get(self.current_index.get())
            .ok_or_else(||
                LexerError::OutOfRange {
                    current: self.current_index.get(),
                    max: self.current_source.len(),
                }
            )
            .map(|x| *x)
    }

    pub(crate) fn consume_char(&self) -> Result<char, LexerError> {
        let c = self.current_char()?;
        // trace!("consume: `{c}` (\\U{{{k:06X}}})", k = c as u32);
        self.advance();
        Ok(c)
    }

    fn reached_end(&self) -> bool {
        self.current_index.get() >= self.current_source.len()
    }

    fn advance(&self) {
        trace!("lexer:advance");
        self.set_current_index(self.current_index.get() + 1).map_err(|e| {
            warn!("discarding error: {e}");
        }).unwrap_or_default();
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

struct LineComputation;

impl LineComputation {
    #[inline(never)]
    fn compute(future_index: usize, new_line_occurrences: &SortedSet<usize>) -> Result<SourcePos, LineComputationError> {
        /*
        // This may be an error, however this snippet leads to infinite loop.
        if new_line_occurrences.contains(&future_index) {
            return Err(LineComputationError::PointedOnNewLine)
        }
        */

        let future_line = new_line_occurrences.count_lowers_exclusive(future_index) + 1;

        let most_recent_new_line_occurrence_codepoint: usize = new_line_occurrences
            .max_upper_bounded_exclusive(future_index)
            // if future_index is still on first line, there's no such occurrence - substitute
            // this value with zero to leave future_index as is.
            .unwrap_or(0);

        assert!(future_index >= most_recent_new_line_occurrence_codepoint, "{future_index} >= {most_recent_new_line_occurrence_codepoint}");
        let future_line_column = future_index - most_recent_new_line_occurrence_codepoint;

        Ok(SourcePos {
            line: future_line.try_into().map_err(|_| LineComputationError::LineIsZero)?,
            column: future_line_column.try_into().map_err(|_| LineComputationError::ColumnIsZero)?,
        })
    }
}

#[derive(Error, Debug, Eq, PartialEq, Copy, Clone)]
enum LineComputationError {
    #[error("the index pointed on newline")]
    PointedOnNewLine,
    #[error("line number is zero")]
    LineIsZero,
    #[error("column number is zero")]
    ColumnIsZero,
}

#[cfg(test)]
mod tests {
    use origlang_ast::SourcePos;
    use crate::lexer::{LineComputation, LineComputationError, SortedSet, OccurrenceSet};

    #[test]
    fn no_newline() {
        assert_eq!(
            LineComputation::compute(12, &SortedSet::default()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 12.try_into().unwrap(),
            })
        );
    }

    #[test]
    fn single_newline_pre() {
        assert_eq!(
            LineComputation::compute(1, &OccurrenceSet::new(vec![100]).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 1.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_pre_99() {
        assert_eq!(
            LineComputation::compute(99, &OccurrenceSet::new(vec![100]).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 99.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_post() {
        assert_eq!(
            LineComputation::compute(101, &OccurrenceSet::new(vec![100]).unwrap()),
            Ok(SourcePos {
                line: 2.try_into().unwrap(),
                column: 1.try_into().unwrap(),
            })
        )
    }

    #[test]
    fn single_newline_point_is_error() {
        assert_eq!(
            LineComputation::compute(100, &OccurrenceSet::new(vec![100]).unwrap()),
            Ok(SourcePos {
                line: 1.try_into().unwrap(),
                column: 100.try_into().unwrap(),
            })
        )
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
    /// `//`
    PartSlashSlash,
    Comment {
        content: Comment,
    },
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
            Self::PartSlashSlash => "part:slash_slash",
            Self::Comment { .. } => "comment",
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
