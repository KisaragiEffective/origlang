pub(crate) mod error;
#[cfg(test)]
mod tests;
pub mod token;

use std::borrow::Cow;
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::ControlFlow;
use std::panic::RefUnwindSafe;
use log::{debug, trace, warn};
use self::error::LexerError;
use origlang_ast::{Comment, Identifier};
use origlang_source_span::{SourcePosition as SourcePos, Pointed as WithPosition};
use crate::char_list::ASCII_NUMERIC_CHARS;
use crate::chars::boundary::{Utf8CharBoundaryStartByte, Utf8CharStride};
use crate::chars::line::{LineComputation, LineComputationError};
use crate::chars::occurrence::OccurrenceSet;
use crate::lexer::token::{TemporalLexerUnwindToken, Token};

static KEYWORDS: [&str; 10] =
    ["var", "if", "else", "then", "exit", "true", "false", "print", "block", "end"];

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

// FIXME: 行番号、列番号がおかしい
#[derive(Debug)]
pub struct Lexer {
    source_bytes_nth: Cell<Utf8CharBoundaryStartByte>,
    source: String,
    current_line: Cell<NonZeroUsize>,
    current_column: Cell<NonZeroUsize>,
    newline_codepoint_nth_index: OccurrenceSet<Utf8CharBoundaryStartByte>,
}

impl Lexer {
    #[must_use = "Lexer do nothing unless calling parsing function"]
    pub fn create(source: &str) -> Self {
        let src: Cow<'_, str> = if cfg!(windows) {
            source.replace("\r\n", "\n").into()
        } else {
            Cow::Borrowed(source)
        };

        let newline_codepoint_nth_index = src.bytes().enumerate()
            .filter(|(_, x)| *x == b'\n')
            .map(|(i, _)| Utf8CharBoundaryStartByte::new(i))
            // we can't use try_collect because it requires nightly compiler.
            // we also can't have FromIterator<T> for OccurrenceSet<T> where T: Ord because doing so may
            // break invariant of OccurrenceSet (i.e. the underlying iterator was not sorted.)
            .collect::<Vec<_>>();

        // SAFETY: inner value has sorted, because:
        //     char_indices yields sorted index.
        let newline_codepoint_nth_index = unsafe {
            OccurrenceSet::new_unchecked(newline_codepoint_nth_index)
        };

        Self {
            source_bytes_nth: Cell::new(Utf8CharBoundaryStartByte::new(0)),
            current_line: Cell::new(
                // SAFETY: 1 != 0
                unsafe { NonZeroUsize::new_unchecked(1) }
            ),
            current_column: Cell::new(
                // SAFETY: 1 != 0
                unsafe { NonZeroUsize::new_unchecked(1) }
            ),
            source: src.to_string(),
            newline_codepoint_nth_index
        }
    }

    fn drain_space(&self) {
        while !self.reached_end() && self.current_char().expect("drain_space") == ' ' {
            self.consume_char().unwrap();
        }
    }

    fn try_char(&self, t: char) -> Result<Option<char>, LexerError> {
        trace!("lexer:try:{t:?}");
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
            .or_else(||
                fold!(
                    self.try_char(':').expect("huh?"),
                    Some(Token::SymColon),
                    None
                )
            )
            .or_else(|| {
                self.one_or_many_accumulator(
                    String::new(),
                    (true, false),
                    |x, (first, exit_on_next_iteration)| {
                        if exit_on_next_iteration {
                            return ControlFlow::Break(())
                        }

                        let discarding = x == '_' && first;
                        let is_identifier = x.is_ascii_alphabetic() || (!first && x.is_ascii_digit());

                        if is_identifier {
                            ControlFlow::Continue((false, false))
                        } else if discarding {
                            ControlFlow::Continue((false, true))
                        } else {
                            ControlFlow::Break(())
                        }
                    },
                    |x, identifier| {
                        identifier.push(x);
                        debug!("identifier: {identifier:?}");
                    }
                )
                    .ok()
                    .map(|scanned| {
                        let is_keyword = KEYWORDS.contains(&scanned.as_str());
                        if is_keyword {
                            match scanned.as_str() {
                                "var" => Token::VarKeyword,
                                "true" => Token::KeywordTrue,
                                "false" => Token::KeywordFalse,
                                "if" => Token::KeywordIf,
                                "then" => Token::KeywordThen,
                                "else" => Token::KeywordElse,
                                "print" => Token::KeywordPrint,
                                "block" => Token::KeywordBlock,
                                "end" => Token::KeywordEnd,
                                "exit" => Token::KeywordExit,
                                other => Token::Reserved {
                                    matched: other.to_string(),
                                }
                            }
                        } else {
                            Token::Identifier { inner: Identifier::new(scanned) }
                        }
                    })
            })
            // dont eager evaluate
            .unwrap_or_else(|| Token::UnexpectedChar {
                // TODO: this is cold path, so may convert boundary to char_nth.
                index: self.source_bytes_nth.get(),
                char: self.current_char().expect("unexpected_char"),
            });
        Ok(v)
    }

    pub fn next(&self) -> WithPosition<Token> {
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile.with_pos(self)
        }

        let r = self.next_inner()
            .expect("Lexer phase error")
            .with_pos(self);

        debug!("next: {r:?}", r = &r);
        r
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.current_line.get(),
            column: self.current_column.get(),
        }
    }

    fn one_or_many(&self, scan_while: impl Fn(char) -> bool, ignore_trailing_char_on_exit: bool) -> Result<String, LexerError> {
        let mut buf = String::new();
        loop {
            if self.reached_end() {
                break
            }

            let c = self.current_char()?;
            if !scan_while(c) {
                if ignore_trailing_char_on_exit {
                    self.consume_char()?;
                }

                break
            }
            let c = self.consume_char()?;

            buf.push(c);
        }

        Ok(buf)
    }

    fn one_or_many_accumulator<Acc, R: RefUnwindSafe>(
        &self,
        scan_sequence_accumulator: Acc,
        registers: R,
        judge: impl Fn(char, R) -> ControlFlow<(), R>,
        accumulate_before_next_iteration_after_break: impl Fn(char, &mut Acc)
    ) -> Result<Acc, LexerError> {
        let mut acc = scan_sequence_accumulator;
        let mut registers = registers;

        loop {
            if self.reached_end() {
                break
            }

            let c = self.current_char()?;
            let cf = judge(c, registers);
            match cf {
                ControlFlow::Continue(c) => {
                    registers = c;
                    self.consume_char()?;
                }
                ControlFlow::Break(b) => {
                    break
                }
            }

            accumulate_before_next_iteration_after_break(c, &mut acc);
        }

        Ok(acc)
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
        let buf = self.one_or_many(|c| ASCII_NUMERIC_CHARS.contains(&c), false)?;
        let builtin_suffix = self.scan_digit_suffix_opt()?;

        Ok(Token::Digits {
            sequence: buf,
            suffix: builtin_suffix,
        })
    }

    fn scan_string_literal(&self) -> Result<Token, LexerError> {
        fn calc_skip_byte_in_utf8(start: Utf8CharBoundaryStartByte, source: &str) -> Option<Utf8CharBoundaryStartByte> {
            // well, at least, this code accesses memory to sequential order.
            const BATCH_SIZE: usize = 32;
            let sub_slice = &source.as_bytes()[start.as_usize()..];
            for step in 0..(sub_slice.len() / BATCH_SIZE) {
                let offset = step * BATCH_SIZE;
                let chunk = &sub_slice[offset..(offset + BATCH_SIZE)];
                for (sub_offset, b) in chunk.iter().enumerate() {
                    if *b == b'"' {
                        return Some(Utf8CharBoundaryStartByte::new(offset + sub_offset))
                    }
                }
            }

            let last_offset = sub_slice.len() / BATCH_SIZE * BATCH_SIZE;
            let last_byte = sub_slice.len();

            #[allow(clippy::needless_range_loop)]
            for offset in last_offset..last_byte {
                if sub_slice[offset] == b'"' {
                    return Some(Utf8CharBoundaryStartByte::new(offset));
                }
            }

            None
        }
        debug!("lexer:lit:string");

        // this search is exact at this point.
        // However, once we introduce escape sequence or another delimiter for string literal,
        // this code is likely to needed to be rewritten.

        let Some(skip_byte_in_utf8) = calc_skip_byte_in_utf8(self.source_bytes_nth.get(), &self.source) else {
            return Err(LexerError::UnclosedStringLiteral)
        };

        let mut string_char_literal_content = {
            // the starting quote is handled in `next_inner`, so this boundary is either first
            // char in the literal, or ending quote.
            let maybe_first_char_boundary = self.source_bytes_nth.get();
            let quote_end_boundary = Utf8CharBoundaryStartByte::new(maybe_first_char_boundary.as_usize() + skip_byte_in_utf8.as_usize());

            // assert!(found_boundary_nth >= current_chars_nth, "{found_boundary_nth:?} >= {current_chars_nth:?}");

            let s = &self.source[(maybe_first_char_boundary.as_usize())..(quote_end_boundary.as_usize())];
            self.source_bytes_nth.set(quote_end_boundary);
            s.to_string()
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
            string_char_literal_content.push(c);
        }
        Ok(Token::StringLiteral(string_char_literal_content))
    }

    #[inline(never)]
    fn set_current_index(&self, future_index: Utf8CharBoundaryStartByte) -> Result<(), LineComputationError> {
        if future_index == self.source_bytes_nth.get() {
            // no computation is needed
            Ok(())
        } else {
            let b = self.source_bytes_nth.get().stride(Utf8CharStride::One);
            if future_index == b && self.current_char_stride() == Ok(Utf8CharStride::One) {
                return if let Ok(c) = self.current_char() {
                    self.source_bytes_nth.set(b);
                    if c == '\n' {
                        // new line, setting $(L + 1):C.
                        self.current_line.set(NonZeroUsize::new(self.current_line.get().get() + 1).expect("we do not support this"));
                        // SAFETY: 1 != 0
                        self.current_column.set(unsafe { NonZeroUsize::new_unchecked(1) });
                    } else {
                        // not new line, setting L:$(C + 1).
                        self.current_column.set(NonZeroUsize::new(self.current_column.get().get() + 1).expect("we do not support this"));
                    }
                    Ok(())
                } else {
                    // ?
                    Err(LineComputationError::OutOfRange)
                }
            } else {
                // trace!("set index to: {future_index}");
                let SourcePos { line, column } =
                    LineComputation::compute(
                        future_index.stride(Utf8CharStride::from('\n')),
                        &self.newline_codepoint_nth_index
                    )?;

                trace!("compute: {line}:{column}");
                self.source_bytes_nth.set(future_index);
                self.current_line.set(line);
                self.current_column.set(column);

                Ok(())
                // full computation
            }
        }
    }

    fn scan_line_comment(&self) -> Result<Token, LexerError> {
        let content = self.one_or_many(|c| c != '\n', false)?;

        Ok(Token::Comment {
            content: Comment {
                content,
            },
        })
    }

    /// Get n-step away token without consume it.
    pub fn peek_n(&self, advance_step: usize) -> WithPosition<Token> {
        debug!("peek_n:{advance_step}");
        let to_rollback = self.source_bytes_nth.get();
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

    fn current_char_stride(&self) -> Result<Utf8CharStride, LexerError> {
        let current_boundary = self.source_bytes_nth.get();
        let index = current_boundary.as_usize();
        let heading_byte = self.source.as_bytes()[index];

        let stride = if heading_byte <= 0x7F {
            Utf8CharStride::One
        } else if heading_byte & 0b1110_0000 == 0b1110_0000 {
            Utf8CharStride::Four
        } else if heading_byte & 0b1100_0000 == 0b1100_0000 {
            Utf8CharStride::Three
        } else if heading_byte & 0b1000_0000 == 0b1000_0000 {
            Utf8CharStride::Four
        } else {
            return Err(LexerError::MalformedAsUtf8 {
                boundary: current_boundary,
            })
        };

        Ok(stride)
    }

    fn current_char(&self) -> Result<char, LexerError> {
        let current_boundary = self.source_bytes_nth.get();
        let index = current_boundary.as_usize();
        let stride = self.current_char_stride()?;


        let s = unsafe { self.source.get_unchecked(index..(index + stride.as_usize())) };

        let c = s.chars().next().ok_or(LexerError::OutOfRange {
            current: current_boundary,
            // bytes in UTF-8
            max: self.source.len(),
        })?;


        Ok(c)
    }

    pub(crate) fn consume_char(&self) -> Result<char, LexerError> {
        let c = self.current_char()?;
        // trace!("consume: `{c}` (\\U{{{k:06X}}})", k = c as u32);
        self.advance();
        Ok(c)
    }

    fn reached_end(&self) -> bool {
        // <&str>::len() yields length of BYTES, not CHARS
        self.source_bytes_nth.get().as_usize() >= self.source.len()
    }

    fn advance(&self) {
        trace!("lexer:advance");
        let new = self.source_bytes_nth.get().stride(self.current_char_stride().unwrap());
        self.set_current_index(new).map_err(|e| {
            warn!("discarding error: {e}");
        }).unwrap_or_default();
    }

    /// パースに失敗するかも知れないものをパースしようと試みる。
    /// 成功したならパースした値
    /// 失敗したならNoneを返しつつ内部インデックスをこの関数を呼び出したときの値に戻す:
    ///   これによってコパーサがどれだけ壊れていたとしても失敗時にもとのインデックスに戻ることが保証される
    /// # Errors
    /// もしfがErrを返したとき、パーサーの位置を戻し、その後fの値を伝播する。
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

    #[must_use = "Dropping token do nothing"]
    fn create_reset_token(&self) -> TemporalLexerUnwindToken {
        TemporalLexerUnwindToken::new(self.source_bytes_nth.get())
    }
}
