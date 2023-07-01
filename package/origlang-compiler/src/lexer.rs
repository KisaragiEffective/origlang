use std::borrow::Cow;
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::num::NonZeroUsize;
use std::ops::ControlFlow;
use std::panic::RefUnwindSafe;
use log::{debug, trace, warn};
use thiserror::Error;
use origlang_ast::{Comment, Identifier};
use origlang_source_span::{SourcePosition as SourcePos, Pointed as WithPosition};
use crate::char_list::ASCII_NUMERIC_CHARS;
use crate::chars::boundary::{Utf8CharBoundaryStartByte, Utf8CharStride};
use crate::chars::line::{LineComputation, LineComputationError};
use crate::chars::occurrence::OccurrenceSet;

static KEYWORDS: [&str; 10] =
    ["var", "if", "else", "then", "exit", "true", "false", "print", "block", "end"];

#[derive(Error, Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum LexerError {
    #[error("Invalid suffix for integer literal. Supported suffixes are [`i8`, `i16`, `i32`, `i64`]")]
    InvalidSuffix,
    #[error("Internal compiler error: lexer index overflow: {current:?} > {max}")]
    OutOfRange {
        current: Utf8CharBoundaryStartByte,
        max: usize,
    },
    #[error("Unclosed string literal was found")]
    UnclosedStringLiteral,
    #[error("Input is malformed UTF-8")]
    MalformedAsUtf8 {
        boundary: Utf8CharBoundaryStartByte,
    },
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
                    true,
                    |x, first| {
                        let b = x.is_ascii_alphabetic() || (!first && x.is_ascii_digit());

                        if b {
                            ControlFlow::Continue(false)
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
        accumulate: impl Fn(char, &mut Acc)
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

            accumulate(c, &mut acc);
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
        } else if future_index == self.source_bytes_nth.get().stride(Utf8CharStride::One) && self.current_char() != Ok('\n') && self.current_char_stride() == Ok(Utf8CharStride::One) {
            // advance to the next, and current is in ASCII and is not a LF;
            // in this case, call to LineComputation is not needed.
            self.source_bytes_nth.set(self.source_bytes_nth.get().stride(Utf8CharStride::One));
            self.current_column.set(NonZeroUsize::new(self.current_column.get().get() + 1).expect("we do not support this"));
            Ok(())
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

        let c = self.source[index..(index + stride.as_usize())].chars().next().ok_or(LexerError::OutOfRange {
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
        TemporalLexerUnwindToken {
            unwind_index: self.source_bytes_nth.get()
        }
    }
}

struct TemporalLexerUnwindToken {
    unwind_index: Utf8CharBoundaryStartByte,
}

impl TemporalLexerUnwindToken {
    fn reset(self, lexer: &Lexer) {
        lexer.source_bytes_nth.set(self.unwind_index);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Identifier {
        inner: Identifier,
    },
    Digits {
        sequence: String,
        suffix: Option<Box<str>>
    },
    UnexpectedChar {
        index: Utf8CharBoundaryStartByte,
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
    /// `exit`
    KeywordExit,
    /// `:`
    SymColon,
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
    #[must_use = "You'd like to call its Display impl"]
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
            Self::KeywordExit => "keyword:exit",
            Self::SymColon => "sym:colon",
            Self::Reserved { .. } => "reserved_token",
        }
    }

    pub const fn is_error(&self) -> bool {
        matches!(self, Token::UnexpectedChar { .. })
    }

    pub const fn is_end(&self) -> bool {
        matches!(self, Token::EndOfFile)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct DisplayToken(&'static str);

impl Display for DisplayToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

#[cfg(test)]
mod tests {
    use origlang_ast::Identifier;
    use crate::lexer::{Lexer, Token};

    fn test(str_lit: &str) {
        let src = format!("var x = \"{str_lit}\"\n");
        let p = Lexer::create(&src);

        assert_eq!(p.next().data, Token::VarKeyword);
        assert_eq!(p.next().data, Token::Identifier {
            inner: Identifier::new("x".to_string()),
        });
        assert_eq!(p.next().data, Token::SymEq);
        assert_eq!(p.next().data, Token::StringLiteral(str_lit.to_string()));
    }

    #[test]
    fn parse_string_literal_ascii() {
        test("123456")
    }

    #[test]
    fn parse_string_literal_empty() {
        test("")
    }

    #[test]
    fn parse_string_literal_two_bytes() {
        test("\u{80}")
    }

    #[test]
    fn parse_string_literal_three_bytes() {
        test("\u{800}")
    }

    #[test]
    fn parse_string_literal_mixed_1_2() {
        test("1\u{80}")
    }

    #[test]
    fn parse_string_literal_mixed_1_3() {
        test("1あ")
    }

    #[test]
    fn parse_string_literal_mixed_1_4() {
        test("1\u{10000}")
    }

    #[test]
    fn parse_string_literal_mixed_2_1() {
        test("\u{80}1")
    }

    #[test]
    fn parse_string_literal_mixed_2_3() {
        test("\u{80}あ")
    }

    #[test]
    fn parse_string_literal_mixed_2_4() {
        test("\u{80}\u{10000}")
    }

    #[test]
    fn parse_string_literal_mixed_3_1() {
        test("あ1")
    }

    #[test]
    fn parse_string_literal_mixed_3_2() {
        test("あ\u{80}")
    }

    #[test]
    fn parse_string_literal_mixed_3_4() {
        test("あ\u{10000}")
    }

    #[test]
    fn parse_string_literal_mixed_4_1() {
        test("\u{10000}1")
    }

    #[test]
    fn parse_string_literal_mixed_4_2() {
        test("\u{10000}\u{80}")
    }

    #[test]
    fn parse_string_literal_mixed_4_3() {
        test("\u{10000}あ")
    }
}