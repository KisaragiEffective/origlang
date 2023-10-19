pub(crate) mod error;
#[cfg(test)]
mod tests;
pub mod token;

use std::borrow::Cow;
use std::cell::Cell;
use std::convert::Infallible;

use std::num::NonZeroUsize;
use log::{debug, trace, warn};
use self::error::LexerError;
use origlang_ast::{Comment, Identifier};
use origlang_source_span::{SourcePosition as SourcePos, Pointed as WithPosition};
use crate::chars::boundary::{Utf8CharBoundaryStartByte, Utf8CharStride};
use crate::lexer::error::OutOfRangeError;
use crate::lexer::token::{TemporalLexerUnwindToken, Token};

static KEYWORDS: [&str; 12] =
    ["var", "if", "else", "then", "exit", "true", "false", "print", "block", "end", "type", "_"];

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

#[derive(Debug)]
pub struct Lexer<'src> {
    source_bytes_nth: Cell<Utf8CharBoundaryStartByte>,
    source: &'src str,
    line: Cell<NonZeroUsize>,
    column: Cell<NonZeroUsize>,
}

impl<'src> Lexer<'src> {
    #[must_use = "Lexer do nothing unless calling parsing function"]
    pub fn create(source: &'src str) -> Self {
        Self {
            source_bytes_nth: Cell::new(Utf8CharBoundaryStartByte::new(0)),
            source,
            line: Cell::new(NonZeroUsize::new(1).unwrap()),
            column: Cell::new(NonZeroUsize::new(1).unwrap()),
        }
    }
}

impl Lexer<'_> {
    fn drain_space(&self) {
        trace!("drain_space: start vvvvvvvvvvvvvvvvvvv");
        while !self.reached_end() {
            if self.try_and_eat_str(" ").unwrap() == Some(" ") || self.try_and_eat_str("\t").unwrap() == Some("\t") {
            } else {
                break
            }
        }
        trace!("drain_space: end   ^^^^^^^^^^^^^^^^^^^");
    }

    /// Note
    /// calling [`Self::advance_bytes`], [`Self::advance`], or [`Self::set_current_index`] is error-prone.
    fn try_and_eat_str<'s>(&self, s: &'s str) -> Result<Option<&'s str>, Infallible> {
        trace!("lexer:try:{s:?}");
        let start = self.source_bytes_nth.get();
        let end_exclusive = start.as_usize() + s.len();
        if let Some(b) = self.source.get((start.as_usize())..end_exclusive) {
            if s == b {
                match self.set_current_index(Utf8CharBoundaryStartByte::new(end_exclusive)) {
                    Ok(_) => Ok(Some(s)),
                    Err(OutOfRangeError { .. }) => Ok(None),
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    #[allow(clippy::unnecessary_wraps)]
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
            .or_else(|| self.try_and_eat_str("\r\n").expect("huh?").map(|_| Token::NewLine))
            .or_else(|| self.try_and_eat_str("\n").expect("huh?").map(|_| Token::NewLine))
            .or_else(||
                fold!(
                    self.try_and_eat_str(r#"="#).expect("huh?"),
                    {
                        let double_eq = self.try_and_eat_str(r#"="#).expect("huh?");
                        if double_eq.is_some() {
                            Some(Token::PartEqEq)
                        } else {
                            Some(Token::SymEq)
                        }
                    },
                    None
                )
            )
            .or_else(|| self.try_and_eat_str(r#"+"#).expect("huh?").map(|_| Token::SymPlus))
            .or_else(|| self.try_and_eat_str(r#"-"#).expect("huh?").map(|_| Token::SymMinus))
            .or_else(|| self.try_and_eat_str(r#"*"#).expect("huh?").map(|_| Token::SymAsterisk))
            .or_else(||
                fold!(
                    self.try_and_eat_str(r#"/"#).expect("huh?"),
                    fold!(
                        self.try_and_eat_str(r#"/"#).expect("huh?"),
                        Some(self.scan_line_comment().expect("unable to parse comment")),
                        Some(Token::SymSlash)
                    ),
                    None
                )
            )
            .or_else(|| self.try_and_eat_str(r#"("#).expect("huh?").map(|_| Token::SymLeftPar))
            .or_else(|| self.try_and_eat_str(r#")"#).expect("huh?").map(|_| Token::SymRightPar))
            .or_else(|| {
                if let Some(_) = self.try_and_eat_str(r#"<"#).expect("huh?") {
                    if let Some(_) = self.try_and_eat_str(r#"="#).expect("huh?") {
                        if let Some(_) = self.try_and_eat_str(r#">"#).expect("huh?") {
                            Some(Token::PartLessEqMore)
                        } else {
                            Some(Token::PartLessEq)
                        }
                    } else if let Some(_) = self.try_and_eat_str(r#"<"#).expect("huh?") {
                        Some(Token::PartLessLess)
                    } else {
                        Some(Token::SymLess)
                    }
                } else {
                    None
                }
            })
            .or_else(|| {
                if let Some(_) = self.try_and_eat_str(r#">"#).expect("huh?") {
                    if let Some(_) = self.try_and_eat_str(r#"="#).expect("huh?") {
                        Some(Token::PartMoreEq)
                    } else if let Some(_) = self.try_and_eat_str(r#">"#).expect("huh?") {
                        Some(Token::PartMoreMore)
                    } else {
                        Some(Token::SymMore)
                    }
                } else {
                    None
                }
            })
            .or_else(|| 
                fold!(
                    self.try_and_eat_str(r#"!"#).expect("huh?"),
                    fold!(
                        self.try_and_eat_str(r#"="#).expect("huh?"),
                        Some(Token::PartBangEq),
                        Some(Token::SymBang)
                    ),
                    None
                )
            )
            .or_else(|| 
                fold!(
                    self.try_and_eat_str(r#"""#).expect("huh?"),
                    Some(self.scan_string_literal().expect("unable to parse string literal")),
                    None
                )
            )
            .or_else(|| self.scan_digits().expect("huh?"))
            .or_else(||
                fold!(
                    self.try_and_eat_str(r#","#).expect("huh?"),
                    Some(Token::SymComma),
                    None
                )
            )
            .or_else(||
                fold!(
                    self.try_and_eat_str(r#":"#).expect("huh?"),
                    Some(Token::SymColon),
                    None
                )
            )
            .or_else(|| {
                self.scan_identifier()
                    .ok()
                    .flatten()
                    .map(|scanned| {
                        let is_keyword = KEYWORDS.contains(&scanned.as_name());
                        if is_keyword {
                            match scanned.as_name() {
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
                                "type" => Token::KeywordType,
                                "_" => Token::SymUnderscore,
                                other => Token::Reserved {
                                    matched: other.to_string(),
                                }
                            }
                        } else {
                            Token::Identifier { inner: scanned }
                        }
                    })
            })
            // dont eager evaluate
            .unwrap_or_else(|| {
                fn current_char(this: &Lexer) -> Result<char, LexerError> {
                    let current_boundary = this.source_bytes_nth.get();
                    let index = current_boundary.as_usize();
                    let stride = this.current_char_stride()?;


                    let s = unsafe { this.source.get_unchecked(index..(index + stride.as_usize())) };

                    let c = s.chars().next().ok_or(this.report_out_of_range_error())?;


                    Ok(c)
                }
                
                Token::UnexpectedChar {
                    // TODO: this is cold path, so may convert boundary to char_nth.
                    index: self.source_bytes_nth.get(),
                    char: current_char(self).expect("unexpected_char"),
                }
            });
        Ok(v)
    }

    pub fn next(&self) -> WithPosition<Token> {
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile.with_pos(self)
        }

        let pos = self.current_pos();

        let r = WithPosition {
            data: self.next_inner()
                .expect("Lexer phase error"),
            position: pos,
        };

        debug!("next: {r:?}", r = &r);
        r
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line.get(),
            column: self.column.get(),
        }
    }

    fn scan_digit_suffix_opt(&self) -> Result<Option<Box<str>>, LexerError> {
        if self.reached_end() {
            return Ok(None)
        }

        for s in ["i8", "i16", "i32", "i64"] {
            let a = self.try_and_eat_str(s)?;

            if let Some(a) = a {
                return Ok(Some(a.to_string().into_boxed_str()))
            }
        }

        Ok(None)
    }

    fn scan_digits(&self) -> Result<Option<Token>, LexerError> {
        debug!("lexer:digit");
        let mut plus = 0;

        loop {
            let r = self.byte_skip_n(plus);

            if let Ok(b) = r {
                if (b'0'..b'9').contains(&b) {
                    plus += 1;
                } else {
                    break
                }
            } else {
                break
            }
        }

        if plus == 0 {
            Ok(None)
        } else {
            let start = self.source_bytes_nth.get().as_usize();
            let end_inclusive = start + plus;
            self.set_current_index(Utf8CharBoundaryStartByte::new(end_inclusive))?;

            let scanned = self.source[start..end_inclusive].to_string();
            let builtin_suffix = self.scan_digit_suffix_opt()?;

            debug!("digit: done ({scanned} {builtin_suffix:?})");

            Ok(Some(
                Token::Digits {
                    sequence: scanned,
                    suffix: builtin_suffix,
                }
            ))
        }

    }

    fn scan_string_literal(&self) -> Result<Token, LexerError> {
        let start = self.source_bytes_nth.get().as_usize();
        let rel_pos = self.source[start..].find('"').unwrap_or(self.source.len() - start);
        self.advance_bytes(rel_pos + 1)?;

        let s = self.source[start..(start + rel_pos)].to_string();
        Ok(Token::StringLiteral(s))
    }

    fn advance_bytes(&self, advance: usize) -> Result<(), OutOfRangeError> {
        self.set_current_index(Utf8CharBoundaryStartByte::new(self.source_bytes_nth.get().as_usize() + advance))
    }

    #[inline(never)]
    fn set_current_index(&self, future_index: Utf8CharBoundaryStartByte) -> Result<(), OutOfRangeError> {
        let old = self.source_bytes_nth.get().as_usize();
        let new = future_index.as_usize();

        if old == new {
            return Ok(())
        }

        let current_line = self.line.get().get();

        let src = &self.source;
        if old < new {
            // forward
            let new_line = current_line + src[old..new].bytes().filter(|x| *x == b'\n').count();
            let new_col = if let Some(old_relative) = src[old..new].rfind('\n') {
                // .......................OLD.................NEW
                //                         |<--------N------>|
                new - (old + old_relative)
            } else {
                let mut c = self.column.get().get();
                c += (new - old);

                c
            };

            self.line.set(NonZeroUsize::new(new_line).expect("overflow"));
            self.column.set(NonZeroUsize::new(new_col).expect("overflow"))
        } else {
            // back
            let new_line = current_line - src[new..old].bytes().filter(|x| *x == b'\n').count();
            let new_col = if let Some(new_relative) = src[new..old].find('\n') {
                // .......................NEW.................OLD
                //                         |<--------N------>|
                let nr = new + new_relative;
                if let Some(most_recent_nl) = src[..nr].rfind('\n') {
                    // ..............NEW.................OLD
                    //                |<--------N------>|
                    // |<-----MRN-------------->|

                    // this is effectively static assertion, should not
                    // cost on runtime.
                    assert!(most_recent_nl < nr);
                    nr - most_recent_nl
                } else {
                    nr
                }
            } else {
                let mut c = self.column.get().get();
                c += old - new;

                c
            };

            self.line.set(NonZeroUsize::new(new_line).expect("overflow"));
            self.column.set(NonZeroUsize::new(new_col).expect("overflow"))
        }

        debug!("index: requested = {future_index:?}");
        self.source_bytes_nth.set(future_index);

        Ok(())
    }

    fn scan_line_comment(&self) -> Result<Token, LexerError> {
        let start = self.source_bytes_nth.get().as_usize();
        let rel_pos = self.source[start..].find("\n").unwrap_or(self.source.len());
        self.advance_bytes(rel_pos)?;

        let content = self.source[start..(start + rel_pos)].to_string();
        Ok(Token::Comment {
            content: Comment {
                content
            }
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
        } else if heading_byte & 0b1111_0000 == 0b1111_0000 {
            Utf8CharStride::Four
        } else if heading_byte & 0b1110_0000 == 0b1110_0000 {
            Utf8CharStride::Three
        } else if heading_byte & 0b1100_0000 == 0b1100_0000 {
            Utf8CharStride::Two
        } else {
            return Err(LexerError::MalformedAsUtf8 {
                boundary: current_boundary,
            })
        };

        Ok(stride)
    }

    fn reached_end(&self) -> bool {
        // <&str>::len() yields length of BYTES, not CHARS
        self.source_bytes_nth.get().as_usize() >= self.source.len()
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

    fn scan_identifier(&self) -> Result<Option<Identifier>, LexerError> {
        debug!("lexer:identifier");

        let first = self.current_byte()?;
        let mut plus = 0;

        if first.is_ascii_alphabetic() || first == b'_' {
            plus += 1;
            loop {
                trace!("lexer:identifier: {plus}");
                match self.byte_skip_n(plus) {
                    Ok(b) => {
                        if b.is_ascii_alphanumeric() || b == b'_' {
                            plus += 1;
                        } else {
                            break
                        }
                    }
                    Err(e) => {
                        warn!("discarding error: {e}");
                        break
                    }
                }
            }

            debug!("lexer:identifier: length of {plus}");
            let start = self.source_bytes_nth.get().as_usize();
            let s = Identifier::new(self.source[start..(start + plus)].to_string());
            self.advance_bytes(plus)?;

            Ok(Some(s))
        } else {
            Ok(None)
        }
    }

    fn current_byte(&self) -> Result<u8, LexerError> {
        self.source.bytes().nth(self.source_bytes_nth.get().as_usize()).ok_or_else(|| self.report_out_of_range_error())
    }

    fn byte_skip_n(&self, skip: usize) -> Result<u8, LexerError> {
        self.source.bytes().nth(self.source_bytes_nth.get().as_usize() + skip).ok_or_else(|| self.report_out_of_range_error())
    }

    fn report_out_of_range_error(&self) -> LexerError {
        LexerError::OutOfRange(OutOfRangeError {
            current: self.source_bytes_nth.get(),
            max: self.source.len(),
        })
    }
}
