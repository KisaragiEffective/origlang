#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]
pub mod error;
#[cfg(test)]
mod tests;
pub mod token;
mod boundary;

use std::cell::Cell;

use std::num::NonZeroUsize;
use log::{debug, trace, warn};
use self::error::LexerError;
use origlang_ast::{Comment, Identifier};
use origlang_source_span::{SourcePosition as SourcePos, Pointed as WithPosition};
use crate::boundary::{Utf8CharBoundaryStartByte, Utf8CharStride};
use crate::error::OutOfRangeError;
use crate::token::{TemporalLexerUnwindToken, Token};

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
pub struct LcManager {
    /// don't use directly.
    line: Cell<NonZeroUsize>,
    /// don't use directly.
    column: Cell<NonZeroUsize>,
}

#[derive(Debug)]
pub struct Lexer<'src> {
    source_bytes_nth: Cell<Utf8CharBoundaryStartByte>,
    source: &'src str,
    lc_manager: LcManager,
}

impl<'src> Lexer<'src> {
    #[must_use = "Lexer do nothing unless calling parsing function"]
    #[allow(clippy::missing_panics_doc)]
    pub fn create(source: &'src str) -> Self {
        Self {
            source_bytes_nth: Cell::new(Utf8CharBoundaryStartByte::new(0)),
            source,
            lc_manager: LcManager {
                #[allow(clippy::missing_panics_doc)]
                line: Cell::new(1.try_into().expect("unreachable!!")),
                #[allow(clippy::missing_panics_doc)]
                column: Cell::new(1.try_into().expect("unreachable!!")),
            }
        }
    }
}

impl Lexer<'_> {
    //noinspection SpellCheckingInspection
    fn drain_space(&self) {
        trace!("drain_space: start vvvvvvvvvvvvvvvvvvv");
        while !self.reached_end() {
            if self.try_and_eat_str(" ") || self.try_and_eat_str("\t") {
            } else {
                break
            }
        }
        trace!("drain_space: end   ^^^^^^^^^^^^^^^^^^^");
    }

    /// Note
    /// calling [`Self::advance_bytes`] or [`Self::set_current_index`] is error-prone.
    fn try_and_eat_str(&self, s: &str) -> bool {
        trace!("lexer:try:{s:?}");
        let start = self.source_bytes_nth.get();
        let end_exclusive = start.as_usize() + s.len();
        let Some(b) = self.source.get(start.as_usize()..end_exclusive) else { return false };
        if s != b {
            return false
        }

        self.set_current_index(Utf8CharBoundaryStartByte::new(end_exclusive));
        
        true
    }

    fn next_inner(&self) -> Token {
        let v =
            self.reached_end().then_some(Token::EndOfFile)
            .or_else(|| self.try_and_eat_str("\r\n").then_some(Token::NewLine))
            .or_else(|| self.try_and_eat_str("\n").then_some(Token::NewLine))
            .or_else(|| self.try_and_eat_str("==").then_some(Token::PartEqEq))
            .or_else(|| self.try_and_eat_str("=").then_some(Token::SymEq))
            .or_else(|| self.try_and_eat_str("+").then_some(Token::SymPlus))
            .or_else(|| self.try_and_eat_str("-").then_some(Token::SymMinus))
            .or_else(|| self.try_and_eat_str("*").then_some(Token::SymAsterisk))
            .or_else(|| self.try_and_eat_str("//").then_some(self.scan_line_comment()))
            .or_else(|| self.try_and_eat_str("/").then_some(Token::SymSlash))
            .or_else(|| self.try_and_eat_str("(").then_some(Token::SymLeftPar))
            .or_else(|| self.try_and_eat_str(")").then_some(Token::SymRightPar))
            .or_else(|| self.try_and_eat_str("<=>").then_some(Token::PartLessEqMore))
            .or_else(|| self.try_and_eat_str("<<").then_some(Token::PartLessLess))
            .or_else(|| self.try_and_eat_str("<=").then_some(Token::PartLessEq))
            .or_else(|| self.try_and_eat_str("<").then_some(Token::SymLess))
            .or_else(|| self.try_and_eat_str(">>").then_some(Token::PartMoreMore))
            .or_else(|| self.try_and_eat_str(">=").then_some(Token::PartMoreEq))
            .or_else(|| self.try_and_eat_str(">").then_some(Token::SymMore))
            .or_else(|| self.try_and_eat_str("!=").then_some(Token::PartBangEq))
            .or_else(|| self.try_and_eat_str("!").then_some(Token::SymBang))    
            .or_else(|| self.try_and_eat_str("\"").then_some(self.scan_string_literal()))
            .or_else(|| self.scan_digits())
            .or_else(|| self.try_and_eat_str(",").then_some(Token::SymComma))
            .or_else(|| self.try_and_eat_str(":").then_some(Token::SymColon))
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

                    #[allow(clippy::or_fun_call)] // latter is fine because it does not cost
                    let c = s.chars().next().ok_or(this.report_out_of_range_error())?;


                    Ok(c)
                }
                
                Token::UnexpectedChar {
                    // TODO: this is cold path, so may convert boundary to char_nth.
                    index: self.source_bytes_nth.get(),
                    char: current_char(self).expect("unexpected_char"),
                }
            });
        
        v
    }

    pub fn next(&self) -> WithPosition<Token> {
        debug!("-------------------------------------------------");
        self.drain_space();
        
        if self.reached_end() {
            return Token::EndOfFile.with_pos(self)
        }

        let pos = self.current_pos();

        let r = WithPosition {
            data: self.next_inner(),
            position: pos,
        };

        debug!("next: {r:?}", r = &r);
        r
    }

    fn current_pos(&self) -> SourcePos {
        SourcePos {
            line: self.line(),
            column: self.column(),
        }
    }

    fn scan_digit_suffix_opt(&self) -> Option<&str> {
        if self.reached_end() {
            return None
        }

        for s in ["i8", "i16", "i32", "i64"] {
            let a = self.try_and_eat_str(s);

            if a {
                return Some(s)
            }
        }

        None
    }

    fn scan_digits(&self) -> Option<Token> {
        debug!("lexer:digit");
        let mut plus = 0;

        loop {
            let r = self.byte_skip_n(plus);

            if let Ok(b) = r {
                if b.is_ascii_digit() {
                    plus += 1;
                } else {
                    break
                }
            } else {
                break
            }
        }

        if plus == 0 {
            None
        } else {
            let start = self.source_bytes_nth.get().as_usize();
            let end_inclusive = start + plus;
            self.set_current_index(Utf8CharBoundaryStartByte::new(end_inclusive));

            let scanned = self.source[start..end_inclusive].to_string();
            let builtin_suffix = self.scan_digit_suffix_opt().map(|s| s.to_string().into_boxed_str());

            debug!("digit: done ({scanned} {builtin_suffix:?})");

            Some(
                Token::Digits {
                    sequence: scanned,
                    suffix: builtin_suffix,
                }
            )
        }

    }

    fn scan_string_literal(&self) -> Token {
        let start = self.source_bytes_nth.get().as_usize();
        let rel_pos = self.source[start..].find('"').unwrap_or(self.source.len() - start);
        self.advance_bytes(rel_pos + 1);

        let s = self.source[start..(start + rel_pos)].to_string();
        Token::StringLiteral(s)
    }

    fn advance_bytes(&self, advance: usize) {
        self.set_current_index(Utf8CharBoundaryStartByte::new(self.source_bytes_nth.get().as_usize() + advance));
    }

    #[inline(never)]
    fn set_current_index(&self, future_index: Utf8CharBoundaryStartByte) {
        let old = self.source_bytes_nth.get().as_usize();
        let new = future_index.as_usize();
        debug!("index: {old} -> {future_index:?}");

        if old == new {
            return
        }

        let current_line = self.line().get();

        let src = &self.source;
        if old < new {
            // forward
            let new_line = current_line + src[old..new].bytes().filter(|x| *x == b'\n').count();
            let new_col = src[old..new].rfind('\n').map_or_else(|| {
                let mut c = self.column().get();
                c += new - old;

                c
            }, |old_relative| {
                new - (old + old_relative)
            });

            self.set_line(NonZeroUsize::new(new_line).expect("overflow"));
            self.set_column(NonZeroUsize::new(new_col).expect("overflow"));
        } else {
            // THIS BRANCH IS IMPORTANT!!! OTHERWISE, RESET OPERATION WILL NOT WORK!!!
            // back
            let new_line = current_line - src[new..old].bytes().filter(|x| *x == b'\n').count();
            let new_col = src[new..old].find('\n').map_or_else(|| {
                let mut c = self.column().get();
                c -= old - new;

                c
            }, |new_relative| {
                // .......................NEW.................OLD
                //                         |<--------N------>|
                let nr = new + new_relative;
                src[..nr].rfind('\n').map_or(nr, |most_recent_nl| {
                    // ..............NEW.................OLD
                    //                |<--------N------>|
                    // |<-----MRN-------------->|

                    // this is effectively static assertion, should not
                    // cost on runtime.
                    assert!(most_recent_nl < nr);
                    nr - most_recent_nl
                })
            });

            self.set_line(NonZeroUsize::new(new_line).expect("overflow: line"));
            self.set_column(NonZeroUsize::new(new_col).expect("overflow: col"));
        }

        self.source_bytes_nth.set(future_index);
    }

    fn scan_line_comment(&self) -> Token {
        let start = self.source_bytes_nth.get().as_usize();
        let rel_pos = self.source[start..].find('\n').unwrap_or(self.source.len());
        self.advance_bytes(rel_pos);

        let content = self.source[start..(start + rel_pos)].to_string();
        Token::Comment {
            content: Comment {
                content
            }
        }
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
            self.advance_bytes(plus);

            Ok(Some(s))
        } else {
            Ok(None)
        }
    }

    fn current_byte(&self) -> Result<u8, LexerError> {
        self.source.as_bytes().get(self.source_bytes_nth.get().as_usize()).copied().ok_or_else(|| self.report_out_of_range_error())
    }

    fn byte_skip_n(&self, skip: usize) -> Result<u8, LexerError> {
        self.source.as_bytes().get(self.source_bytes_nth.get().as_usize() + skip).copied().ok_or_else(|| self.report_out_of_range_error())
    }

    fn report_out_of_range_error(&self) -> LexerError {
        LexerError::OutOfRange(OutOfRangeError {
            current: self.source_bytes_nth.get(),
            max: self.source.len(),
        })
    }
    
    fn line(&self) -> NonZeroUsize {
        self.lc_manager.line.get()
    }
    
    fn set_line(&self, line: NonZeroUsize) {
        debug!("line: {old} -> {new}", old = self.line(), new = line);
        self.lc_manager.line.set(line);
    }
    
    fn column(&self) -> NonZeroUsize {
        self.lc_manager.column.get()
    }
    
    fn set_column(&self, column: NonZeroUsize) {
        debug!("column: {old} -> {new}", old = self.column(), new = column);
        self.lc_manager.column.set(column);
    }
}
