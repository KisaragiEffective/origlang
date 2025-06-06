use crate::boundary::Utf8CharBoundaryStartByte;
use crate::token::internal::DisplayToken;
use crate::Lexer;
use origlang_ast::{Comment, Identifier};

pub struct TemporalLexerUnwindToken {
    unwind_index: Utf8CharBoundaryStartByte,
}

impl TemporalLexerUnwindToken {
    #[must_use = "call Self::reset to invoke"]
    pub const fn new(reset_to: Utf8CharBoundaryStartByte) -> Self {
        Self {
            unwind_index: reset_to,
        }
    }

    pub fn reset(self, lexer: &Lexer) {
        lexer.set_current_index(self.unwind_index);
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Identifier {
        inner: Identifier,
    },
    Digits {
        sequence: String,
        suffix: Option<Box<str>>,
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
    /// `<<`
    PartLessLess,
    /// `>>`
    PartMoreMore,
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
    StringLiteral(Box<str>),
    /// reserved for future use.
    Reserved {
        matched: Box<str>,
    },
    /// `_`
    SymUnderscore,
    /// `type`
    KeywordType,
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
            Self::SymUnderscore => "sym:underscore",
            Self::KeywordType => "keyword:type",
            Self::PartLessLess => "part:less_less",
            Self::PartMoreMore => "part:more_more",
        }
    }

    #[must_use]
    pub const fn is_error(&self) -> bool {
        matches!(self, Self::UnexpectedChar { .. })
    }

    #[must_use]
    pub const fn is_end(&self) -> bool {
        matches!(self, Self::EndOfFile)
    }
}

pub mod internal {
    use std::fmt::{Display, Formatter};

    #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
    pub struct DisplayToken(pub(super) &'static str);

    impl Display for DisplayToken {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.0)
        }
    }
}
