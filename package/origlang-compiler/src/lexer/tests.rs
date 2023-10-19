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

use std::num::NonZeroUsize;
use origlang_source_span::{Pointed, SourcePosition};
use crate::chars::boundary::Utf8CharBoundaryStartByte;

#[test]
fn token_location() {
    // TODO: var y = 2 w/o new line -> unexpected panic (scan_digits_suffix_opt)
    let src = "var x = 1\nvar y = 2\n";
    let lexer = Lexer::create(src);

    assert_eq!(lexer.next(), Pointed {
        data: Token::VarKeyword,
        position: SourcePosition {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(1).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::Identifier {
            inner: Identifier::new("x".to_string())
        },
        position: SourcePosition {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(5).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::SymEq,
        position: SourcePosition {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(7).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::Digits {
            sequence: "1".to_string(),
            suffix: None,
        },
        position: SourcePosition {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(9).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::NewLine,
        position: SourcePosition {
            line: NonZeroUsize::new(1).unwrap(),
            column: NonZeroUsize::new(10).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::VarKeyword,
        position: SourcePosition {
            line: NonZeroUsize::new(2).unwrap(),
            column: NonZeroUsize::new(1).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::Identifier {
            inner: Identifier::new("y".to_string())
        },
        position: SourcePosition {
            line: NonZeroUsize::new(2).unwrap(),
            column: NonZeroUsize::new(5).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::SymEq,
        position: SourcePosition {
            line: NonZeroUsize::new(2).unwrap(),
            column: NonZeroUsize::new(7).unwrap()
        }
    });

    assert_eq!(lexer.next(), Pointed {
        data: Token::Digits {
            sequence: "2".to_string(),
            suffix: None,
        },
        position: SourcePosition {
            line: NonZeroUsize::new(2).unwrap(),
            column: NonZeroUsize::new(9).unwrap()
        }
    });
}

#[test]
fn digit_regression() {
    const D: &str = "123456";
    let lexer = Lexer::create(D);
    assert_eq!(lexer.next().data, Token::Digits {
        sequence: D.to_string(),
        suffix: None,
    });

    const EMPTY: &str = "";
    let lexer = Lexer::create(EMPTY);
    assert_eq!(lexer.next().data, Token::EndOfFile);
}

#[test]
fn crlf_positive() {
    const S: &str = "\r\n";
    let lexer = Lexer::create(S);
    assert_eq!(lexer.next().data, Token::NewLine);
    assert_eq!(lexer.next().data, Token::EndOfFile);
}

#[test]
fn crlf_negative() {
    const S: &str = "\r";
    let lexer = Lexer::create(S);
    assert_eq!(lexer.next().data, Token::UnexpectedChar {
        index: Utf8CharBoundaryStartByte::new(0),
        char: '\r',
    });
}
