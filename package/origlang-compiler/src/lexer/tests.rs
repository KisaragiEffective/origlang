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

#[test]
fn avoid_off_read() {
    const S: &str = r#"var x = "4あ"
"#;
    let lexer = Lexer::create(S);
    let k = S.chars().count();
    for i in 0..k {
        assert_eq!(lexer.consume_char().expect("oops"), S.chars().nth(i).expect("out of bounds from literal"))
    }
}
