use super::{Test, TestFailureCause};
use origlang_runtime::TypeBox;

#[test]
fn comparisons_have_boolean_type() {
    for suffix in ["", "i8", "i16", "i32", "i64"] {
        for operator in ["<", "<=", ">", ">="] {
            let src =
                format!("var b: Bool = 1{suffix} {operator} 2{suffix}\nprint if b then 3 else 4\n");
            let expected = if operator.starts_with('<') { 3 } else { 4 };
            assert_eq!(
                Test::evaluated_expressions(&src).unwrap(),
                vec![TypeBox::NonCoercedInteger(expected)]
            );
            let src = format!("print (1{suffix} {operator} 2{suffix}) + 1{suffix}\n");
            assert!(matches!(
                Test::evaluated_expressions(&src),
                Err(TestFailureCause::TypeChecker(_))
            ));
        }
    }
}

#[test]
fn block_variables_do_not_escape() {
    let src = "block\nvar x = 42\nend\nprint x\n";
    assert!(matches!(
        Test::evaluated_expressions(src),
        Err(TestFailureCause::TypeChecker(_))
    ));
}

#[test]
fn shadowing_restores_outer_type() {
    let src = "var x = 1\nblock\nvar x = true\nprint x\nend\nx = 2\nprint x\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::Boolean(true), TypeBox::NonCoercedInteger(2)]
    );
}

#[test]
fn if_branches_do_not_share_variables() {
    let src = "print if true then block\nvar x = 1\n1\nend else block\n(x)\nend\n";
    assert!(matches!(
        Test::evaluated_expressions(src),
        Err(TestFailureCause::TypeChecker(_))
    ));
}

#[test]
fn block_final_expression_sees_local_variables() {
    let src = "var x = 1\nprint block\nvar x = 42\n(x)\nend\nprint x\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![
            TypeBox::NonCoercedInteger(42),
            TypeBox::NonCoercedInteger(1)
        ]
    );
}

#[test]
fn tuple_destruction_preserves_block_effects() {
    let src = "var (a, b) = block\nprint 99\nvar x = 1\n((x), 2)\nend\nprint a\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![
            TypeBox::NonCoercedInteger(99),
            TypeBox::NonCoercedInteger(1),
            TypeBox::NonCoercedInteger(2)
        ]
    );
}

#[test]
fn tuple_destruction_requires_matching_arity() {
    for src in [
        "var (a, b, c) = (1, 2)\n",
        "var (a, b) = (1, 2, 3)\n",
        "var (a, (b, c)) = (1, (2, 3, 4))\n",
        "var x = (1, 2)\nvar (a, b, c) = x\n",
        "var (a, b) = block\n(1, 2, 3)\nend\n",
    ] {
        assert!(
            matches!(
                Test::evaluated_expressions(src),
                Err(TestFailureCause::TypeChecker(_))
            ),
            "{src}"
        );
    }
}

#[test]
fn shifts_require_integer_operands() {
    for operator in ["<<", ">>"] {
        for value in ["true", "\"a\"", "()", "(1, 2)"] {
            let src = format!("print {value} {operator} {value}\n");
            assert!(
                matches!(
                    Test::evaluated_expressions(&src),
                    Err(TestFailureCause::TypeChecker(_))
                ),
                "{src}"
            );
        }
    }
}

#[test]
fn block_final_expression_can_start_with_identifier() {
    for expr in ["x", "x + 1", "(x)", "if true then x else 0"] {
        let src = format!("var x = 42\nprint block\n{expr}\nend\n");
        let expected = if expr == "x + 1" { 43 } else { 42 };
        assert_eq!(
            Test::evaluated_expressions(&src).unwrap(),
            vec![TypeBox::NonCoercedInteger(expected)]
        );
    }
}
