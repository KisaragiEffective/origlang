use super::{Test, TestFailureCause};
use origlang_runtime::TypeBox;

#[test]
fn tuple_binding_does_not_change_equality_operands() {
    let src = "var x = true\nvar (x, b) = (1, x == true)\nprint x\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::NonCoercedInteger(1), TypeBox::Boolean(true)]
    );
}

#[test]
fn tuple_binding_does_not_change_arithmetic_operands() {
    let src = "var x = 1\nvar (x, b) = (true, x + 1)\nprint x\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::Boolean(true), TypeBox::NonCoercedInteger(2)]
    );
}

#[test]
fn tuple_binding_does_not_change_if_condition() {
    let src = "var x = true\nvar (x, b) = (1, if x then 2 else 3)\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::NonCoercedInteger(2)]
    );
}

#[test]
fn tuple_binding_does_not_replace_extraction_source() {
    let src = "var t = (1, 2)\nvar (t, b) = t\nprint t\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::NonCoercedInteger(1), TypeBox::NonCoercedInteger(2)]
    );
}

#[test]
fn tuple_binding_does_not_shrink_extraction_source() {
    let src = "var t = ((1, 2), 3, 4)\nvar (t, b, c) = t\nprint b\nprint c\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![TypeBox::NonCoercedInteger(3), TypeBox::NonCoercedInteger(4)]
    );
}

#[test]
fn tuple_binding_preserves_nested_shadowing() {
    let src = "var t = (true, (2, 3))\nvar (t, (b, c)) = t\nprint t\nprint b\nprint c\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![
            TypeBox::Boolean(true),
            TypeBox::NonCoercedInteger(2),
            TypeBox::NonCoercedInteger(3)
        ]
    );
}

#[test]
fn tuple_binding_evaluates_discarded_elements_once_before_binding() {
    let src = "var x = 7\nvar (x, _, b) = (true, block\nprint x\n0\nend, block\nprint x\n(x)\nend)\nprint x\nprint b\n";
    assert_eq!(
        Test::evaluated_expressions(src).unwrap(),
        vec![
            TypeBox::NonCoercedInteger(7),
            TypeBox::NonCoercedInteger(7),
            TypeBox::Boolean(true),
            TypeBox::NonCoercedInteger(7)
        ]
    );
}

#[test]
fn tuple_binding_evaluates_if_source_once() {
    let src =
        "var (a, b) = if true then block\nprint 99\n(1, 2)\nend else (3, 4)\nprint a\nprint b\n";
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
