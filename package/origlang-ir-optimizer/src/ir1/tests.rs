use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ir::IR1;
use origlang_typesystem_model::{Type, TypedExpression, TypedIntLiteral};
use crate::ir1::{EliminateAfterExit, FoldBinaryOperatorInvocationWithConstant, FoldIfWithConstantCondition, InlineSimpleBlock};

#[test]
fn fold_binary_operator_is_recursive() {
    let opt_output = FoldBinaryOperatorInvocationWithConstant(vec![
        IR1::Output(TypedExpression::BinaryOperator {
            lhs: Box::new(TypedExpression::BinaryOperator {
                lhs: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(1))),
                rhs: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(2))),
                operator: BinaryOperatorKind::Plus,
                return_type: Type::GenericInteger,
            }),
            rhs: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(3))),
            operator: BinaryOperatorKind::Plus,
            return_type: Type::GenericInteger,
        })
    ]).optimize();

    assert_eq!(opt_output, vec![
        IR1::Output(TypedExpression::IntLiteral(TypedIntLiteral::Generic(6)))
    ]);
}

#[test]
fn fold_comparison_between_integrals_into_boolean_literal() {
    // TODO: test all comparison operator
    let opt_output = FoldBinaryOperatorInvocationWithConstant(vec![
        IR1::Output(TypedExpression::BinaryOperator {
            lhs: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(2))),
            rhs: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(3))),
            operator: BinaryOperatorKind::Less,
            return_type: Type::Boolean,
        })
    ]).optimize();

    assert_eq!(opt_output, [IR1::Output(TypedExpression::BooleanLiteral(true))]);
}

#[test]
fn fold_if_with_constant_condition() {
    let opt_output = FoldIfWithConstantCondition(vec![
        IR1::Output(TypedExpression::If {
            condition: Box::new(TypedExpression::BooleanLiteral(true)),
            then: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(1))),
            els: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(2))),
            return_type: Type::GenericInteger,
        })
    ]).optimize();

    assert_eq!(opt_output, vec![
        IR1::Output(TypedExpression::IntLiteral(TypedIntLiteral::Generic(1)))
    ]);

    let opt_output = FoldIfWithConstantCondition(vec![
        IR1::Output(TypedExpression::If {
            condition: Box::new(TypedExpression::BooleanLiteral(false)),
            then: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(1))),
            els: Box::new(TypedExpression::IntLiteral(TypedIntLiteral::Generic(2))),
            return_type: Type::GenericInteger,
        })
    ]).optimize();

    assert_eq!(opt_output, vec![
        IR1::Output(TypedExpression::IntLiteral(TypedIntLiteral::Generic(2)))
    ]);

}

#[test]
fn inline_simple_block() {
    let opt_output = InlineSimpleBlock(vec![
        IR1::Output(TypedExpression::Block {
            inner: vec![],
            final_expression: Box::new(TypedExpression::UnitLiteral),
            return_type: Type::Unit,
        })
    ]).optimize();

    assert_eq!(opt_output, vec![
        IR1::Output(TypedExpression::UnitLiteral)
    ]);
}

#[test]
fn cut_down_after_exit() {
    let opt_output = EliminateAfterExit(vec![
        IR1::Exit,
        IR1::Output(TypedExpression::BooleanLiteral(false))
    ]).optimize();

    assert_eq!(opt_output, [ IR1::Exit ]);
}
