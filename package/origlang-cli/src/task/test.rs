#![forbid(dead_code)]
#![allow(clippy::unnecessary_wraps)]

use log::{debug, info};
use thiserror::Error;
use origlang_compiler::parser::{ParserError, SimpleErrorWithPos};
use origlang_runtime::{Runtime, TypeBox, Accumulate, DisplayTupleValue};
use crate::task::Task;
use origlang_ast::{AtomicPattern, Comment, Identifier, RootAst, Statement, TypeSignature};
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_compiler::type_check::error::TypeCheckError;
use origlang_compiler::type_check::TypeChecker;
use origlang_ir::IntoVerbatimSequencedIR;
use origlang_ir_optimizer::lower::{EachStep, LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::{NoOptimization, SimpleOptimization};
use crate::error::TaskExecutionError;

type Err = TestFailureCause;

#[derive(Error, Debug)]
pub enum TestFailureCause {
    #[error("parser failure: {0}")]
    Parser(#[from] SimpleErrorWithPos),
    #[error("type checker failure: {0}")]
    TypeChecker(#[from] TypeCheckError),
}

impl From<TestFailureCause> for TaskExecutionError {
    fn from(value: TestFailureCause) -> Self {
        match value {
            TestFailureCause::Parser(e) => Self::Generic(e),
            TestFailureCause::TypeChecker(e) => Self::TypeCheck(e),
        }

    }
}

pub struct Test;

macro_rules! type_boxes {
    () => {
        vec![]
    };
    (buf $buf:ident) => (
        $buf
    );
    (buf $buf:ident, $x:expr => $k:ident $(,)?) => {
        {
            $buf.push(TypeBox::$k($x));
            type_boxes!(buf $buf)
        }
    };
    (buf $buf:ident, $x:expr => $k:ident, $($xs:expr => $ks:ident),* $(,)?) => {
        {
            $buf.push(TypeBox::$k($x));
            type_boxes!(buf $buf, $($xs => $ks),*)
        }
    };
    ($($x:expr => $k:ident),+ $(,)?) => {
        {
            let mut v = vec![];
            type_boxes!(buf v, $($x => $k),*)
        }
    };
}


impl Test {
    fn evaluated_expressions(src: &str) -> Result<Vec<TypeBox>, Err> {
        Self::evaluated_expressions_with_optimization_preset(src, &NoOptimization)
    }

    fn evaluated_expressions_with_optimization_preset(src: &str, preset: &dyn EachStep) -> Result<Vec<TypeBox>, Err> {
        use origlang_compiler::parser::Parser;
        debug!("src:\n{}", src);
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let acc = Accumulate::default();
        let runtime = Runtime::create(acc);
        let checker = TypeChecker::new();
        let checked = checker.check(root_ast)?;
        let transpiler = TheTranspiler::new(preset);
        let ir = checked.into_ir();
        let ir = transpiler.optimizer().optimize(ir);
        let ir = transpiler.lower(ir);
        let ir = transpiler.optimizer().optimize(ir);
        let ir = transpiler.lower(ir);
        let o = runtime.start(ir);
        println!("{o:?}", o = &o);
        let x = Ok(o.borrow().acc().expect("???"));
        x
    }

    fn ast(src: &str) -> Result<RootAst, Err> {
        use origlang_compiler::parser::Parser;
        debug!("src:\n{}", src);
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        Ok(root_ast)
    }

    #[allow(clippy::unreadable_literal)]
    fn print_literal() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 123456\n")?, type_boxes![123456 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1\nprint 2\n")?, type_boxes![1 => NonCoercedInteger, 2 => NonCoercedInteger]);
        Ok(())
    }

    fn simple_variable_assignment() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint x\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = x\nprint y\n")?, type_boxes![1 => NonCoercedInteger]);
        Ok(())
    }

    fn op_plus() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 1 + 2\n")?, type_boxes![3 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 + x\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x + y\n")?, type_boxes![4 => NonCoercedInteger]);

        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint x + y + z\n")?, type_boxes![6 => NonCoercedInteger]);

        Ok(())
    }

    fn op_minus() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 1 - 2\n")?, type_boxes![-1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 - x\n")?, type_boxes![0 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x - y\n")?, type_boxes![-2 => NonCoercedInteger]);

        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint z - x - y\n")?, type_boxes![0 => NonCoercedInteger]);

        Ok(())
    }

    fn expr_parenthesised() -> Result<(), Err> {
        // paren test
        assert_eq!(Self::evaluated_expressions("print (1)\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 - (2 - 1)\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print (3 - 2) - 1\n")?, type_boxes![0 => NonCoercedInteger]);

        Ok(())
    }

    fn op_multiply() -> Result<(), Err> {
        // multiply test
        assert_eq!(Self::evaluated_expressions("print 3 * 2\n")?, type_boxes![6 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * 2 + 1\n")?, type_boxes![7 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * 2 + 1\n")?, type_boxes![9 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * (2 + 1)\n")?, type_boxes![9 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * (2 + 1)\n")?, type_boxes![7 => NonCoercedInteger]);

        Ok(())
    }

    fn literal_bool() -> Result<(), Err> {
        // boolean literal test
        assert_eq!(Self::evaluated_expressions("print true\n")?, type_boxes![true => Boolean]);
        assert_ne!(Self::evaluated_expressions("print true\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print false\n")?, type_boxes![false => Boolean]);
        assert_ne!(Self::evaluated_expressions("print false\n")?, type_boxes![true => Boolean]);

        Ok(())
    }

    fn run_all() -> Result<(), Err> {
        Self::print_literal().expect("print literal");
        Self::simple_variable_assignment().expect("simple");
        Self::op_plus().expect("plus");
        Self::op_minus().expect("minus");
        Self::expr_parenthesised().expect("paren-expr");
        Self::op_multiply().expect("mult");
        Self::literal_bool().expect("lit");
        Self::test_less().expect("less");
        Self::test_more().expect("more");
        Self::test_spaceship().expect("spaceship");
        Self::test_equality_operator().expect("equality");
        Self::test_if_expression().expect("if");
        Self::test_parenthesised_expression().expect("paren-expr:2");
        Self::test_string_literal().expect("string_literal");
        Self::test_string_concat().expect("string_concat");
        Self::test_unit_literal().expect("unit literal");
        Self::test_coerced_int_literal().expect("int coerced");
        Self::test_infix_op_does_not_cause_panic_by_arithmetic_overflow().expect("overflow?");
        Self::test_overflowed_literal().expect("overflow literal");
        Self::test_variable_reassign().expect("variable reassign");
        Self::test_block_scope().expect("block");
        Self::test_tuple_type().expect("tuple");
        Self::test_comment().expect("comment");
        Self::test_exit().expect("exit");
        Self::test_underscore_discard().expect("underscore_discard");
        Self::test_type_alias().expect("type_alias");
        Self::test_shift().expect("shift");
        Self::test_tuple_destruction().expect("tuple destruction");

        Ok(())
    }

    fn test_less() -> Result<(), Err> {
        // less equal
        assert_eq!(Self::evaluated_expressions("print 1 <= 0\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 2\n")?, type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 <= 0 == 0 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 1 == 1 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 2 == 2 >= 1\n")?, type_boxes![true => Boolean]);

        // less
        assert_eq!(Self::evaluated_expressions("print 1 < 0\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 1\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 2\n")?, type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 < 0 == 0 > 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 1 == 1 > 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 2 == 2 > 1\n")?, type_boxes![true => Boolean]);

        Ok(())
    }

    fn test_more() -> Result<(), Err> {
        // more equal
        assert_eq!(Self::evaluated_expressions("print 1 >= 0\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 2\n")?, type_boxes![false => Boolean]);

        // more equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 >= 0 == 0 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 1 == 1 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 2 == 2 <= 1\n")?, type_boxes![true => Boolean]);

        // more
        assert_eq!(Self::evaluated_expressions("print 1 > 0\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 1\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 2\n")?, type_boxes![false => Boolean]);

        // more reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 > 0 == 0 < 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 1 == 1 < 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 2 == 2 < 1\n")?, type_boxes![true => Boolean]);

        Ok(())
    }

    fn test_spaceship() -> Result<(), Err> {
        // spaceship operator
        assert_eq!(Self::evaluated_expressions("print 1 <=> 0\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1 <=> 1\n")?, type_boxes![0 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1 <=> 2\n")?, type_boxes![-1 => NonCoercedInteger]);

        Ok(())
    }

    fn test_equality_operator() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 42 == 42\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 == 21\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 != 42\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 != 21\n")?, type_boxes![true => Boolean]);
        Ok(())
    }

    fn test_if_expression() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print if true then 1 else 2\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print if true then 1 else 2\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print if false then 1 else 2\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print if false then 1 else 2\n")?, type_boxes![1 => NonCoercedInteger]);
        Ok(())
    }

    fn test_parenthesised_expression() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print (1 == 2)\n")?, type_boxes![false => Boolean]);
        Ok(())
    }

    fn test_string_literal() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print \"123\"\n")?, type_boxes!("123".to_string() => String));
        Ok(())
    }

    fn test_string_concat() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print \"123\" + \"456\"\n")?, type_boxes!("123456".to_string() => String));
        Ok(())
    }

    fn test_unit_literal() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print ()\n")?, vec![TypeBox::Unit]);
        Ok(())
    }

    fn test_coerced_int_literal() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 0i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i16\n")?, type_boxes![0 => Int16]);
        assert_eq!(Self::evaluated_expressions("print 0i32\n")?, type_boxes![0 => Int32]);
        assert_eq!(Self::evaluated_expressions("print 0i64\n")?, type_boxes![0 => Int64]);

        Ok(())
    }

    fn test_infix_op_does_not_cause_panic_by_arithmetic_overflow() -> Result<(), Err> {
        // NOTE: this test covers other coerced int types as well, as long as the `f!` macro handles their match and computation.
        assert_eq!(Self::evaluated_expressions("print 16i8 * 16i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 127i8 + 127i8 + 2i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i8 - (127i8 + 127i8 + 1i8)\n")?, type_boxes![1 => Int8]);
        assert_eq!(Self::evaluated_expressions("print (127i8 + 127i8 + 2i8) / 1i8\n")?, type_boxes![0 => Int8]);

        Ok(())
    }

    fn test_overflowed_literal() -> Result<(), Err> {
        // TODO: test underflow literal
        macro_rules! gen {
            ($t:ty) => {{
                // evaluate
                const MAX: i64 = <$t>::MAX as i64;
                const V: i64 = MAX + 1;
                let src = format!("print {V}{x}", x = stringify!($t));
                let e = Self::evaluated_expressions(src.as_str()).expect_err("this operation should fail");
                if let TestFailureCause::Parser(e) = e {
                    assert_eq!(e.kind, ParserError::OverflowedLiteral {
                        tp: stringify!($t).to_string().into_boxed_str(),
                        min: <$t>::MIN as i64,
                        max: MAX,
                        value: V,
                    });
                } else {
                    panic!("{e:?} is not Parser error: {e}", e = &e);
                }
            }};
        }

        gen!(i8);
        gen!(i16);
        gen!(i32);

        Ok(())
    }

    fn test_variable_reassign() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("var a = 1\na = 2\nprint a\n")?, type_boxes![2 => NonCoercedInteger]);

        Ok(())
    }

    fn test_block_scope() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions(r#"var a = 1
block
var a = 2
print a
end
print a
"#)?, type_boxes![2 => NonCoercedInteger, 1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions(r#"var a = 1
var discard = block
if true then block
var a = 2
print a
()
end else block
var a = 3
print a
()
end
end
"#)?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions(r#"var a = 1
block
    block
        block
            var a = 2
            print a
        end
    end
end
"#)?, type_boxes![2 => NonCoercedInteger]);

        Ok(())
    }

    fn test_tuple_type() -> Result<(), Err> {
        info!("test_tuple");

        assert_eq!(Self::evaluated_expressions(r#"var a = (1, 2)
var b = (3, 4)
print a
"#)?, &[TypeBox::Tuple(DisplayTupleValue { boxes: vec![TypeBox::NonCoercedInteger(1), TypeBox::NonCoercedInteger(2)]})]);

        assert_eq!(Self::ast(r#"var a: (Int32, Int32) = (1i32, 2i32)
"#)?.statement, [Statement::VariableDeclaration {
            pattern: AtomicPattern::Bind(Identifier::new("a".to_string())),
            expression: Expression::Tuple {
                expressions: vec![
                    Expression::IntLiteral {
                        value: 1,
                        suffix: Some("i32".to_string().into_boxed_str()),
                    },
                    Expression::IntLiteral {
                        value: 2,
                        suffix: Some("i32".to_string().into_boxed_str())
                    }
                ],
            },
            type_annotation: Some(
                TypeSignature::Tuple(vec![
                    TypeSignature::Simple(Identifier::new("Int32".to_string())),
                    TypeSignature::Simple(Identifier::new("Int32".to_string())),
                ])
            ),
        }]);
        Ok(())
    }

    fn test_comment() -> Result<(), Err> {
        info!("test_comment");
        assert_eq!(
            Self::ast(r#"//Hello, World!
print 1
"#)?,
            RootAst {
                statement: vec![
                    Statement::Comment {
                        content: Comment {
                            content: "Hello, World!".to_string()
                        },
                    },
                    Statement::Print {
                        expression: Expression::IntLiteral {
                            value: 1,
                            suffix: None,
                        },
                    }
                ]
            }
        );
        assert_eq!(
            Self::ast(r#"print 1
//Hello, World!
"#)?,
            RootAst {
                statement: vec![
                    Statement::Print {
                        expression: Expression::IntLiteral {
                            value: 1,
                            suffix: None,
                        },
                    },
                    Statement::Comment {
                        content: Comment {
                            content: "Hello, World!".to_string()
                        },
                    },
                ]
            }
        );

        Ok(())
    }

    fn test_exit() -> Result<(), Err> {
        assert_eq!(Self::ast("exit\n")?.statement, [ Statement::Exit ]);
        assert_eq!(Self::evaluated_expressions("exit\n")?, []);
        assert_eq!(Self::evaluated_expressions_with_optimization_preset("exit\nprint 1\n", &SimpleOptimization)?, []);

        Ok(())
    }

    fn test_underscore_discard() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("var _ = 1\n")?, []);
        assert_eq!(Self::evaluated_expressions("var a = block\n  print 1\n()\nend\n").expect("FATAL: shouldn't fail"), type_boxes![ 1 => NonCoercedInteger ]);
        assert_eq!(Self::evaluated_expressions("var _ = block\n  print 1\n()\nend\n")?, type_boxes![ 1 => NonCoercedInteger ]);
        assert!(
            matches!(Self::evaluated_expressions("var _ = _\n"),
                Err(
                    TestFailureCause::Parser(
                        SimpleErrorWithPos {
                            kind: ParserError::UnderscoreCanNotBeRightHandExpression,
                            ..
                        }
                    )
                )
            )
        );

        Ok(())
    }

    fn test_type_alias() -> Result<(), Err> {
        assert_eq!(Self::ast("type Ik = Int32\n")?.statement, [ Statement::TypeAliasDeclaration {
            new_name: Identifier::new("Ik".to_string()), replace_with: TypeSignature::Simple(Identifier::new("Int32".to_string())) } ]);
        assert_eq!(Self::evaluated_expressions("type Ik = Int32\n")?, []);
        assert_eq!(Self::evaluated_expressions("type Ik = Int32\nvar t: Ik = 0i32\nprint t\n")?, type_boxes![0 => Int32]);
        Ok(())
    }

    fn test_shift() -> Result<(), Err> {
        assert_eq!(Self::ast("var a = 1 << 2\n").expect("fail").statement, [
            Statement::VariableDeclaration {
                pattern: AtomicPattern::Bind(Identifier::new("a".to_owned())),
                expression: Expression::BinaryOperator {
                    lhs: Box::new(Expression::IntLiteral {
                        value: 1,
                        suffix: None,
                    }),
                    rhs: Box::new(Expression::IntLiteral {
                        value: 2,
                        suffix: None,
                    }),
                    operator: BinaryOperatorKind::ShiftLeft,
                },
                type_annotation: None,
            }
        ]);

        assert_eq!(Self::evaluated_expressions("var t = 1i32 << 2i32\nprint t\n")?, type_boxes![4 => Int32]);
        assert_eq!(Self::ast("var a = 4 >> 2\n").expect("fail").statement, [
            Statement::VariableDeclaration {
                pattern: AtomicPattern::Bind(Identifier::new("a".to_owned())),
                expression: Expression::BinaryOperator {
                    lhs: Box::new(Expression::IntLiteral {
                        value: 4,
                        suffix: None,
                    }),
                    rhs: Box::new(Expression::IntLiteral {
                        value: 2,
                        suffix: None,
                    }),
                    operator: BinaryOperatorKind::ShiftRight,
                },
                type_annotation: None,
            }
        ]);

        assert_eq!(Self::evaluated_expressions("var t = 4i32 >> 2i32\nprint t\n")?, type_boxes![1 => Int32]);
        Ok(())
    }

    fn test_tuple_destruction() -> Result<(), Err> {
        // literal
        assert_eq!(Self::evaluated_expressions("var (a, b) = (1i32, 2i32)\nprint a\nprint b\n")?, type_boxes![1 => Int32, 2 => Int32]);
        assert_eq!(
            Self::evaluated_expressions("var (a, b, c, d, e, f, g, h) = (1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32)\nprint a\nprint b\nprint c\nprint d\nprint e\nprint f\nprint g\nprint h\n")?,
            type_boxes![1 => Int32, 2 => Int32, 3 => Int32, 4 => Int32, 5 => Int32, 6 => Int32, 7 => Int32, 8 => Int32]
        );
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, 2i32)\nprint a\n")?, type_boxes![1 => Int32]);
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, block\nprint 2i32\n()\nend)")?, type_boxes![2 => Int32]);

        // literal (nested)
        assert_eq!(Self::evaluated_expressions("var (a, (b, c)) = (1i32, (2i32, 3i32))\nprint a\nprint b\nprint c\n")?, type_boxes![1 => Int32, 2 => Int32, 3 => Int32]);
        assert_eq!(Self::evaluated_expressions("var (a, (b, _)) = (1i32, (2i32, 3i32))\nprint a\nprint b\n")?, type_boxes![1 => Int32, 2 => Int32]);

        // var
        assert_eq!(Self::evaluated_expressions("var z = (1i32, 2i32)\nvar (a, b) = z\nprint a\nprint b")?, type_boxes![1 => Int32, 2 => Int32]);
        assert_eq!(Self::evaluated_expressions("var z = (1i32, 2i32, 3i32)\nvar (a, b, c) = z\nprint a\nprint b\nprint c")?, type_boxes![1 => Int32, 2 => Int32, 3 => Int32]);

        // var (nested)
        assert_eq!(Self::evaluated_expressions("var z = (1i32, (2i32, 3i32))\nvar (a, (b, c)) = z\nprint a\nprint b\nprint c")?, type_boxes![1 => Int32, 2 => Int32, 3 => Int32], "var nest");
        assert_eq!(Self::evaluated_expressions("var z = (1i32, (2i32, 3i32))\nvar (a, y) = z\nvar (b, c) = y\nprint a\nprint b\nprint c")?, type_boxes![1 => Int32, 2 => Int32, 3 => Int32], "double-var nest");

        Ok(())
    }
}

impl Task for Test {
    type Environment = ();
    type Error = TestFailureCause;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        eprintln!("start");
        Self::run_all()?;
        eprintln!("end");
        Ok(())
    }
}
