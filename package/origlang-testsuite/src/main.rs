#![deny(dead_code)]
#![allow(clippy::unnecessary_wraps)]

fn main() {
    env_logger::init();
    eprintln!("start");
    Test::run_all();
    eprintln!("end");
}

use log::{debug, info};
use thiserror::Error;
use origlang_runtime::{Accumulate, DisplayTupleValue, Runtime, TypeBox};
use origlang_ast::{AtomicPattern, Comment, Identifier, RootAst, Statement, TypeSignature};
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_parser::error::{ParserError, ParserErrorInner};
use origlang_typecheck::type_check::error::TypeCheckError;
use origlang_typecheck::type_check::TypeChecker;
use origlang_ir::IntoVerbatimSequencedIR;
use origlang_ir_optimizer::lower::{EachStep, LowerStep, TheTranspiler};
use origlang_ir_optimizer::preset::NoOptimization;
use origlang_source_span::SourcePosition;

type Err = TestFailureCause;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum TestFailureCause {
    #[error("parser failure: {0}")]
    Parser(#[from] ParserError),
    #[error("type checker failure: {0}")]
    TypeChecker(#[from] TypeCheckError),
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
        use origlang_parser::parser::Parser;
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
        let o = runtime.start(&ir);
        println!("{o:?}", o = &o);
        let x = Ok(o.borrow().acc().expect("???"));
        x
    }

    fn ast(src: &str) -> Result<RootAst, Err> {
        use origlang_parser::parser::Parser;
        debug!("src:\n{}", src);
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        Ok(root_ast)
    }
    
    fn print_literal() {
        assert_eq!(Self::evaluated_expressions("print 123456\n").expect("properly parsed and typed"), type_boxes![123456 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1\nprint 2\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger, 2 => NonCoercedInteger]);
    }

    fn simple_variable_assignment() {
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint x\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = x\nprint y\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
    }

    fn op_plus() {
        assert_eq!(Self::evaluated_expressions("print 1 + 2\n").expect("properly parsed and typed"), type_boxes![3 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 + x\n").expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x + y\n").expect("properly parsed and typed"), type_boxes![4 => NonCoercedInteger]);

        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint x + y + z\n").expect("properly parsed and typed"), type_boxes![6 => NonCoercedInteger]);
    }

    fn op_minus() {
        assert_eq!(Self::evaluated_expressions("print 1 - 2\n").expect("properly parsed and typed"), type_boxes![-1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 - x\n").expect("properly parsed and typed"), type_boxes![0 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x - y\n").expect("properly parsed and typed"), type_boxes![-2 => NonCoercedInteger]);

        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint z - x - y\n").expect("properly parsed and typed"), type_boxes![0 => NonCoercedInteger]);
    }

    fn expr_parenthesised() {
        // paren test
        assert_eq!(Self::evaluated_expressions("print (1)\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 - (2 - 1)\n").expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print (3 - 2) - 1\n").expect("properly parsed and typed"), type_boxes![0 => NonCoercedInteger]);
    }

    fn op_multiply() {
        // multiply test
        assert_eq!(Self::evaluated_expressions("print 3 * 2\n").expect("properly parsed and typed"), type_boxes![6 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * 2 + 1\n").expect("properly parsed and typed"), type_boxes![7 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * 2 + 1\n").expect("properly parsed and typed"), type_boxes![9 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * (2 + 1)\n").expect("properly parsed and typed"), type_boxes![9 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * (2 + 1)\n").expect("properly parsed and typed"), type_boxes![7 => NonCoercedInteger]);
    }

    fn literal_bool() {
        // boolean literal test
        assert_eq!(Self::evaluated_expressions("print true\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_ne!(Self::evaluated_expressions("print true\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print false\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_ne!(Self::evaluated_expressions("print false\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        
    }

    fn run_all() {
        Self::print_literal();
        Self::simple_variable_assignment();
        Self::op_plus();
        Self::op_minus();
        Self::expr_parenthesised();
        Self::op_multiply();
        Self::literal_bool();
        Self::test_less();
        Self::test_more();
        Self::test_spaceship();
        Self::test_equality_operator();
        Self::test_if_expression();
        Self::test_parenthesised_expression();
        Self::test_string_literal();
        Self::test_string_concat();
        Self::test_unit_literal();
        Self::test_coerced_int_literal();
        Self::test_infix_op_does_not_cause_panic_by_arithmetic_overflow();
        Self::test_overflowed_literal();
        Self::test_variable_reassign();
        Self::test_block_scope();
        Self::test_tuple_type();
        Self::test_comment();
        Self::test_exit();
        Self::test_underscore_discard();
        Self::test_type_alias();
        Self::test_shift();
        Self::trigger_confusion();
        Self::test_tuple_destruction();

        
    }

    fn test_less() {
        // less equal
        assert_eq!(Self::evaluated_expressions("print 1 <= 0\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 2\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 <= 0 == 0 >= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 1 == 1 >= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 <= 2 == 2 >= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        // less
        assert_eq!(Self::evaluated_expressions("print 1 < 0\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 1\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 2\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 < 0 == 0 > 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 1 == 1 > 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 < 2 == 2 > 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
    }

    fn test_more() {
        // more equal
        assert_eq!(Self::evaluated_expressions("print 1 >= 0\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 2\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);

        // more equal reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 >= 0 == 0 <= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 1 == 1 <= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 >= 2 == 2 <= 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        // more
        assert_eq!(Self::evaluated_expressions("print 1 > 0\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 1\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 2\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);

        // more reflexibility
        assert_eq!(Self::evaluated_expressions("print 1 > 0 == 0 < 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 1 == 1 < 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 1 > 2 == 2 < 1\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);

        
    }

    fn test_spaceship() {
        // spaceship operator
        assert_eq!(Self::evaluated_expressions("print 1 <=> 0\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1 <=> 1\n").expect("properly parsed and typed"), type_boxes![0 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1 <=> 2\n").expect("properly parsed and typed"), type_boxes![-1 => NonCoercedInteger]);

        
    }

    fn test_equality_operator() {
        assert_eq!(Self::evaluated_expressions("print 42 == 42\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 == 21\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 != 42\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print 42 != 21\n").expect("properly parsed and typed"), type_boxes![true => Boolean]);
        
    }

    fn test_if_expression() {
        assert_eq!(Self::evaluated_expressions("print if true then 1 else 2\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print if true then 1 else 2\n").expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print if false then 1 else 2\n").expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print if false then 1 else 2\n").expect("properly parsed and typed"), type_boxes![1 => NonCoercedInteger]);
        
    }

    fn test_parenthesised_expression() {
        assert_eq!(Self::evaluated_expressions("print (1 == 2)\n").expect("properly parsed and typed"), type_boxes![false => Boolean]);
        
    }

    fn test_string_literal() {
        assert_eq!(Self::evaluated_expressions("print \"123\"\n").expect("properly parsed and typed"), type_boxes!("123".to_string() => String));
        
    }

    fn test_string_concat() {
        assert_eq!(Self::evaluated_expressions("print \"123\" + \"456\"\n").expect("properly parsed and typed"), type_boxes!("123456".to_string() => String));
        
    }

    fn test_unit_literal() {
        assert_eq!(Self::evaluated_expressions("print ()\n").expect("properly parsed and typed"), vec![TypeBox::Unit]);
        assert_eq!(Self::evaluated_expressions("print ((((()))))\n").expect("properly parsed and typed"), vec![TypeBox::Unit]);
    }

    fn test_coerced_int_literal() {
        assert_eq!(Self::evaluated_expressions("print 0i8\n").expect("properly parsed and typed"), type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i16\n").expect("properly parsed and typed"), type_boxes![0 => Int16]);
        assert_eq!(Self::evaluated_expressions("print 0i32\n").expect("properly parsed and typed"), type_boxes![0 => Int32]);
        assert_eq!(Self::evaluated_expressions("print 0i64\n").expect("properly parsed and typed"), type_boxes![0 => Int64]);

        
    }

    fn test_infix_op_does_not_cause_panic_by_arithmetic_overflow() {
        // NOTE: this test covers other coerced int types as well, as long as the `f!` macro handles their match and computation.
        assert_eq!(Self::evaluated_expressions("print 16i8 * 16i8\n").expect("properly parsed and typed"), type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 127i8 + 127i8 + 2i8\n").expect("properly parsed and typed"), type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i8 - (127i8 + 127i8 + 1i8)\n").expect("properly parsed and typed"), type_boxes![1 => Int8]);
        assert_eq!(Self::evaluated_expressions("print (127i8 + 127i8 + 2i8) / 1i8\n").expect("properly parsed and typed"), type_boxes![0 => Int8]);

        
    }

    fn test_overflowed_literal() {
        // TODO: test underflow literal
        macro_rules! gen {
            ($t:ty) => {{
                // evaluate
                const MAX: i64 = <$t>::MAX as i64;
                const V: i64 = MAX + 1;
                let src = format!("print {V}{x}", x = stringify!($t));
                let e = Self::evaluated_expressions(src.as_str()).expect_err("this operation should fail");
                if let TestFailureCause::Parser(e) = e {
                    assert_eq!(e.kind(), &ParserErrorInner::OverflowedLiteral {
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

        
    }

    fn test_variable_reassign() {
        assert_eq!(Self::evaluated_expressions("var a = 1\na = 2\nprint a\n").expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);

        
    }

    fn test_block_scope() {
        assert_eq!(Self::evaluated_expressions(r#"var a = 1
block
var a = 2
print a
end
print a
"#).expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger, 1 => NonCoercedInteger]);
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
"#).expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions(r#"var a = 1
block
    block
        block
            var a = 2
            print a
        end
    end
end
"#).expect("properly parsed and typed"), type_boxes![2 => NonCoercedInteger]);

        
    }

    fn test_tuple_type() {
        info!("test_tuple");

        assert_eq!(Self::evaluated_expressions(r#"var a = (1, 2)
var b = (3, 4)
print a
"#).expect("properly parsed and typed"), &[TypeBox::Tuple(DisplayTupleValue { boxes: vec![TypeBox::NonCoercedInteger(1), TypeBox::NonCoercedInteger(2)]})]);

        assert_eq!(Self::ast(r#"var a: (Int32, Int32) = (1i32, 2i32)
"#).expect("properly parsed").statement, [Statement::VariableDeclaration {
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
        
    }

    fn test_comment() {
        info!("test_comment");
        assert_eq!(
            Self::ast(r#"//Hello, World!
print 1
"#).expect("properly parsed"),
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
"#).expect("properly parsed"),
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

        
    }

    fn test_exit() {
        assert_eq!(Self::ast("exit\n").expect("properly parsed").statement, [ Statement::Exit ]);
        assert_eq!(Self::evaluated_expressions("exit\n").expect("properly parsed and typed"), []);
    }

    fn test_underscore_discard() {
        assert_eq!(Self::evaluated_expressions("var _ = 1\n").expect("properly parsed and typed"), []);
        assert_eq!(Self::evaluated_expressions("var a = block\n  print 1\n()\nend\n").expect("FATAL: shouldn't fail"), type_boxes![ 1 => NonCoercedInteger ]);
        assert_eq!(Self::evaluated_expressions("var _ = block\n  print 1\n()\nend\n").expect("properly parsed and typed"), type_boxes![ 1 => NonCoercedInteger ]);
        assert_eq!(
            Self::evaluated_expressions("var _ = _\n"),
            Err(
                TestFailureCause::Parser(
                    ParserError::new(ParserErrorInner::UnderscoreCanNotBeRightHandExpression, SourcePosition::try_new((1, 9)).unwrap())
                )
            )

        );

        
    }

    fn test_type_alias() {
        assert_eq!(Self::ast("type Ik = Int32\n").expect("properly parsed").statement, [ Statement::TypeAliasDeclaration {
            new_name: Identifier::new("Ik".to_string()), replace_with: TypeSignature::Simple(Identifier::new("Int32".to_string())) } ]);
        assert_eq!(Self::evaluated_expressions("type Ik = Int32\n").expect("properly parsed and typed"), []);
        assert_eq!(Self::evaluated_expressions("type Ik = Int32\nvar t: Ik = 0i32\nprint t\n").expect("properly parsed and typed"), type_boxes![0 => Int32]);
    }

    fn test_shift() {
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

        assert_eq!(Self::evaluated_expressions("var t = 1i32 << 2i32\nprint t\n").expect("properly parsed and typed"), type_boxes![4 => Int32]);
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

        assert_eq!(Self::evaluated_expressions("var t = 4i32 >> 2i32\nprint t\n").expect("properly parsed and typed"), type_boxes![1 => Int32]);
    }

    fn trigger_confusion() {
        let mut errors = vec![];

        match Self::evaluated_expressions("var _ = ()") {
            Ok(e) => assert_eq!(e, type_boxes![]),
            Err(e) => { errors.push(e) }
        }

        //*
        match Self::evaluated_expressions("var _ = ((), ())") {
            Ok(e) => assert_eq!(e, type_boxes![]),
            Err(e) => { errors.push(e) }
        }

        match Self::evaluated_expressions("var (_, _) = ((), ())") {
            Ok(e) => assert_eq!(e, type_boxes![]),
            Err(e) => { errors.push(e) }
        }

        match Self::evaluated_expressions("var (_, _) = (1, 2)") {
            Ok(e) => assert_eq!(e, type_boxes![]),
            Err(e) => { errors.push(e) }

        }
        
        //*/
        
        if !errors.is_empty() {
            panic!("ouch!: {errors:#?}");
        }
    }
    
    fn test_tuple_destruction() {
        // literal
        assert_eq!(Self::evaluated_expressions("var (a, b) = (1i32, 2i32)\nprint a\nprint b\n").expect("properly parsed and typed"), type_boxes![1 => Int32, 2 => Int32]);
        assert_eq!(
            Self::evaluated_expressions("var (a, b, c, d, e, f, g, h) = (1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32)\nprint a\nprint b\nprint c\nprint d\nprint e\nprint f\nprint g\nprint h\n").expect("properly parsed and typed"),
            type_boxes![1 => Int32, 2 => Int32, 3 => Int32, 4 => Int32, 5 => Int32, 6 => Int32, 7 => Int32, 8 => Int32]
        );
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, 2i32)\nprint a\n").expect("properly parsed and typed"), type_boxes![1 => Int32]);
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, ())").expect("properly parsed and typed"), type_boxes![]);
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, block\n()\nend)").expect("properly parsed and typed"), type_boxes![]);
        assert_eq!(Self::evaluated_expressions("var (a, _) = (1i32, block\nprint 2i32\n()\nend)").expect("properly parsed and typed"), type_boxes![2 => Int32]);
        assert_eq!(Self::evaluated_expressions("var (a, _) = (46i32, if true then 178i32 else 251i32)\nprint a\n").expect("properly parsed and typed"), type_boxes![46 => Int32]);

        // literal (nested)
        assert_eq!(Self::evaluated_expressions("var (a, (b, c)) = (1i32, (2i32, 3i32))\nprint a\nprint b\nprint c\n").expect("properly parsed and typed"), type_boxes![1 => Int32, 2 => Int32, 3 => Int32]);
        assert_eq!(Self::evaluated_expressions("var (a, (b, _)) = (1i32, (2i32, 3i32))\nprint a\nprint b\n").expect("properly parsed and typed"), type_boxes![1 => Int32, 2 => Int32]);

        // var
        assert_eq!(Self::evaluated_expressions("var z = (1i32, 2i32)\nvar (a, b) = z\nprint a\nprint b").expect("properly parsed and typed"), type_boxes![1 => Int32, 2 => Int32]);
        assert_eq!(Self::evaluated_expressions("var z = (1i32, 2i32, 3i32)\nvar (a, b, c) = z\nprint a\nprint b\nprint c").expect("properly parsed and typed"), type_boxes![1 => Int32, 2 => Int32, 3 => Int32]);

        // var (nested)
        assert_eq!(Self::evaluated_expressions("var z = (1i32, (21i32, 30i32))\nvar (a, (b, c)) = z\nprint a\nprint b\nprint c").expect("properly parsed and typed"), type_boxes![1 => Int32, 21 => Int32, 30 => Int32], "var nest");
        assert_eq!(Self::evaluated_expressions("var z = (1i32, (25i32, 38i32))\nvar (a, y) = z\nvar (b, c) = y\nprint a\nprint b\nprint c").expect("properly parsed and typed"), type_boxes![1 => Int32, 25 => Int32, 38 => Int32], "double-var nest");
    }
}
