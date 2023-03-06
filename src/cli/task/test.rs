#![forbid(dead_code)]
#![allow(clippy::unnecessary_wraps)]

use log::{debug, info};
use crate::cli::task::Task;
use crate::parser::{ParserError, SimpleErrorWithPos};
use crate::runtime::{Runtime, TypeBox, Accumulate, DisplayTuple};

type Err = SimpleErrorWithPos;

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
            $buf.push($crate::runtime::TypeBox::$k($x));
            type_boxes!(buf $buf)
        }
    };
    (buf $buf:ident, $x:expr => $k:ident, $($xs:expr => $ks:ident),* $(,)?) => {
        {
            $buf.push($crate::runtime::TypeBox::$k($x));
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
        use crate::parser::Parser;
        debug!("src:\n{}", src);
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let acc = Accumulate::default();
        let runtime = Runtime::create(acc);
        let o = runtime.what_will_happen(root_ast.clone());
        println!("{o:?}", o = &o);
        let o = runtime.execute(root_ast);
        let x = Ok(o.borrow().acc().expect("???"));
        x
    }

    #[allow(clippy::unreadable_literal)]
    fn expression_equality_test() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("print 123456\n")?, type_boxes![123456 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 1\nprint 2\n")?, type_boxes![1 => NonCoercedInteger, 2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint x\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = x\nprint y\n")?, type_boxes![1 => NonCoercedInteger]);
        // plus operator test (binary)
        assert_eq!(Self::evaluated_expressions("print 1 + 2\n")?, type_boxes![3 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 + x\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x + y\n")?, type_boxes![4 => NonCoercedInteger]);

        // plus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint x + y + z\n")?, type_boxes![6 => NonCoercedInteger]);

        // minus operator test (binary)
        assert_eq!(Self::evaluated_expressions("print 1 - 2\n")?, type_boxes![-1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nprint 1 - x\n")?, type_boxes![0 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nprint x - y\n")?, type_boxes![-2 => NonCoercedInteger]);

        // minus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nprint z - x - y\n")?, type_boxes![0 => NonCoercedInteger]);

        // paren test
        assert_eq!(Self::evaluated_expressions("print (1)\n")?, type_boxes![1 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 - (2 - 1)\n")?, type_boxes![2 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print (3 - 2) - 1\n")?, type_boxes![0 => NonCoercedInteger]);

        // multiply test
        assert_eq!(Self::evaluated_expressions("print 3 * 2\n")?, type_boxes![6 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * 2 + 1\n")?, type_boxes![7 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * 2 + 1\n")?, type_boxes![9 => NonCoercedInteger]);
        assert_eq!(Self::evaluated_expressions("print 3 * (2 + 1)\n")?, type_boxes![9 => NonCoercedInteger]);
        assert_ne!(Self::evaluated_expressions("print 3 * (2 + 1)\n")?, type_boxes![7 => NonCoercedInteger]);

        // boolean literal test
        assert_eq!(Self::evaluated_expressions("print true\n")?, type_boxes![true => Boolean]);
        assert_ne!(Self::evaluated_expressions("print true\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("print false\n")?, type_boxes![false => Boolean]);
        assert_ne!(Self::evaluated_expressions("print false\n")?, type_boxes![true => Boolean]);

        Self::test_comparison_operator()?;
        Self::test_equality_operator()?;
        Self::test_if_expression()?;
        Self::test_parenthesised_expression()?;
        Self::test_string_literal()?;
        Self::test_string_concat()?;
        Self::test_unit_literal()?;
        Self::test_coerced_int_literal()?;
        Self::test_infix_op_does_not_cause_panic_by_arithmetic_overflow()?;
        Self::test_overflowed_literal()?;
        Self::test_variable_reassign()?;
        Self::test_block_scope()?;
        Self::test_tuple_type()?;

        Ok(())
    }

    fn test_comparison_operator() -> Result<(), Err> {
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

    fn test_parenthesised_expression() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("print (1 == 2)\n")?, type_boxes![false => Boolean]);
        Ok(())
    }

    fn test_string_literal() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("print \"123\"\n")?, type_boxes!("123".to_string() => String));
        Ok(())
    }

    fn test_string_concat() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("print \"123\" + \"456\"\n")?, type_boxes!("123456".to_string() => String));
        Ok(())
    }

    fn test_unit_literal() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("print ()\n")?, type_boxes![() => Unit]);
        Ok(())
    }

    fn test_coerced_int_literal() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("print 0i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i16\n")?, type_boxes![0 => Int16]);
        assert_eq!(Self::evaluated_expressions("print 0i32\n")?, type_boxes![0 => Int32]);
        assert_eq!(Self::evaluated_expressions("print 0i64\n")?, type_boxes![0 => Int64]);

        Ok(())
    }

    fn test_infix_op_does_not_cause_panic_by_arithmetic_overflow() -> Result<(), SimpleErrorWithPos> {
        // NOTE: this test covers other coerced int types as well, as long as the `f!` macro handles their match and computation.
        assert_eq!(Self::evaluated_expressions("print 16i8 * 16i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 127i8 + 127i8 + 2i8\n")?, type_boxes![0 => Int8]);
        assert_eq!(Self::evaluated_expressions("print 0i8 - (127i8 + 127i8 + 1i8)\n")?, type_boxes![1 => Int8]);
        assert_eq!(Self::evaluated_expressions("print (127i8 + 127i8 + 2i8) / 1i8\n")?, type_boxes![0 => Int8]);

        Ok(())
    }

    fn test_overflowed_literal() -> Result<(), SimpleErrorWithPos> {
        // TODO: test underflow literal
        macro_rules! gen {
            ($t:ty) => {{
                // evaluate
                const MAX: i64 = <$t>::MAX as i64;
                const V: i64 = MAX + 1;
                let src = format!("print {V}{x}", x = stringify!($t));
                let e = Self::evaluated_expressions(src.as_str()).expect_err("this operation should fail");
                assert_eq!(e.kind, ParserError::OverflowedLiteral {
                    tp: stringify!($t).to_string().into_boxed_str(),
                    min: <$t>::MIN as i64,
                    max: MAX,
                    value: V,
                });
            }};
        }

        gen!(i8);
        gen!(i16);
        gen!(i32);

        Ok(())
    }

    fn test_variable_reassign() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("var a = 1\na = 2\nprint a\n")?, type_boxes![2 => NonCoercedInteger]);

        Ok(())
    }

    fn test_block_scope() -> Result<(), SimpleErrorWithPos> {
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

    fn test_tuple_type() -> Result<(), SimpleErrorWithPos> {
        info!("test_tuple");

        assert_eq!(Self::evaluated_expressions(r#"var a = (1, 2)
var b = (3, 4)
print a
"#)?, &[TypeBox::Tuple(DisplayTuple { boxes: vec![TypeBox::NonCoercedInteger(1), TypeBox::NonCoercedInteger(2)]})]);

        Ok(())
    }
}

impl Task for Test {
    type Environment = ();
    type Error = SimpleErrorWithPos;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        eprintln!("start");
        Self::expression_equality_test()?;
        eprintln!("end");
        Ok(())
    }
}
