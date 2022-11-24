use crate::cli::task::Task;
use crate::parser::SimpleErrorWithPos;
use crate::runtime::{Runtime, TypeBox};

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
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let runtime = Runtime::create();
        Ok(runtime.yield_all_evaluated_expressions(&root_ast))
    }

    #[allow(clippy::unreadable_literal)]
    fn expression_equality_test() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("123456\n")?, type_boxes![123456 => Integer]);
        assert_eq!(Self::evaluated_expressions("1\n2\n")?, type_boxes![1 => Integer, 2 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nx\n")?, type_boxes![1 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = x\ny\n")?, type_boxes![1 => Integer]);
        // plus operator test (binary)
        assert_eq!(Self::evaluated_expressions("1 + 2\n")?, type_boxes![3 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\n1 + x\n")?, type_boxes![2 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nx + y\n")?, type_boxes![4 => Integer]);

        // plus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nx + y + z\n")?, type_boxes![6 => Integer]);

        // minus operator test (binary)
        assert_eq!(Self::evaluated_expressions("1 - 2\n")?, type_boxes![-1 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\n1 - x\n")?, type_boxes![0 => Integer]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nx - y\n")?, type_boxes![-2 => Integer]);

        // minus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nz - x - y\n")?, type_boxes![0 => Integer]);

        // paren test
        assert_eq!(Self::evaluated_expressions("(1)\n")?, type_boxes![1 => Integer]);
        assert_eq!(Self::evaluated_expressions("3 - (2 - 1)\n")?, type_boxes![2 => Integer]);
        assert_eq!(Self::evaluated_expressions("(3 - 2) - 1\n")?, type_boxes![0 => Integer]);

        // multiply test
        assert_eq!(Self::evaluated_expressions("3 * 2\n")?, type_boxes![6 => Integer]);
        assert_eq!(Self::evaluated_expressions("3 * 2 + 1\n")?, type_boxes![7 => Integer]);
        assert_ne!(Self::evaluated_expressions("3 * 2 + 1\n")?, type_boxes![9 => Integer]);
        assert_eq!(Self::evaluated_expressions("3 * (2 + 1)\n")?, type_boxes![9 => Integer]);
        assert_ne!(Self::evaluated_expressions("3 * (2 + 1)\n")?, type_boxes![7 => Integer]);

        // boolean literal test
        assert_eq!(Self::evaluated_expressions("true\n")?, type_boxes![true => Boolean]);
        assert_ne!(Self::evaluated_expressions("true\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("false\n")?, type_boxes![false => Boolean]);
        assert_ne!(Self::evaluated_expressions("false\n")?, type_boxes![true => Boolean]);

        Self::test_comparison_operator()?;
        Self::test_equality_operator()?;
        Self::test_if_expression()?;
        Self::test_parenthesised_expression()?;
        Self::test_string_literal()?;
        Self::test_string_concat()?;
        Ok(())
    }

    fn test_comparison_operator() -> Result<(), Err> {
        // less equal
        assert_eq!(Self::evaluated_expressions("1 <= 0\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 <= 2\n")?, type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 <= 0 == 0 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 <= 1 == 1 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 <= 2 == 2 >= 1\n")?, type_boxes![true => Boolean]);

        // less
        assert_eq!(Self::evaluated_expressions("1 < 0\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 < 1\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 < 2\n")?, type_boxes![true => Boolean]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 < 0 == 0 > 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 < 1 == 1 > 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 < 2 == 2 > 1\n")?, type_boxes![true => Boolean]);

        // more equal
        assert_eq!(Self::evaluated_expressions("1 >= 0\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 >= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 >= 2\n")?, type_boxes![false => Boolean]);

        // more equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 >= 0 == 0 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 >= 1 == 1 <= 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 >= 2 == 2 <= 1\n")?, type_boxes![true => Boolean]);

        // more
        assert_eq!(Self::evaluated_expressions("1 > 0\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 > 1\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 > 2\n")?, type_boxes![false => Boolean]);

        // more reflexibility
        assert_eq!(Self::evaluated_expressions("1 > 0 == 0 < 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 > 1 == 1 < 1\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("1 > 2 == 2 < 1\n")?, type_boxes![true => Boolean]);

        // spaceship operator
        assert_eq!(Self::evaluated_expressions("1 <=> 0\n")?, type_boxes![1 => Integer]);
        assert_eq!(Self::evaluated_expressions("1 <=> 1\n")?, type_boxes![0 => Integer]);
        assert_eq!(Self::evaluated_expressions("1 <=> 2\n")?, type_boxes![-1 => Integer]);

        Ok(())
    }

    fn test_equality_operator() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("42 == 42\n")?, type_boxes![true => Boolean]);
        assert_eq!(Self::evaluated_expressions("42 == 21\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("42 != 42\n")?, type_boxes![false => Boolean]);
        assert_eq!(Self::evaluated_expressions("42 != 21\n")?, type_boxes![true => Boolean]);
        Ok(())
    }

    fn test_if_expression() -> Result<(), Err> {
        assert_eq!(Self::evaluated_expressions("if true then 1 else 2\n")?, type_boxes![1 => Integer]);
        assert_ne!(Self::evaluated_expressions("if true then 1 else 2\n")?, type_boxes![2 => Integer]);
        assert_eq!(Self::evaluated_expressions("if false then 1 else 2\n")?, type_boxes![2 => Integer]);
        assert_ne!(Self::evaluated_expressions("if false then 1 else 2\n")?, type_boxes![1 => Integer]);
        Ok(())
    }

    fn test_parenthesised_expression() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("(1 == 2)\n")?, type_boxes![false => Boolean]);
        Ok(())
    }

    fn test_string_literal() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("\"123\"\n")?, type_boxes!("123".to_string() => String));
        Ok(())
    }

    fn test_string_concat() -> Result<(), SimpleErrorWithPos> {
        assert_eq!(Self::evaluated_expressions("\"123\" + \"456\"")?, type_boxes!("123456".to_string() => String));
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
