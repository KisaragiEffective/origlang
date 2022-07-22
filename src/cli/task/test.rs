use crate::cli::task::Task;
use crate::Runtime;

pub struct Test;

impl Test {
    fn evaluated_expressions(src: &str) -> Result<Vec<i32>, String> {
        use crate::parser::Parser;
        let source = src;
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let runtime = Runtime::create();
        Ok(runtime.yield_all_evaluated_expressions(&root_ast))
    }

    #[allow(clippy::unreadable_literal)]
    fn expression_equality_test() -> Result<(), String> {
        assert_eq!(Self::evaluated_expressions("123456\n")?, vec![123456]);
        assert_eq!(Self::evaluated_expressions("1\n2\n")?, vec![1, 2]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nx\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = x\ny\n")?, vec![1]);
        // plus operator test (binary)
        assert_eq!(Self::evaluated_expressions("1 + 2\n")?, vec![3]);
        assert_eq!(Self::evaluated_expressions("var x = 1\n1 + x\n")?, vec![2]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nx + y\n")?, vec![4]);

        // plus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nx + y + z\n")?, vec![6]);

        // minus operator test (binary)
        assert_eq!(Self::evaluated_expressions("1 - 2\n")?, vec![-1]);
        assert_eq!(Self::evaluated_expressions("var x = 1\n1 - x\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 3\nx - y\n")?, vec![-2]);

        // minus operator test (more than twice)
        assert_eq!(Self::evaluated_expressions("var x = 1\nvar y = 2\nvar z = 3\nz - x - y\n")?, vec![0]);

        // paren test
        assert_eq!(Self::evaluated_expressions("(1)\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("3 - (2 - 1)\n")?, vec![2]);
        assert_eq!(Self::evaluated_expressions("(3 - 2) - 1\n")?, vec![0]);

        // multiply test
        assert_eq!(Self::evaluated_expressions("3 * 2\n")?, vec![6]);
        assert_eq!(Self::evaluated_expressions("3 * 2 + 1\n")?, vec![7]);
        assert_ne!(Self::evaluated_expressions("3 * 2 + 1\n")?, vec![9]);
        assert_eq!(Self::evaluated_expressions("3 * (2 + 1)\n")?, vec![9]);
        assert_ne!(Self::evaluated_expressions("3 * (2 + 1)\n")?, vec![7]);

        // pesudo-boolean literal test
        assert_eq!(Self::evaluated_expressions("true\n")?, vec![1]);
        assert_ne!(Self::evaluated_expressions("true\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("false\n")?, vec![0]);
        assert_ne!(Self::evaluated_expressions("false\n")?, vec![1]);

        Self::test_comparison_operator()?;
        Self::test_equality_operator()?;
        Ok(())
    }

    fn test_comparison_operator() -> Result<(), String> {
        // less equal
        assert_eq!(Self::evaluated_expressions("1 <= 0\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("1 <= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 <= 2\n")?, vec![1]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 <= 0 == 0 >= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 <= 1 == 1 >= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 <= 2 == 2 >= 1\n")?, vec![1]);

        // less
        assert_eq!(Self::evaluated_expressions("1 < 0\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("1 < 1\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("1 < 2\n")?, vec![1]);

        // less equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 < 0 == 0 > 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 < 1 == 1 > 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 < 2 == 2 > 1\n")?, vec![1]);

        // more equal
        assert_eq!(Self::evaluated_expressions("1 >= 0\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 >= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 >= 2\n")?, vec![0]);

        // more equal reflexibility
        assert_eq!(Self::evaluated_expressions("1 >= 0 == 0 <= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 >= 1 == 1 <= 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 >= 2 == 2 <= 1\n")?, vec![1]);

        // more
        assert_eq!(Self::evaluated_expressions("1 > 0\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 > 1\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("1 > 2\n")?, vec![0]);

        // more reflexibility
        assert_eq!(Self::evaluated_expressions("1 > 0 == 0 < 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 > 1 == 1 < 1\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 > 2 == 2 < 1\n")?, vec![1]);

        // spaceship operator
        assert_eq!(Self::evaluated_expressions("1 <=> 0\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("1 <=> 1\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("1 <=> 2\n")?, vec![-1]);

        Ok(())
    }

    fn test_equality_operator() -> Result<(), String> {
        assert_eq!(Self::evaluated_expressions("42 == 42\n")?, vec![1]);
        assert_eq!(Self::evaluated_expressions("42 == 21\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("42 != 42\n")?, vec![0]);
        assert_eq!(Self::evaluated_expressions("42 != 21\n")?, vec![1]);
        Ok(())
    }
}

impl Task for Test {
    type Environment = ();
    type Error = String;

    fn execute(&self, _environment: Self::Environment) -> Result<(), Self::Error> {
        eprintln!("start");
        Self::expression_equality_test()?;
        eprintln!("end");
        Ok(())
    }
}
