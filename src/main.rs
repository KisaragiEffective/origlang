use std::cell::RefCell;
use std::ops::Deref;

/// 現時点のプログラムとは、プリントするべき式の列である
struct RootAst {
    statement: Vec<Statement>
}

enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: i32,
    },
}

struct Parser {
    current_index: RefCell<usize>,
    current_source: String,
}

impl Parser {
    fn create(source: &str) -> Self {
        Self {
            current_source: source.to_string(),
            current_index: RefCell::new(0),
        }
    }

    fn parse(&self) -> Result<RootAst, String> {
        let mut statements = vec![];
        while (self.current_source.len() > *self.current_index.borrow()) {
            let parsed_statement = self.parse_statement().expect("an error occured during parsing statement");
            statements.push(parsed_statement);
        }

        Ok(RootAst {
            statement: statements
        })
    }

    fn parse_statement(&self) -> Result<Statement, String> {
        let parsed = self.parse_print()?;
        self.consume_newline();
        Ok(parsed)
    }

    fn consume_newline(&self) -> Result<(), String> {
        assert_eq!(self.current_char(), '\n');
        *self.current_index.borrow_mut() += 1;
        Ok(())
    }

    fn parse_print(&self) -> Result<Statement, String> {
        self.parse_expression().map(|parsed| {
            Statement::Print {
                expression: parsed
            }
        })
    }

    // TODO: create Expression type
    fn parse_expression(&self) -> Result<i32, String> {
        self.parse_int_literal()
    }

    fn parse_int_literal(&self) -> Result<i32, String> {
        let start_index = *self.current_index.borrow().deref();
        while (self.numeric_chars().contains(&self.current_char())) {
            *self.current_index.borrow_mut() += 1;
        }


        let exclusive_end_index = *self.current_index.borrow().deref();
        let slice = &self.current_source.as_str()[start_index..exclusive_end_index];
        println!("debug: {slice}");
        // TODO: this can be more user-friendly
        let parsed = slice.parse::<i32>().unwrap();
        Ok(parsed)
    }

    fn current_char(&self) -> char {
        self.current_source.chars().nth(*self.current_index.borrow().deref()).unwrap()
    }

    #[inline]
    const fn numeric_chars(&self) -> [char; 10] {
        ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    }
}

fn main() -> Result<(), String> {
    eprintln!("start");
    let source: &str = "123456\n";
    let parser = Parser::create(source);
    let root_ast = parser.parse()?;
    eprintln!("statement count: {}", root_ast.statement.len());
    for statement in root_ast.statement {
        match statement {
            Statement::Print { expression } => {
                println!("{expression}");
            }
        }
    }

    eprintln!("end");
    Ok(())
}
