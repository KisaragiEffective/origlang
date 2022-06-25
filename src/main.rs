use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;

/// 現時点のプログラムとは、プリントするべき式の列である
struct RootAst {
    statement: Vec<Statement>
}

enum Statement {
    /// <int_literal> <new_line>
    Print {
        expression: Expression,
    },
    VariableDeclaration {
        identifier: String,
        expression: Expression,
    }
}

#[derive(Clone, Debug)]
enum Expression {
    IntLiteral(i32),
    Variable {
        name: String,
    }
}

impl Into<Expression> for i32 {
    fn into(self) -> Expression {
        Expression::IntLiteral(self)
    }
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
        let parse_result = match self.current_char() {
            // TODO: better switch
            'v' => self.parse_variable_declaration(),
            _ => self.parse_print()
        }?;

        self.consume_newline()?;
        Ok(parse_result)
    }

    fn consume_newline(&self) -> Result<(), String> {
        self.consume('\n')
    }

    fn consume(&self, c: char) -> Result<(), String> {
        if self.current_char() == c {
            self.advance_index(1);
            Ok(())
        } else {
            Err(format!("Invalid character. expected: `{expected}`, got: `{found}`", expected = c, found = self.current_char()))
        }
    }

    fn parse_print(&self) -> Result<Statement, String> {
        self.parse_expression().map(|parsed| {
            Statement::Print {
                expression: parsed.into()
            }
        })
    }

    fn parse_expression(&self) -> Result<Expression, String> {
        let expression = if !self.numeric_chars().contains(&self.current_char()) {
            let ident = self.parse_identifier()?;
            let expr: Expression = Expression::Variable {
                name: ident
            };
            Ok(expr)
        } else {
            self.parse_int_literal().map(|parsed| {
                Expression::IntLiteral(parsed)
            })
        };

        expression
    }

    fn parse_int_literal(&self) -> Result<i32, String> {
        let start_index = *self.current_index.borrow().deref();
        while (self.numeric_chars().contains(&self.current_char())) {
            self.advance_index(1);
        }


        let exclusive_end_index = *self.current_index.borrow().deref();
        let slice = &self.current_source.as_str()[start_index..exclusive_end_index];
        println!("debug: {slice}");
        // TODO: this can be more user-friendly
        let parsed = slice.parse::<i32>().unwrap();
        Ok(parsed)
    }

    fn parse_variable_declaration(&self) -> Result<Statement, String> {
        "var ".chars().map(|c| self.consume(c)).collect::<Result<Vec<_>, _>>()?;
        let ident = self.parse_identifier()?;
        self.consume(' ')?;
        self.consume('=')?;
        self.consume(' ')?;
        let expression = self.parse_expression()?;
        Ok(Statement::VariableDeclaration {
            identifier: ident,
            expression
        })
    }

    fn parse_identifier(&self) -> Result<String, String> {
        let start_index = *self.current_index.borrow();
        // [a-z]+
        while Self::lower_alphabet_chars().contains(&self.current_char()) {
            self.advance_index(1);
        }
        let exclusive_end_index = *self.current_index.borrow();
        if start_index == exclusive_end_index {
            Err("empty identifier is not allowed".to_string())
        } else {
            let slice = &self.current_source.as_str()[start_index..exclusive_end_index];
            Ok(slice.to_string())
        }
    }

    #[inline]
    const fn lower_alphabet_chars() -> [char; 26] {
        ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
    }

    #[inline]
    const fn upper_alphabet_chars() -> [char; 26] {
        ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
    }

    fn current_char(&self) -> char {
        self.current_source.chars().nth(*self.current_index.borrow().deref()).unwrap()
    }

    #[inline]
    const fn numeric_chars(&self) -> [char; 10] {
        ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    }

    fn advance_index(&self, step: usize) {
        *self.current_index.borrow_mut() += step;
    }
}

struct Runtime {
    /// すでに評価された値を格納しておく
    environment: RefCell<HashMap<String, i32>>,
    ast: RootAst,
}

impl Runtime {
    fn create(root_ast: RootAst) -> Self {
        Self {
            environment: RefCell::new(HashMap::new()),
            ast: root_ast,
        }
    }

    fn execute(&self) {
        for statement in &self.ast.statement {
            match statement {
                Statement::Print { expression } => {
                    println!("{value}", value = self.evaluate(expression).unwrap());
                }
                Statement::VariableDeclaration { identifier, expression } => {
                    // NOTE: please do not inline. it causes BorrowError.
                    let evaluated = self.evaluate(expression).expect("error happened during evaluating expression");
                    self.environment.borrow_mut().insert(identifier.clone(), evaluated);
                }
            }
        }
    }

    fn evaluate(&self, expression: &Expression) -> Result<i32, String> {
        match expression {
            Expression::IntLiteral(inner) => Ok(*inner),
            Expression::Variable {
                name
            } => {
                // temporary value
                let read_view = self.environment.borrow();
                let variable = read_view.get(name).expect("variable does not exist");
                Ok(*variable)
            }
        }
    }
}

fn main() -> Result<(), String> {
    eprintln!("start");
    {
        let source: &str = "123456\n";
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let runtime = Runtime::create(root_ast);
        runtime.execute();
    }

    {
        let source = "var x = 1\nx\n";
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let runtime = Runtime::create(root_ast);
        runtime.execute();
    }

    {
        let source = "var x = 1\nvar y = x\ny\n";
        let parser = Parser::create(source);
        let root_ast = parser.parse()?;
        let runtime = Runtime::create(root_ast);
        runtime.execute();
    }
    eprintln!("end");
    Ok(())
}
