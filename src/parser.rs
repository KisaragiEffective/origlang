use std::cell::RefCell;
use std::ops::Deref;
use crate::{Expression, RootAst, Statement};

pub struct Parser {
    current_index: RefCell<usize>,
    current_source: String,
}

impl Parser {
    pub fn create(source: &str) -> Self {
        Self {
            current_source: source.to_string(),
            current_index: RefCell::new(0),
        }
    }

    pub(crate) fn parse(&self) -> Result<RootAst, String> {
        let mut statements = vec![];
        while self.current_source.len() > *self.current_index.borrow() {
            let parsed_statement = self.parse_statement()?;
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
            self.advance_index_by(1);
            Ok(())
        } else {
            Err(format!("Invalid character. expected: `{expected}`, got: `{found}`", expected = c, found = self.current_char()))
        }
    }

    fn parse_print(&self) -> Result<Statement, String> {
        self.parse_expression().map(|parsed| {
            Statement::Print {
                expression: parsed
            }
        })
    }

    fn parse_expression(&self) -> Result<Expression, String> {
        if self.numeric_chars().contains(&self.current_char()) {
            self.parse_int_literal().map(|parsed| {
                Expression::IntLiteral(parsed)
            })
        } else {
            let ident = self.parse_identifier()?;
            let expr: Expression = Expression::Variable {
                name: ident
            };
            Ok(expr)
        }
    }

    fn parse_int_literal(&self) -> Result<i32, String> {
        let start_index = *self.current_index.borrow().deref();
        while (self.numeric_chars().contains(&self.current_char())) {
            // todo rename to advance and advance_by
            self.advance_index_by(1);
        }

        let exclusive_end_index = *self.current_index.borrow().deref();
        let slice = &self.current_source.as_str()[start_index..exclusive_end_index];
        println!("debug: {slice}");
        // TODO: this can be more user-friendly
        let parsed = slice.parse::<i32>().map_err(|e| e.to_string())?;
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
            self.advance_index_by(1);
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

    fn advance_index_by(&self, step: usize) {
        *self.current_index.borrow_mut() += step;
    }
}
