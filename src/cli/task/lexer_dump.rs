use std::convert::Infallible;
use crate::cli::task::print_ast::ParseSource;
use crate::cli::task::Task;
use crate::Lexer;
use crate::lexer::Token;

pub struct LexerDump;

impl Task for LexerDump {
    type Environment = ParseSource;
    type Error = String;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let lexer = Lexer::create(environment.source().as_str());
        while lexer.peek() != Token::EndOfFile {
            println!("{:?}", lexer.next());
        }

        Ok(())
    }
}