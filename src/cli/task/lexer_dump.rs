use crate::cli::task::print_ast::ParseSource;
use crate::cli::task::Task;
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::parser::SimpleErrorWithPos;

pub struct LexerDump;

impl Task for LexerDump {
    type Environment = ParseSource;
    type Error = SimpleErrorWithPos;

    fn execute(&self, environment: Self::Environment) -> Result<(), Self::Error> {
        let lexer = Lexer::create(environment.source().as_str());
        let mut token_count = 0;
        while lexer.peek().data != Token::EndOfFile {
            println!("{:?}", lexer.next());
            token_count += 1;
        }
        println!("there were {token_count} token(s).");

        Ok(())
    }
}