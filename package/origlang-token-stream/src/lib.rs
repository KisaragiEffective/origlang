#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

use std::backtrace::Backtrace;
use std::cell::Cell;
use std::panic::Location;
use log::{debug, warn};
use origlang_source_span::{Pointed, SourcePosition};
use origlang_lexer::Lexer;
use origlang_lexer::token::Token;

pub struct TokenStream {
    concrete: Vec<Pointed<Token>>,
    pub last_position: SourcePosition,
    current_index: Cell<usize>,
}

impl TokenStream {
    fn new(tokens: Vec<Pointed<Token>>) -> Self {
        Self {
            last_position: tokens.last().map_or_else(
                || SourcePosition::new(1.try_into().unwrap(), 1.try_into().unwrap()),
                |x| x.position
            ),
            concrete: tokens,
            current_index: Cell::new(0)
        }
    }

    /// returns current token without cloning, or [`None`] if position is past over end.
    #[track_caller]
    pub fn peek(&self) -> Option<&Pointed<Token>> {
        let o = Location::caller();
        // debug!("peek: {o}");
        let ret = self.concrete.get(self.current_index.get());
        if ret.is_none() {
            warn!("out of bound: {o}");
            warn!("stacktrace: \n{}", Backtrace::force_capture());
        }
        
        ret
    }
    
    /// returns cloned token on current position. use [`Self::peek`] where possible, as it does not clone implicitly.
    /// returns [`Token::EndOfFile`] if position is past over end.
    pub fn peek_cloned(&self) -> Pointed<Token> {
        self.peek().unwrap_or(&Pointed { data: Token::EndOfFile, position: self.last_position }).clone()
    }
    
    /// returns cloned token on current position, and advance position by one.
    #[track_caller]
    pub fn next(&self) -> Pointed<Token> {
        let o = Location::caller();
        debug!("next: {o}");
        let ret = self.peek_cloned();
        debug!("now[{}] = {:?}", self.current_index.get(), self.peek());
        self.current_index.set(self.current_index.get() + 1);
        
        ret
    }

    /// パースに失敗するかも知れないものをパースしようと試みる。
    /// 成功したならパースした値
    /// 失敗したならNoneを返しつつ内部インデックスをこの関数を呼び出したときの値に戻す:
    ///   これによってコパーサがどれだけ壊れていたとしても失敗時にもとのインデックスに戻ることが保証される
    /// # Errors
    /// もしfがErrを返したとき、パーサーの位置を戻し、その後fの値を伝播する。
    pub fn parse_fallible<T, E>(&self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        let old_position = self.current_index.get();
        match f() {
            Ok(t) => Ok(t),
            Err(e) => {
                self.current_index.set(old_position);
                Err(e)
            }
        }
    }
}

impl From<Lexer<'_>> for TokenStream {
    fn from(value: Lexer<'_>) -> Self {
        let mut buf = vec![];
        
        loop {
            let n = value.next();
            if n.data == Token::EndOfFile {
                break;
            }
            
            buf.push(n);
        }
        
        Self::new(buf)
    }
}