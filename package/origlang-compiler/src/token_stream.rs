use std::cell::Cell;
use origlang_source_span::{Pointed, SourcePosition};
use crate::lexer::Lexer;
use crate::lexer::token::Token;

pub struct TokenStream {
    concrete: Vec<Pointed<Token>>,
    pub(crate) last_position: SourcePosition,
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
    pub(crate) fn peek_ref(&self) -> Option<&Pointed<Token>> {
        self.concrete.get(self.current_index.get())
    }
    
    /// returns cloned token on current position. use [`Self::peek_ref`] where possible, as it does not clone implicitly.
    /// returns [`Token::EndOfFile`] if position is past over end.
    pub(crate) fn peek(&self) -> Pointed<Token> {
        self.peek_ref().unwrap_or(&Pointed { data: Token::EndOfFile, position: self.last_position }).clone()
    }
    
    /// returns cloned token on current position, and advance position by one.
    pub(crate) fn next(&self) -> Pointed<Token> {
        let ret = self.peek();
        self.current_index.set(self.current_index.get() + 1);
        
        ret
    }

    /// パースに失敗するかも知れないものをパースしようと試みる。
    /// 成功したならパースした値
    /// 失敗したならNoneを返しつつ内部インデックスをこの関数を呼び出したときの値に戻す:
    ///   これによってコパーサがどれだけ壊れていたとしても失敗時にもとのインデックスに戻ることが保証される
    /// # Errors
    /// もしfがErrを返したとき、パーサーの位置を戻し、その後fの値を伝播する。
    pub(crate) fn parse_fallible<T, E>(&self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
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