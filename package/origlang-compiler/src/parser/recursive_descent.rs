use std::convert::Infallible;
use std::marker::PhantomData;
use std::ops::Mul;
use log::debug;
use origlang_ast::{Identifier, RootAst, Statement};
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_source_span::Pointed;
use crate::lexer::token::Token;
use crate::parser::error::{ParserError, ParserErrorInner, UnexpectedTupleLiteralElementCount};
use crate::parser::recursive_descent::combinator::{BacktrackOnFail, LeftAssoc};
use crate::parser::recover::PartiallyParseFixCandidate;
use super::{Parser, TokenKind};

mod combinator {
    use crate::lexer::Lexer;
    use crate::lexer::token::TemporalLexerUnwindToken;
    use crate::parser::Parser;
    use crate::parser::recursive_descent::{IntoAbstractSyntaxTree, TryFromParser};

    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    pub enum Error2<A, B> {
        A(A),
        B(B),
    }

    #[derive(Eq, PartialEq, Copy, Clone, Debug)]
    pub enum Error3<A, B, C> {
        A(A),
        B(B),
        C(C)
    }

    type Punctuated<M, Sep> = GeneralizedPunctuated<M, Sep, M>;
    type PunctuatedPlus<M, Sep> = GeneralizedPunctuated<(M, Sep, M), Sep, M>;

    pub struct GeneralizedPunctuated<First, Sep, RestMember> {
        x: First,
        xs: Vec<(Sep, RestMember)>
    }

    impl<First: TryFromParser, Sep: TryFromParser, M: TryFromParser> TryFromParser for GeneralizedPunctuated<First, Sep, M> {
        type Err = Error3<First::Err, Sep::Err, M::Err>;

        fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
            let x = parser.parse().map_err(Self::Err::A)?;

            let mut vec = Vec::<(Sep, M)>::new();
            while let Ok(sep) = parser.parse().map_err(Self::Err::B) {
                let m = parser.parse().map_err(Self::Err::C)?;

                vec.push((sep, m));
            }

            Ok(Self {
                xs: vec,
                x
            })
        }
    }

    struct AllowTrailing<M, Sep> {
        prev: PunctuatedPlus<M, Sep>,
        trailing: Option<Sep>,
    }

    impl<M: TryFromParser, Sep: TryFromParser> TryFromParser for AllowTrailing<M, Sep> {
        type Err = <PunctuatedPlus<M, Sep> as TryFromParser>::Err;

        fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
            let prev = parser.parse()?;
            let trailing = parser.parse().expect("this is infallible");

            Ok(Self {
                prev, trailing
            })
        }
    }

    /// utility for left-associative binary operator.
    pub(crate) struct LeftAssoc<Operand, Operator>(PunctuatedPlus<Operand, Operator>);

    impl<Operand: TryFromParser, Operator: TryFromParser> TryFromParser for LeftAssoc<Operand, Operator> where Punctuated<Operand, Operator>: TryFromParser {
        type Err = <PunctuatedPlus<Operand, Operator> as TryFromParser>::Err;

        fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
            let res = parser.parse::<PunctuatedPlus<_, _>>()?;

            Ok(Self(res))
        }
    }

    impl<Operand: IntoAbstractSyntaxTree, Operator> IntoAbstractSyntaxTree for LeftAssoc<Operand, Operator>
        where Operand::Ast: BuildBinaryOperator<Operand, Operator> {
        type Ast = Operand::Ast;

        fn into_ast(self) -> Self::Ast {
            let (lhs, op, rhs) = self.0.x;
            self.0.xs.into_iter().fold(Self::Ast::build_binary_operator(lhs, op, rhs), |ast, (new_op, new_rhs)| {
                ast.chain_rhs(new_op, new_rhs)
            })
        }
    }

    pub(crate) trait BuildBinaryOperator<Operand, Operator> : Sized {
        fn build_binary_operator(lhs: Operand, op: Operator, rhs: Operand) -> Self;

        fn chain_rhs(self, child_op: Operator, child_rhs: Operand) -> Self;
    }

    /// parse [`Self::Inner`] or return token to backtrack [`Parser`].
    pub struct BacktrackOnFail<Inner>(Inner);

    impl<I> BacktrackOnFail<I> {
        pub fn into_inner(self) -> I {
            self.0
        }
    }

    impl<I: TryFromParser> TryFromParser for BacktrackOnFail<I> {
        type Err = BacktrackInto;

        fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
            let x = parser.lexer.create_reset_token();
            if let Ok(e) = parser.parse() {
                Ok(e)
            } else {
                Err(BacktrackInto(x))
            }
        }
    }

    #[must_use = "this token should be used or dropped in general case"]
    pub struct BacktrackInto(TemporalLexerUnwindToken);

    impl BacktrackInto {
        pub fn backtrack(self, lexer: &Lexer) {
            self.0.reset(lexer);
        }
    }
}

impl<
    Operand: IntoAbstractSyntaxTree<Ast=Expression>, Operator: Into<BinaryOperatorKind>
> combinator::BuildBinaryOperator<Operand, Operator> for Expression {
    fn build_binary_operator(lhs: Operand, op: Operator, rhs: Operand) -> Self {
        Self::BinaryOperator {
            lhs: Box::new(lhs.into_ast()),
            rhs: Box::new(rhs.into_ast()),
            operator: op.into(),
        }
    }

    fn chain_rhs(self, child_op: Operator, child_rhs: Operand) -> Self {
        assert!(matches!(self, Self::BinaryOperator { .. }));
        Self::BinaryOperator {
            lhs: Box::new(self),
            rhs: Box::new(child_rhs.into_ast()),
            operator: child_op.into()
        }
    }
}

pub trait TryFromParser: Sized {
    type Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err>;
}

pub trait IntoAbstractSyntaxTree: Sized {
    type Ast;

    fn into_ast(self) -> Self::Ast;
}

impl<R: TryFromParser> TryFromParser for Pointed<R> {
    type Err = R::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        let pos = parser.lexer.peek().position;
        let r = R::parse(parser)?;

        Ok(Pointed {
            data: r,
            position: pos,
        })
    }
}

impl<R: TryFromParser> TryFromParser for Box<R> {
    type Err = R::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        R::parse(parser).map(Box::new)
    }
}

impl<A: TryFromParser, B: TryFromParser> TryFromParser for (A, B) {
    type Err = combinator::Error2<A::Err, B::Err>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        Ok((parser.parse().map_err(Self::Err::A)?, parser.parse().map_err(Self::Err::B)?))
    }
}

impl<A: TryFromParser, B: TryFromParser, C: TryFromParser> TryFromParser for (A, B, C) {
    type Err = combinator::Error3<A::Err, B::Err, C::Err>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        Ok((parser.parse().map_err(Self::Err::A)?, parser.parse().map_err(Self::Err::B)?, parser.parse().map_err(Self::Err::C)?))
    }
}

impl<A: TryFromParser> TryFromParser for Option<A> {
    type Err = Infallible;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        Ok(parser.parse().ok())
    }
}

impl TryFromParser for RootAst {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        let mut statements = vec![];
        while parser.lexer.peek().data != Token::EndOfFile {
            let res = parser.parse()?;
            statements.push(res);
        }

        {
            let t = parser.lexer.next();
            match t.data {
                Token::EndOfFile | Token::NewLine => Ok(RootAst {
                    statement: statements,
                }),
                other => Err(ParserError::new(ParserErrorInner::UnconsumedToken { token: other }, t.position)),
            }
        }
    }
}

impl TryFromParser for Statement {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        while let Token::NewLine = parser.lexer.peek().data {
            parser.lexer.next();
        }

        if parser.lexer.peek().data == Token::EndOfFile {
            parser.lexer.next();
            return Ok(Statement::Exit)
        }

        let head1 = parser.lexer.peek();
        let head = head1.data;
        let pos = head1.position;

        let s = match head {
            Token::Identifier { .. } => {
                parser.parse_variable_assignment()?
            }
            Token::VarKeyword => {
                parser.parse_variable_declaration()?
            }
            Token::KeywordPrint => {
                // assuming expression
                parser.lexer.next();
                debug!("expr:lowest");
                let expr = parser.parse::<BlockExpressionRule>()?.into_ast();

                Statement::Print {
                    expression: expr
                }
            }
            Token::KeywordBlock => {
                parser.parse_block_scope()?
            }
            Token::Comment { content } => {
                parser.lexer.next();
                Statement::Comment { content }
            }
            Token::KeywordExit => {
                parser.lexer.next();
                Statement::Exit
            }
            Token::KeywordType => {
                parser.lexer.next();
                let aliased = parser.lexer.next();

                let Token::Identifier { inner: aliased } = aliased.data else {
                    return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                        pat: TokenKind::Identifier,
                        unmatch: aliased.data
                    }, aliased.position)) };

                parser.read_and_consume_or_report_unexpected_token(Token::SymEq)?;
                let Ok(replace_with) = parser.lexer.parse_fallible(|| parser.parse_type()) else {
                    return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                        pat: TokenKind::StartOfTypeSignature,
                        unmatch: parser.lexer.peek().data
                    }, parser.lexer.peek().position))
                };

                Statement::TypeAliasDeclaration {
                    new_name: aliased,
                    replace_with,
                }
            }
            x => {
                return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                    pat: TokenKind::Statement,
                    unmatch: x,
                }, pos,))
            }
        };

        // 文は絶対に改行かEOFで終わる必要がある
        let next = parser.lexer.next();
        if next.data != Token::NewLine && next.data != Token::EndOfFile {
            return Err(ParserError::new(ParserErrorInner::PartiallyParsed {
                hint: vec![
                    PartiallyParseFixCandidate::InsertAfter {
                        tokens: vec![ Token::NewLine ]
                    }
                ],
                intermediate_state: vec![],
            }, next.position))
        }

        Ok(s)
    }
}

pub enum First {
    Variable {
        ident: Identifier,
    },
    BooleanLiteral(bool),
    StringLiteral(String),
    IntLiteral {
        value: i64,
        suffix: Option<Box<str>>,
    },
    UnitLiteral,
    Parenthesised(Box<LowestExpression>),
    TupleLiteral(TupleExpression),
}

impl TryFromParser for First {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        debug!("expr:first");
        let token = parser.lexer.peek();
        match token.data {
            Token::Identifier { inner } => {
                // consume
                parser.lexer.next();
                Ok(Self::Variable {
                    ident: inner
                })
            }
            Token::SymUnderscore => {
                parser.lexer.next();
                Err(ParserError::new(ParserErrorInner::UnderscoreCanNotBeRightHandExpression, token.position,))
            }
            Token::Digits { .. } => {
                parser.parse_int_literal().map(|(parsed, suffix)| {
                    Self::IntLiteral { value: parsed, suffix }
                })
            }
            Token::SymLeftPar => {
                assert_eq!(parser.lexer.next().data, Token::SymLeftPar);
                // FIXME: (1 == 2)を受け付けない
                if parser.lexer.peek().data == Token::SymRightPar {
                    parser.lexer.next();
                    Ok(Self::UnitLiteral)
                } else if let Ok(expr_tuple) = TupleExpression::parse(parser) {
                    Ok(Self::TupleLiteral(expr_tuple))
                } else {
                    let inner_expression = parser.parse()?;
                    parser.read_and_consume_or_report_unexpected_token(Token::SymRightPar)?;
                    Ok(Self::Parenthesised(inner_expression))
                }
            }
            Token::KeywordTrue => {
                parser.lexer.next();
                Ok(Self::BooleanLiteral(true))
            }
            Token::KeywordFalse => {
                parser.lexer.next();
                Ok(Self::BooleanLiteral(false))
            }
            Token::EndOfFile => {
                Err(ParserError::new(ParserErrorInner::EndOfFileError, token.position,))
            }
            Token::StringLiteral(s) => {
                parser.lexer.next();
                Ok(Self::StringLiteral(s))
            }
            e => Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                pat: TokenKind::First,
                unmatch: e,
            }, token.position,))
        }
    }
}

impl IntoAbstractSyntaxTree for First {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            First::Variable { ident } => Self::Ast::Variable { ident },
            First::BooleanLiteral(v) => Self::Ast::BooleanLiteral(v),
            First::StringLiteral(v) => Self::Ast::StringLiteral(v),
            First::IntLiteral { value, suffix } => Self::Ast::IntLiteral { value, suffix },
            First::UnitLiteral => Self::Ast::UnitLiteral,
            First::Parenthesised(v) => v.into_ast(),
            First::TupleLiteral(expressions) => Self::Ast::Tuple {
                expressions: expressions.0.into_iter().map(|x| x.into_ast()).collect(),
            }
        }
    }
}

struct TupleExpression(Vec<LowestExpression>);

impl TryFromParser for TupleExpression {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        parser.lexer.parse_fallible(|| {
            debug!("expr.tuple");

            let mut buf = vec![];
            while let Ok(e) = LowestExpression::parse(parser) {
                buf.push(e);
                let peek = parser.lexer.peek();
                if peek.data == Token::SymRightPar {
                    parser.lexer.next();
                    break
                } else if peek.data != Token::SymComma {
                    return Err(ParserError::new(ParserErrorInner::UnexpectedToken {
                        pat: TokenKind::Only(Token::SymComma.display()),
                        unmatch: peek.data,
                    }, peek.position,))
                }

                parser.lexer.next();
            }

            let bl = buf.len();

            if bl == 0 {
                // disallow ()
                return Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::Zero), parser.lexer.peek().position,))
            } else if bl == 1 {
                // disallow (expr)
                return Err(ParserError::new(ParserErrorInner::InsufficientElementsForTupleLiteral(UnexpectedTupleLiteralElementCount::One), parser.lexer.peek().position,))
            }

            Ok(Self(buf))
        })
    }
}

pub enum MultiplicativeExpressionRule {
    Operator {
        operands: LeftAssoc<First, MultiplicativeExpressionOperator>
    },
    Propagate(First)
}

impl TryFromParser for MultiplicativeExpressionRule {
    type Err = <First as TryFromParser>::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.parse::<BacktrackOnFail<_>>() {
            Ok(e) => return Ok(Self::Operator { operands: e.into_inner() }),
            Err(e) => e.backtrack(&parser.lexer)
        };

        Ok(Self::Propagate(parser.parse()?))
    }
}

impl IntoAbstractSyntaxTree for MultiplicativeExpressionRule {
    type Ast = <First as IntoAbstractSyntaxTree>::Ast;

    fn into_ast(self) -> Self::Ast {
        match self {
            Self::Operator { operands } => operands.into_ast(),
            Self::Propagate(x) => x.into_ast(),
        }
    }
}

enum MultiplicativeExpressionOperator {
    Multiple,
    Divide,
}

impl TryFromParser for MultiplicativeExpressionOperator {
    type Err = InvalidTokenForOperator<Self>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.lexer.peek().data {
            Token::SymAsterisk => {
                parser.lexer.next();
                Ok(Self::Multiple)
            }
            Token::SymSlash => {
                parser.lexer.next();
                Ok(Self::Divide)
            }
            _ => Err(InvalidTokenForOperator(PhantomData))
        }
    }
}

impl From<MultiplicativeExpressionOperator> for BinaryOperatorKind {
    fn from(value: MultiplicativeExpressionOperator) -> Self {
        match value {
            MultiplicativeExpressionOperator::Multiple => Self::Multiply,
            MultiplicativeExpressionOperator::Divide => Self::Divide
        }
    }
}

enum AdditiveExpressionRule {
    Operator {
        operands: LeftAssoc<MultiplicativeExpressionRule, AdditiveExpressionOperator>,
    },
    Propagate(MultiplicativeExpressionRule)
}

impl TryFromParser for AdditiveExpressionRule {
    type Err = <MultiplicativeExpressionRule as TryFromParser>::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.parse::<BacktrackOnFail<_>>() {
            Ok(e) => return Ok(Self::Operator { operands: e.into_inner() }),
            Err(e) => e.backtrack(&parser.lexer)
        };

        Ok(Self::Propagate(parser.parse()?))
    }
}

impl IntoAbstractSyntaxTree for AdditiveExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            AdditiveExpressionRule::Operator { operands } => operands.into_ast(),
            AdditiveExpressionRule::Propagate(inner) => inner.into_ast(),
        }
    }
}

enum AdditiveExpressionOperator {
    Add,
    Subtract,
}

impl TryFromParser for AdditiveExpressionOperator {
    type Err = InvalidTokenForOperator<Self>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.lexer.peek().data {
            Token::SymPlus => {
                parser.lexer.next();
                Ok(Self::Add)
            }
            Token::SymMinus => {
                parser.lexer.next();
                Ok(Self::Subtract)
            }
            _ => Err(InvalidTokenForOperator(PhantomData))
        }
    }
}

impl From<AdditiveExpressionOperator> for BinaryOperatorKind {
    fn from(value: AdditiveExpressionOperator) -> Self {
        match value {
            AdditiveExpressionOperator::Add => Self::Plus,
            AdditiveExpressionOperator::Subtract => Self::Minus
        }
    }
}

enum ShiftExpressionRule {
    Operator {
        operands: LeftAssoc<AdditiveExpressionRule, ShiftExpressionOperator>
    },
    Propagate(AdditiveExpressionRule)
}

impl TryFromParser for ShiftExpressionRule {
    type Err = <AdditiveExpressionRule as TryFromParser>::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.parse::<BacktrackOnFail<_>>() {
            Ok(e) => return Ok(Self::Operator { operands: e.into_inner() }),
            Err(e) => e.backtrack(&parser.lexer)
        };

        Ok(Self::Propagate(parser.parse()?))
    }
}

impl IntoAbstractSyntaxTree for ShiftExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            ShiftExpressionRule::Operator { operands } => operands.into_ast(),
            ShiftExpressionRule::Propagate(inner) => inner.into_ast()
        }
    }
}

enum ShiftExpressionOperator {
    LeftShift,
    RightShift,
}

impl TryFromParser for ShiftExpressionOperator {
    type Err = InvalidTokenForOperator<Self>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.lexer.peek().data {
            Token::PartLessLess => {
                parser.lexer.next();
                Ok(Self::LeftShift)
            }
            Token::PartMoreMore => {
                parser.lexer.next();
                Ok(Self::RightShift)
            }
            _ => Err(InvalidTokenForOperator(PhantomData))
        }
    }
}

impl From<ShiftExpressionOperator> for BinaryOperatorKind {
    fn from(value: ShiftExpressionOperator) -> Self {
        match value {
            ShiftExpressionOperator::LeftShift => Self::ShiftLeft,
            ShiftExpressionOperator::RightShift => Self::ShiftRight,
        }
    }
}

enum RelationExpressionRule {
    Operator {
        operands: LeftAssoc<ShiftExpressionRule, RelationExpressionOperator>
    },
    Propagate(ShiftExpressionRule)
}

impl TryFromParser for RelationExpressionRule {
    type Err = <ShiftExpressionRule as TryFromParser>::Err;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        match parser.parse::<BacktrackOnFail<_>>() {
            Ok(e) => return Ok(Self::Operator { operands: e.into_inner() }),
            Err(e) => e.backtrack(&parser.lexer)
        };

        Ok(Self::Propagate(parser.parse()?))
    }
}

impl IntoAbstractSyntaxTree for RelationExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            RelationExpressionRule::Operator { operands } => operands.into_ast(),
            RelationExpressionRule::Propagate(x) => x.into_ast(),
        }
    }
}

enum RelationExpressionOperator {
    More,
    MoreOrEqual,
    Less,
    LessOrEqual,
    ThreeWay,
}

impl TryFromParser for RelationExpressionOperator {
    type Err = InvalidTokenForOperator<Self>;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        struct OnlyMove;
        let call_at_most_once = OnlyMove;
        let next = move || {
            #[allow(path_statements, clippy::no_effect)]
            call_at_most_once;
            parser.lexer.next()
        };

        match parser.lexer.peek().data {
            Token::SymLess => {
                next();
                Ok(Self::Less)
            }
            Token::SymMore => {
                next();
                Ok(Self::More)
            }
            Token::PartLessEq => {
                next();
                Ok(Self::LessOrEqual)
            }
            Token::PartMoreEq => {
                next();
                Ok(Self::MoreOrEqual)
            }
            Token::PartLessEqMore => {
                next();
                Ok(Self::ThreeWay)
            }
            _ => Err(InvalidTokenForOperator(PhantomData))
        }
    }
}

impl From<RelationExpressionOperator> for BinaryOperatorKind {
    fn from(value: RelationExpressionOperator) -> Self {
        match value {
            RelationExpressionOperator::More => Self::More,
            RelationExpressionOperator::MoreOrEqual => Self::MoreEqual,
            RelationExpressionOperator::Less => Self::Less,
            RelationExpressionOperator::LessOrEqual => Self::LessEqual,
            RelationExpressionOperator::ThreeWay => Self::ThreeWay,
        }
    }
}

enum EqualityExpressionRule {
    Equality {
        operands: LeftAssoc<RelationExpressionRule, EqualityExpressionOperator>,
    },
    Propagate(RelationExpressionRule),
}

impl TryFromParser for EqualityExpressionRule {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        debug!("expr:eq");
        match parser.parse::<BacktrackOnFail<_>>() {
            Ok(e) => return Ok(Self::Equality { operands: e.into_inner() }),
            Err(e) => e.backtrack(&parser.lexer)
        };

        Ok(Self::Propagate(parser.parse()?))
    }
}

impl IntoAbstractSyntaxTree for EqualityExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            Self::Equality { operands } => operands.into_ast(),
            Self::Propagate(x) => x.into_ast(),
        }
    }
}

struct InvalidTokenForOperator<OperatorFamily>(PhantomData<OperatorFamily>);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum EqualityExpressionOperator {
    Equal,
    NotEqual,
}

impl TryFrom<Token> for EqualityExpressionOperator {
    type Error = InvalidTokenForOperator<Self>;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::PartEqEq => Ok(Self::Equal),
            Token::PartBangEq => Ok(Self::NotEqual),
            _ => Err(InvalidTokenForOperator(PhantomData))
        }
    }
}

impl TryFromParser for EqualityExpressionOperator {
    type Err = <Self as TryFrom<Token>>::Error;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        Self::try_from(parser.lexer.next().data)
    }
}

impl From<EqualityExpressionOperator> for BinaryOperatorKind {
    fn from(value: EqualityExpressionOperator) -> Self {
        match value {
            EqualityExpressionOperator::Equal => Self::Equal,
            EqualityExpressionOperator::NotEqual => Self::NotEqual,
        }
    }
}

enum IfExpressionRule {
    If {
        condition: Box<LowestExpression>,
        then_clause_value: Box<LowestExpression>,
        else_clause_value: Box<LowestExpression>,
    },
    Propagate(EqualityExpressionRule)
}

impl TryFromParser for IfExpressionRule {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        debug!("expr:if");
        if parser.lexer.peek().data == Token::KeywordIf {
            parser.lexer.next();
            let condition = parser.parse()?;
            parser.read_and_consume_or_report_unexpected_token(Token::KeywordThen)?;
            let then_clause_value = parser.parse()?;
            parser.read_and_consume_or_report_unexpected_token(Token::KeywordElse)?;
            let else_clause_value = parser.parse()?;
            Ok(Self::If {
                condition: Box::new(condition),
                then_clause_value: Box::new(then_clause_value),
                else_clause_value: Box::new(else_clause_value),
            })
        } else {
            Ok(Self::Propagate(parser.parse()?))
        }
    }
}

impl IntoAbstractSyntaxTree for IfExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            IfExpressionRule::If { condition, then_clause_value, else_clause_value } => {
                Expression::If {
                    condition: Box::new(condition.into_ast()),
                    then_clause_value: Box::new(then_clause_value.into_ast()),
                    else_clause_value: Box::new(else_clause_value.into_ast()),
                }
            }
            IfExpressionRule::Propagate(x) => {
                x.into_ast()
            }
        }
    }
}

pub enum BlockExpressionRule {
    Block {
        intermediate_statements: Vec<Statement>,
        final_expression: Box<Self>,
    },
    Propagate(IfExpressionRule),
}

impl TryFromParser for BlockExpressionRule {
    type Err = ParserError;

    fn parse(parser: &Parser<'_>) -> Result<Self, Self::Err> {
        debug!("parser:block:expr");
        if parser.lexer.peek().data == Token::KeywordBlock {
            parser.lexer.next();
            parser.read_and_consume_or_report_unexpected_token(Token::NewLine)?;
            let mut statements = vec![];
            while let Ok(v) = Statement::parse(parser) {
                statements.push(v);
            }
            let final_expression = Self::parse(parser)?;
            if parser.lexer.peek().data == Token::NewLine {
                parser.lexer.next();
            }
            parser.read_and_consume_or_report_unexpected_token(Token::KeywordEnd)?;
            Ok(Self::Block {
                intermediate_statements: statements,
                final_expression: Box::new(final_expression),
            })
        } else {
            Ok(Self::Propagate(parser.parse()?))
        }
    }
}

impl IntoAbstractSyntaxTree for BlockExpressionRule {
    type Ast = Expression;

    fn into_ast(self) -> Self::Ast {
        match self {
            BlockExpressionRule::Block { intermediate_statements, final_expression } => {
                Self::Ast::Block {
                    intermediate_statements,
                    final_expression: Box::new(final_expression.into_ast())
                }
            }
            BlockExpressionRule::Propagate(x) => x.into_ast(),
        }
    }
}

type LowestExpression = BlockExpressionRule;
