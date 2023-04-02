use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use derivative::Derivative;
use derive_more::{Display, From};
use log::debug;
use tap::Conv;
use thiserror::Error;
use origlang_ast::{RootAst, Statement};
use origlang_ast::after_parse::{BinaryOperatorKind, Expression};
use origlang_typesystem_model::Type;

#[derive(From)]
pub struct Coerced(i64);

impl From<Coerced> for TypeBox {
    fn from(value: Coerced) -> Self {
        Self::Int64(value.0)
    }
}

#[derive(From)]
pub struct NonCoerced(i64);

impl From<NonCoerced> for TypeBox {
    fn from(value: NonCoerced) -> Self {
        Self::NonCoercedInteger(value.0)
    }
}

#[derive(Error, Debug)]
pub enum TypeBoxUnwrapError {
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DisplayTuple {
    pub boxes: Vec<TypeBox>
}

impl Display for DisplayTuple {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self.boxes.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let s = format!("({s})");
        f.write_str(&s)
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Display, From)]
pub enum TypeBox {
    #[display(fmt = "{_0}")]
    NonCoercedInteger(i64),
    #[display(fmt = "{_0}")]
    #[from]
    Int8(i8),
    #[display(fmt = "{_0}")]
    #[from]
    Int16(i16),
    #[display(fmt = "{_0}")]
    #[from]
    Int32(i32),
    #[display(fmt = "{_0}")]
    Int64(i64),
    #[display(fmt = "{_0}")]
    #[from]
    Boolean(bool),
    #[display(fmt = "{_0}")]
    #[from]
    String(String),
    #[display(fmt = "()")]
    #[from]
    Unit(()),
    #[display(fmt = "{_0}")]
    #[from]
    Tuple(DisplayTuple)
}

impl TypeBox {
    pub fn get_type(&self) -> Type {
        match self {
            Self::NonCoercedInteger(_) => Type::GenericInteger,
            Self::Boolean(_) => Type::Boolean,
            Self::String(_) => Type::String,
            Self::Unit(_) => Type::Unit,
            Self::Int8(_) => Type::Int8,
            Self::Int16(_) => Type::Int16,
            Self::Int32(_) => Type::Int32,
            Self::Int64(_) => Type::Int64,
            Self::Tuple(t) => Type::Tuple(t.boxes.iter().map(|x| x.get_type()).collect::<Vec<_>>().into())
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, TypeBox>,
}

impl Scope {
    fn empty() -> Self {
        Self {
            variables: (HashMap::new())
        }
    }
}

pub trait OutputAccumulator: Debug {
    fn output(&mut self, tb: TypeBox);

    fn acc(&self) -> Option<Vec<TypeBox>>;
}

#[derive(Debug)]
pub struct PrintToStdout;

impl OutputAccumulator for PrintToStdout {
    fn output(&mut self, tb: TypeBox) {
        println!("{tb}");
    }

    fn acc(&self) -> Option<Vec<TypeBox>> {
        None
    }
}

#[derive(Default, Debug)]
pub struct Accumulate(Vec<TypeBox>);

impl OutputAccumulator for Accumulate {
    fn output(&mut self, tb: TypeBox) {
        self.0.push(tb);
    }

    fn acc(&self) -> Option<Vec<TypeBox>> {
        Some(self.0.clone())
    }
}

trait DebuggableTrait: (Fn() -> TypeBox) + Debug {}

impl<T: (Fn() -> TypeBox) + Debug> DebuggableTrait for T {}

#[derive(Derivative)]
#[derivative(Debug)]
pub enum EffectDescriptor<'cls> {
    Output(#[derivative(Debug="ignore")] Box<dyn (Fn() -> TypeBox) + 'cls>),
    UpdateVariable {
        ident: String,
        #[derivative(Debug="ignore")]
        value: Box<dyn (Fn() -> TypeBox) + 'cls>,
    },
    PushScope,
    PopScope,
}

impl<'s: 'cls, 'cls> EffectDescriptor<'cls> {
    fn invoke(&'s self, runtime: &Runtime) {
        match self {
            EffectDescriptor::Output(e) => {
                runtime.o.as_ref().borrow_mut().output(e());
            }
            EffectDescriptor::UpdateVariable { ident, value } => {
                runtime.upsert_member_to_current_scope(ident.clone(), value());
            }
            EffectDescriptor::PushScope => {
                runtime.push_scope();
            }
            EffectDescriptor::PopScope => {
                runtime.pop_scope();
            }
        }
    }
}

#[derive(Debug)]
pub struct Runtime {
    scopes: RefCell<VecDeque<Scope>>,
    o: Box<RefCell<dyn OutputAccumulator>>,
}

impl Runtime {
    pub fn create<T: OutputAccumulator + 'static>(t: T) -> Self {
        let mut scopes = VecDeque::new();
        scopes.push_front(Scope::empty());
        let scopes = RefCell::new(scopes);
        let o = Box::new(RefCell::new(t));
        Self {
            scopes,
            o
        }
    }

    #[allow(dead_code)]
    pub fn execute<'s: 'o, 'o>(&'s self, ast: RootAst) -> &'o RefCell<dyn OutputAccumulator> {
        self.what_will_happen(ast).into_iter().for_each(|x| x.invoke(self));
        &self.o
    }

    pub fn what_will_happen(&self, ast: RootAst) -> Vec<EffectDescriptor> {
        ast.statement.into_iter()
            .flat_map(|x| self.what_will_happen1(x))
            .collect::<Vec<EffectDescriptor>>()
    }

    fn what_will_happen1(&self, statement: Statement) -> Vec<EffectDescriptor> {
        match statement {
            Statement::Print { expression } => {
                vec![
                    EffectDescriptor::Output(Box::new(move || self.evaluate(&expression).unwrap()))
                ]
            }
            Statement::VariableDeclaration { identifier, expression } => {
                vec![
                    EffectDescriptor::UpdateVariable {
                        ident: identifier,
                        value: Box::new(move || self.evaluate(&expression).unwrap()),
                    }
                ]
            }
            Statement::VariableAssignment { identifier, expression } => {
                vec![
                    EffectDescriptor::UpdateVariable {
                        ident: identifier,
                        value: Box::new(move || self.evaluate(&expression).unwrap()),
                    }
                ]
            }
            Statement::Block { inner_statements } => {
                let mut vec = inner_statements.into_iter()
                    .flat_map(|x| self.what_will_happen1(x))
                    .collect::<VecDeque<EffectDescriptor>>();
                vec.push_front(EffectDescriptor::PushScope);
                vec.push_back(EffectDescriptor::PopScope);
                vec.into()
            }
        }
    }

    pub fn evaluate<E: CanBeEvaluated>(&self, expression: &E) -> EvaluateResult {
        expression.evaluate(self)
    }

    fn push_scope(&self) {
        debug!("enter scope({len})", len = self.scopes.borrow().len());
        self.scopes.borrow_mut().push_front(Scope::empty());
    }

    fn pop_scope(&self) {
        debug!("exit scope({len})", len = self.scopes.borrow().len());
        self.scopes.borrow_mut().pop_front().expect("scope must not be empty");
    }

    fn search_member(&self, identifier: &str) -> Option<TypeBox> {
        let x = self.scopes.borrow();
        x.iter().find_map(|x| x.variables.get(identifier)).cloned()
    }

    fn upsert_member_to_current_scope(&self, identifier: String, value: TypeBox) {
        let mut guard = self.scopes.borrow_mut();
        let current_scope = guard.front_mut().expect("scope must not be empty");
        current_scope.variables.insert(identifier, value);
    }
}

#[derive(Error, Debug, Eq, PartialEq)]
#[allow(clippy::module_name_repetitions)]
pub enum RuntimeError {
    #[error("variable {identifier} is not defined in current scope")]
    UndefinedVariable {
        identifier: Box<str>,
    },
}

type EvaluateResult = Result<TypeBox, RuntimeError>;
pub trait CanBeEvaluated {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult;
}

macro_rules! f {
    ($lhs:ident, $operator:expr, $rhs:ident) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let ret = match $operator {
            BinaryOperatorKind::Plus => (lhs.wrapping_add(rhs)).into(),
            BinaryOperatorKind::Minus => (lhs.wrapping_sub(rhs)).into(),
            BinaryOperatorKind::Multiply => (lhs.wrapping_mul(rhs)).into(),
            BinaryOperatorKind::Divide => (lhs.wrapping_div(rhs)).into(),
            BinaryOperatorKind::More => (lhs > rhs).into(),
            BinaryOperatorKind::MoreEqual => (lhs >= rhs).into(),
            BinaryOperatorKind::Less => (lhs < rhs).into(),
            BinaryOperatorKind::LessEqual => (lhs <= rhs).into(),
            BinaryOperatorKind::ThreeWay => {
                match lhs.cmp(&rhs) {
                    ::std::cmp::Ordering::Less => -1,
                    ::std::cmp::Ordering::Equal => 0,
                    ::std::cmp::Ordering::Greater => 1,
                }
            }.into(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => unreachable!()
        };

        Ok(ret)
    }};
    ($lhs:ident, $operator:expr, $rhs:ident as $intermediate:ty) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        let ret = match $operator {
            BinaryOperatorKind::Plus => (lhs.wrapping_add(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Minus => (lhs.wrapping_sub(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Multiply => (lhs.wrapping_mul(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::Divide => (lhs.wrapping_div(rhs)).conv::<$intermediate>().into(),
            BinaryOperatorKind::More => (lhs > rhs).into(),
            BinaryOperatorKind::MoreEqual => (lhs >= rhs).into(),
            BinaryOperatorKind::Less => (lhs < rhs).into(),
            BinaryOperatorKind::LessEqual => (lhs <= rhs).into(),
            BinaryOperatorKind::ThreeWay => {
                match lhs.cmp(&rhs) {
                    ::std::cmp::Ordering::Less => -1,
                    ::std::cmp::Ordering::Equal => 0,
                    ::std::cmp::Ordering::Greater => 1,
                }
            }.conv::<$intermediate>().into(),
            BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual => unreachable!()
        };

        Ok(ret)
    }};
}

macro_rules! indicate_type_checker_bug {
    (context = $ctx:expr) => {
        unreachable!(
            "INTERNAL ERROR: this branch must not be reached, because this branch is executed after type-check'd. Please report this bug. Bug context: {x}",
            x = $ctx
        )
    };
}

impl CanBeEvaluated for Expression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            #[allow(clippy::cast_possible_truncation)]
            Self::IntLiteral { value: i, suffix } => suffix.as_ref().map_or_else(
                || Ok((*i).conv::<NonCoerced>().into()),
                |suffix| match suffix.as_ref() {
                    "i8" => Ok(((*i) as i8).into()),
                    "i16" => Ok(((*i) as i16).into()),
                    "i32" => Ok(((*i) as i32).into()),
                    "i64" => Ok((*i).conv::<Coerced>().into()),
                    _ => unreachable!()
                }),
            Self::BooleanLiteral(b) => Ok((*b).into()),
            Self::StringLiteral(s) => Ok(s.clone().into()),
            Self::UnitLiteral => Ok(().into()),
            Self::Variable { ident } => {
                runtime.search_member(ident)
                    .ok_or(RuntimeError::UndefinedVariable { identifier: ident.clone().into_boxed_str() })
            },
            Self::BinaryOperator { lhs, rhs, operator } => {
                let lhs = lhs.as_ref().evaluate(runtime)?;
                let rhs = rhs.as_ref().evaluate(runtime)?;
                if matches!(operator, BinaryOperatorKind::Equal | BinaryOperatorKind::NotEqual) {
                    return if lhs.get_type() == rhs.get_type() {
                        let ret = match operator {
                            BinaryOperatorKind::Equal => lhs == rhs,
                            BinaryOperatorKind::NotEqual => lhs != rhs,
                            _ => unreachable!(),
                        };
                        Ok(ret.into())
                    } else {
                        indicate_type_checker_bug!(context = "type checker must deny equality check between different types")
                    }
                }

                return match (lhs, rhs) {
                    (TypeBox::NonCoercedInteger(lhs), TypeBox::NonCoercedInteger(rhs)) => {
                        f!(lhs, operator, rhs as NonCoerced)
                    },
                    (TypeBox::Int8(lhs), TypeBox::Int8(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int16(lhs), TypeBox::Int16(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int32(lhs), TypeBox::Int32(rhs)) => {
                        f!(lhs, operator, rhs)
                    },
                    (TypeBox::Int64(lhs), TypeBox::Int64(rhs)) => {
                        f!(lhs, operator, rhs as Coerced)
                    },
                    (TypeBox::String(lhs), TypeBox::String(rhs)) => {
                        let mut ret = lhs;
                        // give hint to compiler
                        ret.reserve_exact(rhs.len());
                        ret += rhs.as_str();
                        Ok(ret.into())
                    }
                    _ => indicate_type_checker_bug!(context = "type checker must deny operator application between different type")
                };
            }
            Self::If { condition, then_clause_value, else_clause_value } => {
                let ret = condition.as_ref().evaluate(runtime)?;
                if let TypeBox::Boolean(b) = ret {
                    runtime.push_scope();
                    if b {
                        let v = then_clause_value.as_ref().evaluate(runtime);
                        runtime.pop_scope();
                        v
                    } else {
                        let v = else_clause_value.as_ref().evaluate(runtime);
                        runtime.pop_scope();
                        v
                    }
                } else {
                    indicate_type_checker_bug!(context = "if clause's expression must be Bool")
                }
            }
            Self::Block { intermediate_statements, final_expression } => {
                runtime.push_scope();
                for s in intermediate_statements {
                    runtime.what_will_happen1(s.clone()).iter().for_each(|x| x.invoke(runtime));
                }
                runtime.pop_scope();

                final_expression.as_ref().evaluate(runtime)
            }
            Self::Tuple { expressions } => {
                let mut res = Vec::with_capacity(expressions.len());
                for e in expressions {
                    res.push(e.evaluate(runtime)?);
                }

                Ok(TypeBox::Tuple(DisplayTuple {
                    boxes: res
                }))
            }
        }
    }
}

impl CanBeEvaluated for &Expression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        (*self).evaluate(runtime)
    }
}

impl CanBeEvaluated for Box<Expression> {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        self.as_ref().evaluate(runtime)
    }
}
