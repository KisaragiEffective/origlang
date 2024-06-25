#![deny(clippy::all)]
#![warn(clippy::pedantic, clippy::nursery)]

mod invoke_once;

use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Display, Formatter};
use derive_more::{Display, From};
use log::debug;
use tap::Conv;
use thiserror::Error;
use origlang_ast::Identifier;
use origlang_ast::after_parse::BinaryOperatorKind;
use origlang_ir::{CompiledTypedExpression, IR2};

use origlang_typesystem_model::{DisplayRecordType, Type, TypedIntLiteral};
use crate::invoke_once::InvokeOnce;

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
pub struct DisplayTupleValue {
    pub boxes: Vec<TypeBox>
}

impl Display for DisplayTupleValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self.boxes.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let s = format!("({s})");
        f.write_str(&s)
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct DisplayRecordValue {
    pub name: Identifier,
    pub values: Vec<TypeBox>,
}

impl Display for DisplayRecordValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self.values.iter().map(ToString::to_string).collect::<Vec<_>>().join(", ");
        let name = &self.name;
        let s = format!("{name} {{{s}}}");
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
    Unit,
    #[display(fmt = "{_0}")]
    #[from]
    Tuple(DisplayTupleValue),
    #[display(fmt = "{_0}")]
    #[from]
    Record(DisplayRecordValue),
}

impl TypeBox {
    #[must_use]
    pub fn get_type(&self) -> Type {
        match self {
            Self::NonCoercedInteger(_) => Type::GenericInteger,
            Self::Boolean(_) => Type::Boolean,
            Self::String(_) => Type::String,
            Self::Unit => Type::Unit,
            Self::Int8(_) => Type::Int8,
            Self::Int16(_) => Type::Int16,
            Self::Int32(_) => Type::Int32,
            Self::Int64(_) => Type::Int64,
            Self::Tuple(t) => Type::Tuple(t.boxes.iter().map(Self::get_type).collect::<Vec<_>>().into()),
            Self::Record(r) => Type::Record(
                DisplayRecordType::new(
                    r.name.clone(),
                    r.values.iter().map(Self::get_type).collect::<Vec<_>>()
                )
            ),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<Identifier, TypeBox>,
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

pub trait ExitSignalReceiver: Debug {
    fn on_exit(&self);
}

#[derive(Debug)]
pub struct Runtime {
    scopes: RefCell<VecDeque<Scope>>,
    o: Box<RefCell<dyn OutputAccumulator>>,
    on_exit: Option<InvokeOnce<Box<dyn ExitSignalReceiver>>>,
}

impl Runtime {
    pub fn create<T: OutputAccumulator + 'static>(t: T) -> Self {
        let mut scopes = VecDeque::new();
        scopes.push_front(Scope::empty());
        let scopes = RefCell::new(scopes);
        let o = Box::new(RefCell::new(t));
        Self {
            scopes,
            o,
            on_exit: None
        }
    }

    /// Start runtime. Never returns until execution is completed.
    pub fn start<'s: 'o, 'o>(&'s self, seq: &[IR2]) -> &'o RefCell<dyn OutputAccumulator> {
        // info!("{ast:?}", ast = &ast);
        let x = seq;
        // info!("{x:?}", x = &x);
        x
            .iter().for_each(|x| self.invoke(x));
        &self.o
    }

    pub fn execute(&self, ir: &[IR2]) {
        ir.iter().for_each(|x| self.invoke(x));
    }
    
    pub fn invoke(&self, ir: &IR2) {
        match ir {
            IR2::Output(e) => {
                self.o.as_ref().borrow_mut().output(e.evaluate(self).expect("runtime exception"));
            }
            IR2::UpdateVariable { ident, value } => {
                self.upsert_member_to_current_scope(
                    ident.clone(),
                    value.evaluate(self).expect("self exception")
                );
            }
            IR2::PushScope => {
                self.push_scope();
            }
            IR2::PopScope => {
                self.pop_scope();
            }
            IR2::Exit => {
                if let Some(x) = self.on_exit.as_ref() {
                    x.try_get().expect("exit receiver is already called!").on_exit();
                }
            }
            IR2::EvalAndForget { expression } => {
                self.evaluate(expression).expect("runtime exception");
            }
        }
    }

    /// # Errors
    /// If the evaluation was failed, this method will return proper [`RuntimeError`].
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

    fn search_member(&self, identifier: &Identifier) -> Option<TypeBox> {
        let x = self.scopes.borrow();
        x.iter().find_map(|x| x.variables.get(identifier)).cloned()
    }

    fn upsert_member_to_current_scope(&self, identifier: Identifier, value: TypeBox) {
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
        identifier: Identifier,
    },
}

type EvaluateResult = Result<TypeBox, RuntimeError>;
pub trait CanBeEvaluated {
    /// # Errors
    /// If the evaluation was failed, this method will return proper [`RuntimeError`].
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
            BinaryOperatorKind::ShiftLeft => (lhs << rhs).into(),
            BinaryOperatorKind::ShiftRight => (lhs >> rhs).into(),
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
            BinaryOperatorKind::ShiftLeft => (lhs << rhs).conv::<$intermediate>().into(),
            BinaryOperatorKind::ShiftRight => (lhs >> rhs).conv::<$intermediate>().into(),
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

fn evaluate_bin_op(runtime: &Runtime, lhs: &CompiledTypedExpression, rhs: &CompiledTypedExpression, operator: &BinaryOperatorKind) -> EvaluateResult {
    let lhs = lhs.evaluate(runtime)?;
    let rhs = rhs.evaluate(runtime)?;
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

    match (lhs, rhs) {
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
    }
}

impl CanBeEvaluated for CompiledTypedExpression {
    fn evaluate(&self, runtime: &Runtime) -> EvaluateResult {
        match self {
            #[allow(clippy::cast_possible_truncation)]
            Self::IntLiteral(int) => match int {
                TypedIntLiteral::Generic(i) => Ok(NonCoerced(*i).into()),
                TypedIntLiteral::Bit64(i) => Ok(Coerced(*i).into()),
                TypedIntLiteral::Bit32(i) => Ok((*i).into()),
                TypedIntLiteral::Bit16(i) => Ok((*i).into()),
                TypedIntLiteral::Bit8(i) => Ok((*i).into()),
            },
            Self::BooleanLiteral(b) => Ok((*b).into()),
            Self::StringLiteral(s) => Ok(s.clone().into()),
            Self::UnitLiteral => Ok(().into()),
            Self::Variable { ident, tp: _ } => {
                runtime.search_member(ident)
                    .ok_or(RuntimeError::UndefinedVariable { identifier: ident.clone() })
            },
            Self::BinaryOperator { lhs, rhs, operator, return_type: _ } => {
                evaluate_bin_op(runtime, lhs, rhs, operator)
            }
            Self::If { condition, then: then_clause_value, els: else_clause_value, return_type: _ } => {
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
            Self::Block { inner: intermediate_statements, final_expression, return_type: _ } => {
                runtime.push_scope();
                runtime.execute(intermediate_statements);
                runtime.pop_scope();

                final_expression.as_ref().evaluate(runtime)
            }
            Self::Tuple { expressions } => {
                let mut res = Vec::with_capacity(expressions.len());
                for e in expressions {
                    res.push(e.evaluate(runtime)?);
                }

                Ok(TypeBox::Tuple(DisplayTupleValue {
                    boxes: res
                }))
            }
            Self::ExtractTuple { expr, index } => {
                let x = expr.evaluate(runtime)?;
                match x {
                    TypeBox::Tuple(x) => {
                        x.boxes.get(*index).map_or_else(
                            || indicate_type_checker_bug!(context = "tuple_destruction: out of bounds"),
                            |x| Ok(x.clone())
                        )
                    }
                    _other => indicate_type_checker_bug!(context = "must be tuple")
                }
            }
        }
    }
}
