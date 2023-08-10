use origlang_ir::{CompiledTypedExpression, IntoVerbatimSequencedIR, IR0, IR1, IR2};
use origlang_typesystem_model::{TypedExpression, TypedStatement};
use crate::preset::OptimizationPreset;

// TODO: 最適化と診断の送信とloweringを一手に担う高レベルなAPIを用意する
pub struct TheTranspiler<'t> {
    pub optimization_preset: &'t dyn EachStep
}

impl<'t> TheTranspiler<'t> {
    pub fn new(preset: &'t dyn EachStep) -> Self {
        Self {
            optimization_preset: preset,
        }
    }

    pub fn optimizer(&self) -> &'t dyn EachStep {
        self.optimization_preset
    }
}

pub trait EachStep: OptimizationPreset<IR0> + OptimizationPreset<IR1> + OptimizationPreset<IR2> {}

impl<T: OptimizationPreset<IR0> + OptimizationPreset<IR1> + OptimizationPreset<IR2> + ?Sized> EachStep for T {}

impl TheTranspiler<'_> {
    fn compile_typed_expression(&self, te: TypedExpression) -> CompiledTypedExpression {
        match te {
            TypedExpression::IntLiteral(v) => CompiledTypedExpression::IntLiteral(v),
            TypedExpression::BooleanLiteral(v) => CompiledTypedExpression::BooleanLiteral(v),
            TypedExpression::StringLiteral(v) => CompiledTypedExpression::StringLiteral(v),
            TypedExpression::UnitLiteral => CompiledTypedExpression::UnitLiteral,
            TypedExpression::Variable { ident, tp } => CompiledTypedExpression::Variable {
                ident, tp
            },
            TypedExpression::BinaryOperator { lhs, rhs, operator, return_type } => CompiledTypedExpression::BinaryOperator {
                lhs: Box::new(self.compile_typed_expression(*lhs)),
                rhs: Box::new(self.compile_typed_expression(*rhs)),
                operator,
                return_type,
            },
            TypedExpression::If { condition, then, els, return_type } => CompiledTypedExpression::If {
                condition: Box::new(self.compile_typed_expression(*condition)),
                then: Box::new(self.compile_typed_expression(*then)),
                els: Box::new(self.compile_typed_expression(*els)),
                return_type,
            },
            TypedExpression::Block { inner, final_expression, return_type } => CompiledTypedExpression::Block {
                inner: inner.into_iter().flat_map(|x| self.compile_typed_statement(x)).collect(),
                final_expression: Box::new(self.compile_typed_expression(*final_expression)),
                return_type,
            },
            TypedExpression::Tuple { expressions } => CompiledTypedExpression::Tuple {
                expressions: expressions.into_iter().map(|x| self.compile_typed_expression(x)).collect()
            },
        }
    }

    fn compile_typed_statement(&self, ts: TypedStatement) -> Vec<IR2> {
        let opt_ir0 = self.optimization_preset.optimize(ts.into_ir());
        let ir1 = self.lower(opt_ir0);
        let opt_ir1 = OptimizationPreset::<IR1>::optimize(self.optimization_preset, ir1);
        let ir2 = self.lower(opt_ir1);

        OptimizationPreset::<IR2>::optimize(self.optimization_preset, ir2)
    }
}

pub trait LowerStep<From, To> {
    fn lower(&self, ir: Vec<From>) -> Vec<To>;
}

impl LowerStep<IR0, IR1> for TheTranspiler<'_> {
    fn lower(&self, ir: Vec<IR0>) -> Vec<IR1> {
        // TODO: this sequence is always terminated by IR0::Exit. Should be reflected on the type.
        ir.into_iter().map(|x| match x {
            IR0::Normal(ir1) => ir1,
            IR0::Exit => IR1::Exit,
        }).collect()
    }
}

impl LowerStep<IR1, IR2> for TheTranspiler<'_> {
    fn lower(&self, ir: Vec<IR1>) -> Vec<IR2> {
        ir.into_iter().map(|x| match x {
            IR1::Output(expr) => IR2::Output(self.compile_typed_expression(expr)),
            IR1::UpdateVariable { ident, value } => IR2::UpdateVariable { ident, value: self.compile_typed_expression(value) },
            IR1::PushScope => IR2::PushScope,
            IR1::PopScope => IR2::PopScope,
            IR1::Exit => IR2::Exit,
            IR1::EvalAndForget { expression } => IR2::EvalAndForget { expression: self.compile_typed_expression(expression) }
        }).collect()
    }
}
