use tap::Pipe;
use origlang_ir::{IR1, IR2};

pub trait OptimizationPreset<IR> {
    /// 最適化をする。
    ///
    /// # Panics
    /// しない。
    #[must_use = "return value is optimized IR, dropping it will waste it"]
    fn optimize(&self, seq: Vec<IR>) -> Vec<IR>;
}

/// Do not perform optimization at all.
pub struct NoOptimization;

impl OptimizationPreset<IR1> for NoOptimization {
    fn optimize(&self, seq: Vec<IR1>) -> Vec<IR1> {
        #[allow(unused_imports)]
        use crate::ir1::*;

        seq
            .pipe(EliminateAfterExit).pipe(|x| x.optimize())
    }
}

impl OptimizationPreset<IR2> for NoOptimization {
    fn optimize(&self, seq: Vec<IR2>) -> Vec<IR2> {
        seq
    }
}

/// Enables some basic optimization.
pub struct SimpleOptimization;

impl OptimizationPreset<IR1> for SimpleOptimization {
    #[allow(clippy::redundant_closure_for_method_calls)]
    fn optimize(&self, seq: Vec<IR1>) -> Vec<IR1> {
        #[allow(clippy::wildcard_imports)]
        use crate::ir1::*;

        seq
            .pipe(FoldBinaryOperatorInvocationWithConstant).pipe(|x| x.optimize())
            .pipe(FoldIfWithConstantCondition).pipe(|x| x.optimize())
            .pipe(InlineSimpleBlock).pipe(|x| x.optimize())
            .pipe(EliminateAfterExit).pipe(|x| x.optimize())
    }
}

impl OptimizationPreset<IR2> for SimpleOptimization {
    fn optimize(&self, seq: Vec<IR2>) -> Vec<IR2> {
        seq
    }
}
