use tap::Pipe;
use origlang_ir::IR1;

pub trait OptimizationPreset {
    /// 最適化をする。
    ///
    /// # Panics
    /// しない。
    fn optimize(seq: Vec<IR1>) -> Vec<IR1>;
}

/// Do not perform optimization at all.
pub struct NoOptimization;

impl OptimizationPreset for NoOptimization {
    fn optimize(seq: Vec<IR1>) -> Vec<IR1> {
        seq
    }
}

/// Enables some basic optimization.
pub struct SimpleOptimization;

impl OptimizationPreset for SimpleOptimization {
    fn optimize(seq: Vec<IR1>) -> Vec<IR1> {
        use super::ir1::*;
        seq
            .pipe(FoldBinaryOperatorInvocationWithConstant).pipe(|x| x.optimize())
            .pipe(FoldIfWithConstantCondition).pipe(|x| x.optimize())
            .pipe(InlineSimpleBlock).pipe(|x| x.optimize())
        
    }
}