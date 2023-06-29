use origlang_ir::{IR0, IR1};

pub struct TheTranspiler;

pub trait LowerStep {
    type From;
    type To;

    fn lower(&self, ir: Vec<Self::From>) -> Vec<Self::To>;
}

impl LowerStep for TheTranspiler {
    type From = IR0;
    type To = IR1;

    fn lower(&self, ir: Vec<Self::From>) -> Vec<Self::To> {
        // TODO: this sequence is always terminated by IR0::Exit. Should be reflected on the type.
        ir.into_iter().map(|x| match x {
            IR0::Normal(ir1) => ir1,
            IR0::Exit => IR1::Exit,
        }).collect()
    }
}
