use origlang_ir::IR0;

pub struct EliminateAfterExit(pub Vec<IR0>);

impl EliminateAfterExit {
    pub fn optimize(self) -> Vec<IR0> {
        let mut x = self.0.into_iter().take_while(|x| *x != IR0::Exit).collect::<Vec<_>>();
        x.push(IR0::Exit);
        x
    }
}
