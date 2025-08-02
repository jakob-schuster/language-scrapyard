use std::fmt::Display;

#[derive(Clone)]
pub enum Prim {
    BoolEq,
    IntEq,
    IntAdd,
    IntSub,
    IntMul,
    IntNeg,
}

impl Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prim::BoolEq | Prim::IntEq => "==",
            Prim::IntAdd => "+",
            Prim::IntSub => "-",
            Prim::IntMul => "*",
            Prim::IntNeg => "-",
        }
        .fmt(f)
    }
}
