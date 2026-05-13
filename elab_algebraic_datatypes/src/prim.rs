use std::fmt::Display;

use crate::core::Val;

#[derive(Clone, PartialEq)]
pub enum Prim {
    BoolEq,

    IntEq,
    IntAdd,
    IntSub,
    IntMul,
    IntNeg,

    EnumEq,

    StructEq,

    Error,
}

impl Prim {
    pub fn apply(&self, args: &[Val]) -> Val {
        match self {
            Prim::BoolEq => todo!(),
            Prim::IntEq => todo!(),
            Prim::IntAdd => todo!(),
            Prim::IntSub => todo!(),
            Prim::IntMul => todo!(),
            Prim::IntNeg => todo!(),
            Prim::EnumEq => todo!(),
            Prim::StructEq => todo!(),
            Prim::Error => todo!(),
        }
    }
}

impl Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Prim::BoolEq | Prim::IntEq | Prim::EnumEq | Prim::StructEq => "==",
            Prim::IntAdd => "+",
            Prim::IntSub => "-",
            Prim::IntMul => "*",
            Prim::IntNeg => "-",
            Prim::Error => "!#!",
        }
        .fmt(f)
    }
}
