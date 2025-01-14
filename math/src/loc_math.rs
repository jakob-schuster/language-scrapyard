// A simple math language, keeping track of location data for friendly error messages.

use std::rc::Rc;

#[derive(Debug)]
enum Val {
    Num(f32),
}

#[derive(Debug)]
pub struct Located<T> {
    pub start: usize,
    pub end: usize,
    pub data: T,
}

pub type Exp = Located<ExpData>;
#[derive(Debug)]
pub enum ExpData {
    Num(f32),
    UnOp { op: UnOp, e: Rc<Exp> },
    BinOp { op: BinOp, e1: Rc<Exp>, e2: Rc<Exp> },
}

impl ExpData {
    fn new_num(n: f32) -> ExpData {
        ExpData::Num(n)
    }
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

fn eval(e: &Exp) -> Val {
    match &e.data {
        ExpData::Num(n) => Val::Num(*n),
        ExpData::UnOp { op, e } => match (op, eval(e)) {
            (UnOp::Neg, Val::Num(n)) => Val::Num(-1.0 * n),
        },
        ExpData::BinOp { op, e1, e2 } => match (op, eval(e1), eval(e2)) {
            (BinOp::Add, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 + n2),
            (BinOp::Sub, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 - n2),
            (BinOp::Mul, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 * n2),
            (BinOp::Div, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 / n2),
            (BinOp::Mod, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 % n2),
        },
    }
}
