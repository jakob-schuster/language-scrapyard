// A simple math language.

use std::rc::Rc;

pub fn main() {
    let e = Exp::BinOp {
        op: BinOp::Mod,
        e1: Rc::new(Exp::Num(3.0)),
        e2: Rc::new(Exp::Num(2.0)),
    };

    let v = eval(&e);

    println!("{:?}", v);
}

#[derive(Debug)]
enum Val {
    Num(f32),
}

#[derive(Debug)]
enum Exp {
    Num(f32),
    UnOp { op: UnOp, e: Rc<Exp> },
    BinOp { op: BinOp, e1: Rc<Exp>, e2: Rc<Exp> },
}

impl Exp {
    fn new_num(n: f32) -> Exp {
        Exp::Num(n)
    }
}

#[derive(Debug)]
enum UnOp {
    Neg,
}

#[derive(Debug)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

fn eval(e: &Exp) -> Val {
    match e {
        Exp::Num(n) => Val::Num(*n),
        Exp::UnOp { op, e } => match (op, eval(e)) {
            (UnOp::Neg, Val::Num(n)) => Val::Num(-1.0 * n),
        },
        Exp::BinOp { op, e1, e2 } => match (op, eval(e1), eval(e2)) {
            (BinOp::Add, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 + n2),
            (BinOp::Sub, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 - n2),
            (BinOp::Mul, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 * n2),
            (BinOp::Div, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 / n2),
            (BinOp::Mod, Val::Num(n1), Val::Num(n2)) => Val::Num(n1 % n2),
        },
    }
}
