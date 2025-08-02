use std::rc::Rc;

use crate::util::Located;

pub type Prog = Located<ProgData>;
#[derive(Clone, Debug)]
pub struct ProgData {
    pub stmt: Stmt,
}

pub type Stmt = Located<StmtData>;
#[derive(Clone, Debug)]
pub enum StmtData {
    If {
        branches: Vec<Branch>,
        next: Rc<Stmt>,
    },
    Tm {
        tm: Tm,
        next: Rc<Stmt>,
    },
    Group {
        stmt: Rc<Stmt>,
        next: Rc<Stmt>,
    },
    End,
}

pub type Branch = Located<BranchData>;
#[derive(Clone, Debug)]
pub enum BranchData {
    Bool { tm: Tm, stmt: Stmt },
}

pub type Tm = Located<TmData>;
#[derive(Clone, Debug)]
pub enum TmData {
    //
    Name { name: String },
    Proj { tm: Rc<Tm>, name: String },

    //
    FunCall { tm: Rc<Tm>, args: Vec<Tm> },

    //
    Num { n: i32 },

    //
    Mul { tm0: Rc<Tm>, tm1: Rc<Tm> },
    Div { tm0: Rc<Tm>, tm1: Rc<Tm> },

    //
    Add { tm0: Rc<Tm>, tm1: Rc<Tm> },
    Sub { tm0: Rc<Tm>, tm1: Rc<Tm> },
}
