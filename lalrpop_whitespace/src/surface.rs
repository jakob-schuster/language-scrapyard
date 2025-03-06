use crate::util::Located;

#[derive(Debug)]
pub struct Prog {
    pub stmts: Vec<Stmt>,
}

pub type Stmt = Located<StmtData>;
#[derive(Debug)]
pub enum StmtData {
    Let { name: String, tm: Tm },
    Print { tm: Tm },
}

pub type Tm = Located<TmData>;
#[derive(Debug)]
pub enum TmData {
    Name { name: String },

    BoolLit { b: bool },
    IntLit { i: i32 },
}
