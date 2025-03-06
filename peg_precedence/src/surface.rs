use std::rc::Rc;

use crate::util::Located;

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
