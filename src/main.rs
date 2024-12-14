use std::{collections::HashMap, rc::Rc};

mod datalog;
mod lc;
mod math;
mod stlc_bidirectional;
mod system_f_bidi;
mod topl_imperative;
mod ucs;
mod util;

fn main() {
    stlc_bidirectional::main();
}
