use crate::core::Index;

// an environment of bindings that can be looked up directly using an
// index, or by converting to a level using level_to_index
#[derive(Clone)]
pub struct Env<T> {
    vec: Vec<T>,
}

impl<T: Clone> Env<T> {
    pub fn lookup(&self, index: &Index) -> &T {
        match self.vec.get(*index) {
            Some(val) => val,
            None => panic!("bad index in env"),
        }
    }

    pub fn with(&self, t: T) -> Env<T> {
        let mut vec = self.vec.clone();
        vec.push(t);

        Env { vec }
    }
}
