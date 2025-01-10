pub type Arena = bumpalo::Bump;

#[derive(Clone)]
pub struct Env<A> {
    vec: Vec<A>,
}

impl<A> Env<A> {
    pub fn iter(&self) -> std::slice::Iter<'_, A> {
        self.vec.iter()
    }

    pub fn get(&self, i: usize) -> &A {
        match self.vec.get(i) {
            Some(a) => a,
            None => panic!("Bad index in env!"),
        }
    }
}

impl<A: Eq> Env<A> {
    pub fn find_first(&self, a: &A) -> Option<usize> {
        for (i, b) in self.vec.iter().enumerate() {
            if b.eq(a) {
                return Some(i);
            }
        }
        None
    }

    pub fn find_last(&self, a: &A) -> Option<usize> {
        for (i, b) in self.vec.iter().enumerate().rev() {
            if b.eq(a) {
                return Some(i);
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use super::Env;

    #[test]
    fn find_first_some() {
        let env = Env {
            vec: vec![1, 2, 1, 3],
        };

        assert_eq!(env.find_first(&1), Some(0))
    }
    #[test]
    fn find_first_none() {
        let env = Env {
            vec: vec![1, 2, 1, 3],
        };

        assert_eq!(env.find_first(&6), None)
    }
    #[test]
    fn find_last_some() {
        let env = Env {
            vec: vec![1, 3, 2, 3],
        };

        assert_eq!(env.find_last(&3), Some(3))
    }
}

impl<A: Clone> Env<A> {
    pub fn with(&self, a: A) -> Env<A> {
        let mut vec = self.vec.clone();
        vec.push(a);

        Env { vec }
    }
}

impl<A> Default for Env<A> {
    fn default() -> Self {
        Env { vec: vec![] }
    }
}
