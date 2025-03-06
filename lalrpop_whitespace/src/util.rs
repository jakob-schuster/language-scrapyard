use std::fmt::Display;

use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Location {
        Location { start, end }
    }
}

#[derive(Clone, Debug)]
pub struct Located<T> {
    pub location: Location,
    pub data: T,
}

impl<T> Located<T> {
    pub fn new(location: Location, data: T) -> Located<T> {
        Located { location, data }
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.data.fmt(f)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RecField<T> {
    pub name: String,
    pub data: T,
}

impl<T> RecField<T> {
    pub fn new(name: String, data: T) -> RecField<T> {
        RecField { name, data }
    }
}

impl<T: Display> Display for RecField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!("{} = {}", self.name, self.data).fmt(f)
    }
}

#[derive(Clone)]
pub struct Env<A> {
    vec: Vec<A>,
}

impl<A: std::fmt::Display> Env<A> {
    pub fn iter(&self) -> std::slice::Iter<'_, A> {
        self.vec.iter()
    }

    pub fn get_level(&self, i: usize) -> &A {
        match self.vec.get(i) {
            Some(a) => a,
            None => panic!("Bad index in env!"),
        }
    }

    pub fn get_index(&self, i: usize) -> &A {
        match self.vec.get(self.vec.len() - 1 - i) {
            Some(a) => a,
            None => panic!("Bad index in env!"),
        }
    }
}

impl<A: Display> Display for Env<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format!(
            "{{ {} }}",
            self.iter()
                .map(|a| a.to_string())
                .collect::<Vec<_>>()
                .join(" ")
        )
        .fmt(f)
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
