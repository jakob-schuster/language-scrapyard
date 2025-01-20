use std::fmt::Display;

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

pub struct Located<T> {
    pub location: Location,
    pub data: T,
}

impl<T> Located<T> {
    pub fn new(location: Location, data: T) -> Located<T> {
        Located { location, data }
    }
}

#[derive(Clone, PartialEq, Eq)]
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
