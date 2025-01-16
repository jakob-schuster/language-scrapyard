#[derive(Debug, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}

pub struct Located<T> {
    pub location: Location,
    pub data: T,
}
