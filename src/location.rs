use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
pub struct SourceLocation {
    row: usize,
    column: usize
}

impl SourceLocation {
    pub fn start() -> Self {
        Self {
            row: 1,
            column: 1
        }
    }

    pub fn increment(&mut self) {
        self.column += 1;
    }

    pub fn newline(&mut self) {
        self.column = 1;
        self.row += 1;
    }
}

impl Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

#[derive(Debug)]
pub struct Located<T> {
    data: T,
    location: SourceLocation
}

impl<T> Located<T> {
    pub fn new(data: T, location: SourceLocation) -> Self {
        Self { data, location }
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.data, self.location)
    }
}
