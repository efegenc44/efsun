use std::fmt::Display;

#[derive(Clone, Copy, Debug)]
pub struct SourceLocation {
    row: usize,
    column: usize,
}

impl SourceLocation {
    pub fn start() -> Self {
        Self { row: 1, column: 1 }
    }

    pub fn eof() -> Self {
        Self { row: 0, column: 0 }
    }

    pub fn is_eof(&self) -> bool {
        self.row == 0 && self.column == 0
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn column(&self) -> usize {
        self.column
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

#[derive(Clone, Copy, Debug)]
pub struct Located<T> {
    data: T,
    start: SourceLocation,
    end: SourceLocation
}

impl<T> Located<T> {
    pub fn new(data: T, start: SourceLocation, end: SourceLocation) -> Self {
        Self { data, start, end }
    }

    pub fn destruct(self) -> (T, SourceLocation, SourceLocation) {
        (self.data, self.start, self.end)
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn start(&self) -> SourceLocation {
        self.start
    }

    pub fn end(&self) -> SourceLocation {
        self.end
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.data, self.start, self.end)
    }
}
