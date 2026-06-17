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
pub struct Span {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.start, self.end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Located<T> {
    pub data: T,
    pub span: Span,
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.data, self.span)
    }
}
