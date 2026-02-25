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
    start: SourceLocation,
    end: SourceLocation
}

impl Span {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self { start, end }
    }

    pub fn eof() -> Self {
        let zero = SourceLocation { row: 0, column: 0 };
        Self { start: zero, end: zero }
    }

    pub fn is_eof(&self) -> bool {
        self.start.row == 0
    }

    pub fn start(&self) -> SourceLocation {
        self.start
    }

    pub fn end(&self) -> SourceLocation {
        self.end
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.start, self.end)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Located<T> {
    data: T,
    span: Span,
}

impl<T> Located<T> {
    pub fn new(data: T, span: Span) -> Self {
        Self { data, span }
    }

    pub fn destruct(self) -> (T, Span) {
        (self.data, self.span)
    }

    pub fn as_data(self) -> T {
        self.data
    }

    pub fn data(&self) -> &T {
        &self.data
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

impl<T: Display> Display for Located<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.data, self.span)
    }
}
