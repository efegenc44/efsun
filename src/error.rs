use crate::{
    check::TypeCheckError,
    interner::Interner,
    parse::{ParseError, lex::LexError},
    resolver::ResolutionError,
    location::{Located, SourceLocation}
};

#[derive(Clone)]
pub enum Error {
    Lex(LexError),
    Parse(ParseError),
    Resolution(ResolutionError),
    Check(TypeCheckError),
}

impl Error {
    pub fn description(&self, interner: &Interner) -> String {
        match self {
            Self::Lex(error) => match error {
                LexError::UnknownStartOfAToken(unknown) => {
                    format!("Encountered an unknown start of a token `{unknown}`")
                },
                LexError::UnterminatedStringLiteral => {
                    "Unterminated string literal".to_string()
                }
            }
            Self::Parse(error) => match error {
                ParseError::LexError(error) => Self::Lex(*error).description(interner),
                ParseError::UnexpectedEOF => {
                    "Encountered unexpected EOF.".to_string()
                },
                ParseError::UnexpectedToken(token) => {
                    format!("Encountered unexpected token `{}`", token.display(interner))
                },
            },
            Self::Resolution(error) => match error {
                ResolutionError::UnboundIdentifier(id) => {
                    format!("Identifier `{}` is not bound.", interner.lookup(*id))
                },
            },
            Self::Check(error) => match error {
                TypeCheckError::TypeMismatch { first, second } => {
                    format!("Couldn't match type `{first}` with `{second}`")
                },
            },
        }
    }
}

impl From<LexError> for Error {
    fn from(value: LexError) -> Self {
        Self::Lex(value)
    }
}

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

impl From<ResolutionError> for Error {
    fn from(value: ResolutionError) -> Self {
        Self::Resolution(value)
    }
}

impl From<TypeCheckError> for Error {
    fn from(value: TypeCheckError) -> Self {
        Self::Check(value)
    }
}

pub fn located_error<T: Into<Error>>(error: T, start: SourceLocation, end: SourceLocation) -> Located<Error> {
    Located::new(error.into(), start, end)
}

impl Located<Error> {
    pub fn report(&self, source_name: &str, source: &str, interner: &Interner) {
        let mut lines = source.lines();

        if self.start().is_eof() {
            eprintln!();
            eprintln!("        | [{source_name}]");
            eprintln!("        |");
            eprintln!("        | {}", self.data().description(interner));
            return;
        }

        let first_line_number = self.start().row();

        eprintln!();
        eprintln!("        | [{source_name}:{first_line_number}:{}] ", self.start().column());
        eprintln!("        |");

        if self.start().row() == self.end().row() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..self.start().column()).len(),
                carrots = (self.start().column()..self.end().column()).len()
            );
            eprintln!("        | {}", self.data().description(interner))
        } else {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..self.start().column()).len(),
                carrots = (self.start().column()..first_line.chars().count() + 1).len()
            );

            for line_number in (first_line_number + 1)..self.end().row() {
                let line = lines.next().unwrap();
                eprintln!("  {line_number:>5} | {line}");
                eprintln!("        | {:^^carrots$}", "",
                    carrots = line.chars().count()
                )
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}", self.end().row());
            eprintln!("        | {:^^carrots$}", "",
                carrots = (1..self.end().column()).len()
            );
            eprintln!("        | {}", self.data().description(interner))
        }
    }
}

pub type Result<T> = std::result::Result<T, Located<Error>>;

