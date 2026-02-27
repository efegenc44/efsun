use crate::{
    check::TypeCheckError,
    interner::Interner,
    parse::{ParseError, lex::LexError},
    resolution::ResolutionError,
    location::{Located, Span}
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
                ParseError::UnexpectedEOF => {
                    "Encountered unexpected EOF.".to_string()
                },
                ParseError::UnexpectedToken(token) => {
                    format!("Encountered unexpected token `{}`", token.display(interner))
                },
            },
            Self::Resolution(error) => match error {
                ResolutionError::UnboundPath(path) => {
                    format!("`{}` is not bound.", path.display(interner))
                },
                ResolutionError::MissingModuleDefinition => {
                    "Module definiton is missing.".to_string()
                },
                ResolutionError::UnresolvedImport(path) => {
                    format!("Import `{}` could not be resolved.", path.display(interner))
                },
            },
            Self::Check(error) => match error {
                TypeCheckError::TypeMismatch { first, second } => {
                    format!("Couldn't match type `{first}` with `{second}`")
                },
                TypeCheckError::CyclicDefinition(path) => {
                    format!("`{}` is defined cyclically", path.display(interner))
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

pub fn located_error<T: Into<Error>>(error: T, span: Span, source_name: String) -> (Located<Error>, String) {
    (Located::new(error.into(), span), source_name)
}

pub fn eof_error<T: Into<Error>>(error: T, source_name: String) -> (Located<Error>, String) {
    (Located::new(error.into(), Span::eof()), source_name)
}

impl Located<Error> {
    pub fn report(&self, source_name: &str, source: &str, interner: &Interner) {
        let mut lines = source.lines();

        let start = self.span().start();
        let end = self.span().end();

        if self.span().is_eof() {
            eprintln!();
            eprintln!("        | [{source_name}]");
            eprintln!("        |");
            eprintln!("        | {}", self.data().description(interner));
            return;
        }

        let first_line_number = start.row();

        eprintln!();
        eprintln!("        | [{source_name}:{first_line_number}:{}] ", start.column());
        eprintln!("        |");

        if start.row() == end.row() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..start.column()).len(),
                carrots = (start.column()..end.column()).len()
            );
            eprintln!("        | {}", self.data().description(interner))
        } else {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!("        | {:spaces$}{:^^carrots$}", "", "",
                spaces = (1..start.column()).len(),
                carrots = (start.column()..first_line.chars().count() + 1).len()
            );

            for line_number in (first_line_number + 1)..end.row() {
                let line = lines.next().unwrap();
                eprintln!("  {line_number:>5} | {line}");
                eprintln!("        | {:^^carrots$}", "",
                    carrots = line.chars().count()
                )
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}", end.row());
            eprintln!("        | {:^^carrots$}", "",
                carrots = (1..end.column()).len()
            );
            eprintln!("        | {}", self.data().description(interner))
        }
    }
}

pub type Result<T> = std::result::Result<T, (Located<Error>, String)>;

