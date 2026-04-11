use crate::{
    check::TypeCheckError,
    interner::{Interner, WithInterner},
    location::{Located, Span},
    parse::{ParseError, lex::LexError},
    resolution::ResolutionError,
};

fn lex_error_description(error: &LexError, _interner: &Interner) -> String {
    match error {
        LexError::UnknownStartOfAToken(unknown) => {
            format!("Encountered an unknown start of a token `{unknown}`")
        }
        LexError::UnterminatedStringLiteral => "Unterminated string literal".to_string(),
    }
}

fn parse_error_description(error: &ParseError, interner: &Interner) -> String {
    match error {
        ParseError::UnexpectedEOF => "Encountered unexpected EOF.".to_string(),
        ParseError::UnexpectedToken {
            unexpected,
            expected,
        } => {
            format!(
                "Encountered unexpected token `{}`, expected {}.",
                WithInterner::new(unexpected, interner),
                WithInterner::new(expected, interner),
            )
        }
        ParseError::UnexpectedTokenStart {
            unexpected,
            expected,
        } => {
            let mut message = format!(
                "Encountered unexpected token `{}`, expected ",
                WithInterner::new(unexpected, interner)
            );
            match expected {
                [] => unreachable!(),
                [token] => {
                    message.push_str(token.kind_string());
                }
                [tail @ .., head] => {
                    for token in tail {
                        message.push_str(&format!("either {}, ", token.kind_string()));
                    }
                    message.push_str(&format!("or {}", head.kind_string()));
                }
            }

            message
        }
        ParseError::IllFormedExpression => {
            "Ill formed expression, not all tokens are able to be consumed.".to_string()
        }
    }
}

fn resolution_error_description(error: &ResolutionError, interner: &Interner) -> String {
    match error {
        ResolutionError::UnboundPath(path) => {
            format!("`{}` is not bound.", WithInterner::new(path, interner))
        }
        ResolutionError::MissingModuleDefinition => "Module definiton is missing.".to_string(),
        ResolutionError::UnresolvedImport(path) => {
            format!(
                "Import `{}` could not be resolved.",
                WithInterner::new(path, interner)
            )
        }
    }
}

fn type_check_error_description(error: &TypeCheckError, interner: &Interner) -> String {
    match error {
        TypeCheckError::TypeMismatch { t1, t2 } => {
            format!(
                "Couldn't match type `{}` with `{}`",
                WithInterner::new(t1, interner),
                WithInterner::new(t2, interner),
            )
        }
        TypeCheckError::CyclicDefinition(path) => {
            format!(
                "`{}` is defined cyclically",
                WithInterner::new(path, interner)
            )
        }
        TypeCheckError::ExpectedStructure(expected) => {
            format!(
                "Can only apply to structures not `{}`",
                WithInterner::new(expected, interner)
            )
        }
        TypeCheckError::TypeArityMismatch { expected, found } => {
            format!("Expected {expected} number of type parameters but found {found}")
        }
    }
}

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
            Self::Lex(error) => lex_error_description(error, interner),
            Self::Parse(error) => parse_error_description(error, interner),
            Self::Resolution(error) => resolution_error_description(error, interner),
            Self::Check(error) => type_check_error_description(error, interner),
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

#[derive(Clone)]
pub struct ReportableError {
    error: Box<Located<Error>>,
    source_name: String,
}

impl ReportableError {
    pub fn new<E: Into<Error>>(error: E, span: Span, source_name: String) -> Self {
        Self {
            error: Box::new(Located::new(error.into(), span)),
            source_name,
        }
    }

    pub fn eof<E: Into<Error>>(error: E, source_name: String) -> Self {
        Self::new(error, Span::eof(), source_name)
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn report(&self, source: &str, interner: &Interner) {
        let Self { error, source_name } = self;

        let mut lines = source.lines();

        let start = error.span().start();
        let end = error.span().end();

        if error.span().is_eof() {
            eprintln!();
            eprintln!("        | [{source_name}]");
            eprintln!("        |");
            eprintln!("        | {}", error.data().description(interner));
            return;
        }

        let first_line_number = start.row();

        eprintln!();
        eprintln!(
            "        | [{source_name}:{first_line_number}:{}] ",
            start.column()
        );
        eprintln!("        |");

        if start.row() == end.row() {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!(
                "        | {:spaces$}{:^^carrots$}",
                "",
                "",
                spaces = (1..start.column()).len(),
                carrots = (start.column()..end.column()).len()
            );
            eprintln!("        | {}", error.data().description(interner))
        } else {
            let first_line = lines.nth(first_line_number - 1).unwrap();
            eprintln!("  {first_line_number:>5} | {first_line}");
            eprintln!(
                "        | {:spaces$}{:^^carrots$}",
                "",
                "",
                spaces = (1..start.column()).len(),
                carrots = (start.column()..first_line.chars().count() + 1).len()
            );

            for line_number in (first_line_number + 1)..end.row() {
                let line = lines.next().unwrap();
                eprintln!("  {line_number:>5} | {line}");
                eprintln!(
                    "        | {:^^carrots$}",
                    "",
                    carrots = line.chars().count()
                )
            }

            let last_line = lines.next().unwrap();
            eprintln!("  {:>5} | {last_line}", end.row());
            eprintln!(
                "        | {:^^carrots$}",
                "",
                carrots = (1..end.column()).len()
            );
            eprintln!("        | {}", error.data().description(interner))
        }
    }
}

pub type Result<T> = std::result::Result<T, ReportableError>;
