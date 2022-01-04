pub mod combinators;

pub(crate) enum ParseError {
    Unknown,

    InvalidNumberFormat,
}

impl nom::error::FromExternalError<&str, ParseError> for ParseError {
    fn from_external_error(_input: &str, _kind: nom::error::ErrorKind, _e: ParseError) -> Self {
        ParseError::Unknown
    }
}

pub(crate) type ParseResult<'a, T> = nom::IResult<&'a str, T>;
