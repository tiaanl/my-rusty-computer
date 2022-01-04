pub(crate) mod combinators;
pub(crate) mod instructions;
pub(crate) mod source;

pub(crate) enum ParseError {
    Unknown,

    InvalidNumberFormat,

    InvalidOperation,
    InvalidRegister,
    InvalidSegment,
}

impl nom::error::FromExternalError<&str, ParseError> for ParseError {
    fn from_external_error(_input: &str, _kind: nom::error::ErrorKind, _e: ParseError) -> Self {
        ParseError::Unknown
    }
}

pub(crate) type ParseResult<'a, T> = nom::IResult<&'a str, T>;
