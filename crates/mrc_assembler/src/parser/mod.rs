pub(crate) mod combinators;
pub(crate) mod instructions;
pub(crate) mod sources;

pub(crate) enum ParseError {
    InvalidNumberFormat,

    InvalidOperation,
    InvalidRegister,
    InvalidSegment,
    InvalidAddressingMode,
    InvalidOperandSize,
}

pub(crate) type ParseResult<'a, T> = nom::IResult<&'a str, T>;
