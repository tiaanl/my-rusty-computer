pub(crate) mod combinators;
pub(crate) mod instructions;
pub(crate) mod sources;

pub(crate) type ParseResult<'a, T> = nom::IResult<&'a str, T>;
