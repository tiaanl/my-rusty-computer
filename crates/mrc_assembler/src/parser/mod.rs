pub(crate) mod combinators;
pub(crate) mod instructions;
pub(crate) mod sources;

pub(crate) type ParseResult<'s, T> = nom::IResult<&'s str, T>;
