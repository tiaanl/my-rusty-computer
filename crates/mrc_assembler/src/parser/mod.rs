pub(crate) mod lines;
pub(crate) mod base;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str>;

pub type ParseResult<'s, T> = nom::IResult<Span<'s>, T>;
