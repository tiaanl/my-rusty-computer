pub(crate) mod lines;
pub(crate) mod base;

pub use lines::parse_lines;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str>;

pub type ParseResult<'s, T> = nom::IResult<Span<'s>, T>;
