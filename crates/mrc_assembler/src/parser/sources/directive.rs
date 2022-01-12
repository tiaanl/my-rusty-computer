use crate::{
    parse_identifier,
    parser::{tokens::parse_number, ParseResult, Span},
};
use nom::combinator::cut;
use nom::error::ParseError;
use nom::{
    character::complete::{multispace0, space1},
    sequence::terminated,
};
use std::fmt::Formatter;

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Directive {
    Bits(u16),
    Org(u32),
}

impl std::fmt::Display for Directive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Bits(bits) => write!(f, "BITS {}", bits),
            Directive::Org(origin) => write!(f, "ORG {}", origin),
        }
    }
}

pub(crate) fn parse_directive(input: Span) -> ParseResult<Directive> {
    let (input, identifier) = terminated(parse_identifier, space1)(input)?;

    let (input, value) = cut(terminated(parse_number, multispace0))(input)?;

    let directive = match *identifier.fragment() {
        "bits" => Directive::Bits(value as u16),
        "org" => Directive::Org(value as u32),
        _ => {
            return Err(nom::Err::Error(nom::error::Error::from_error_kind(
                input,
                nom::error::ErrorKind::Eof,
            )))
        }
    };

    Ok((input, directive))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span_at;

    #[test]
    fn directive() {
        assert_eq!(
            parse_directive(Span::new("bits 10")),
            Ok((span_at!(7, 1, ""), Directive::Bits(10)))
        );
        assert_eq!(
            parse_directive(Span::new("org 0x0100")),
            Ok((span_at!(10, 1, ""), Directive::Org(256)))
        );

        // Invalid argument to directive should be a failure.
        assert_eq!(
            parse_directive(Span::new("bits abc")),
            Err(nom::Err::Failure(nom::error::Error::from_error_kind(
                span_at!(5, 1, "abc"),
                nom::error::ErrorKind::OneOf
            )))
        );
    }
}
