use crate::{parse_identifier, parse_number, parser::ParseResult};
use nom::{
    character::complete::multispace1,
    combinator::map_res,
    sequence::{terminated, tuple},
};
use std::fmt::Formatter;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Directive {
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

pub(crate) fn parse_directive(input: &str) -> ParseResult<Directive> {
    map_res(
        tuple((terminated(parse_identifier, multispace1), parse_number)),
        |(s, v)| match String::from(s).to_lowercase().as_str() {
            "bits" => Ok(Directive::Bits(v as u16)),
            "org" => Ok(Directive::Org(v as u32)),
            _ => Err(()),
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn directive() {
        assert_eq!(parse_directive("bits 10"), Ok(("", Directive::Bits(10))));
        assert_eq!(parse_directive("org 0x0100"), Ok(("", Directive::Org(256))));
    }
}
