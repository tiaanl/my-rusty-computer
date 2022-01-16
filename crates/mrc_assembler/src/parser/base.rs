//! Parsers for basic building blocks.

use crate::parser::{ParseResult, Span};
use nom::combinator::opt;
use nom::sequence::pair;
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{char, one_of},
    combinator::{map_res, recognize},
    multi::{many0, many1},
    sequence::preceded,
};

fn parse_number_with_radix(
    input: Span,
    digits: fn(input: Span) -> ParseResult<Span>,
    radix: u32,
) -> ParseResult<i32> {
    map_res(digits, |span| i32::from_str_radix(span.fragment(), radix))(input)
}

fn binary_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        preceded(tag_no_case("0b"), recognize(many1(one_of("01"))))(input)
    }

    parse_number_with_radix(input, digits, 2)
}

fn decimal_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        recognize(pair(opt(char('-')), many1(one_of("0123456789"))))(input)
    }

    parse_number_with_radix(input, digits, 10)
}

fn hexadecimal_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        preceded(
            tag_no_case("0x"),
            recognize(many1(one_of("0123456789abcdefABCDEF"))),
        )(input)
    }

    parse_number_with_radix(input, digits, 16)
}

pub fn number(input: Span) -> ParseResult<i32> {
    // Number formats with prefixes should go first!
    alt((binary_number, hexadecimal_number, decimal_number))(input)
}

pub fn identifier(input: Span) -> ParseResult<Span> {
    fn first_char(input: Span) -> ParseResult<Span> {
        recognize(many1(one_of(
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
        )))(input)
    }

    fn other_char(input: Span) -> ParseResult<Span> {
        recognize(many0(one_of(
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_1234567890",
        )))(input)
    }

    recognize(preceded(first_char, other_char))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_number() {
        assert_eq!(binary_number(Span::new("0b1011")).unwrap().1, 11);
        assert_eq!(binary_number(Span::new("0B1011")).unwrap().1, 11);
        assert!(binary_number(Span::new("-0b1011")).is_err());
    }

    #[test]
    fn test_decimal_number() {
        assert_eq!(decimal_number(Span::new("123")).unwrap().1, 123);
        assert_eq!(decimal_number(Span::new("-123")).unwrap().1, -123);
        assert_eq!(
            *decimal_number(Span::new("1-123")).unwrap().0.fragment(),
            "-123"
        );
        assert_eq!(decimal_number(Span::new("1-123")).unwrap().1, 1);
    }

    #[test]
    fn test_hexadecimal_number() {
        assert_eq!(hexadecimal_number(Span::new("0x10ab")).unwrap().1, 4267);
        assert_eq!(hexadecimal_number(Span::new("0X10aB")).unwrap().1, 4267);
        assert!(hexadecimal_number(Span::new("-0X10aB")).is_err());
    }

    #[test]
    fn test_number() {
        assert_eq!(number(Span::new("0b1011")).unwrap().1, 11);
        assert_eq!(number(Span::new("0x10ab")).unwrap().1, 4267);
        assert_eq!(number(Span::new("-123")).unwrap().1, -123);
    }

    #[test]
    fn test_identifier() {
        assert_eq!(
            *identifier(Span::new("hello, world!")).unwrap().0.fragment(),
            ", world!"
        );
        assert_eq!(
            *identifier(Span::new("hello, world!")).unwrap().1.fragment(),
            "hello"
        );

        assert_eq!(
            *identifier(Span::new("HELLO, WORLD!")).unwrap().0.fragment(),
            ", WORLD!"
        );
        assert_eq!(
            *identifier(Span::new("HELLO, WORLD!")).unwrap().1.fragment(),
            "HELLO"
        );

        assert_eq!(
            *identifier(Span::new("_is_valid")).unwrap().1.fragment(),
            "_is_valid"
        );

        assert_eq!(
            *identifier(Span::new("isValid78")).unwrap().1.fragment(),
            "isValid78"
        );

        assert!(identifier(Span::new("1notvalid")).is_err());
    }
}
