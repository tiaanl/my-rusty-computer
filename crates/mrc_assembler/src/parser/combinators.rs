use crate::parser::{ParseError, ParseResult};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::one_of;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::combinator::recognize;
use nom::multi::many1;
use nom::sequence::preceded;

fn parse_number_with_radix(
    input: &str,
    digits: fn(input: &str) -> ParseResult<&str>,
    radix: u32,
) -> ParseResult<i32> {
    alt((
        map_res(preceded(tag("-"), digits), |res| {
            i32::from_str_radix(res, radix)
                .map(|n| -n)
                .map_err(|_| ParseError::InvalidNumberFormat)
        }),
        map_res(digits, |res| {
            i32::from_str_radix(res, radix).map_err(|_| ParseError::InvalidNumberFormat)
        }),
    ))(input)
}

fn parse_binary_number(input: &str) -> ParseResult<i32> {
    fn digits(input: &str) -> ParseResult<&str> {
        preceded(alt((tag("0b"), tag("0B"))), recognize(many1(one_of("01"))))(input)
    }

    parse_number_with_radix(input, digits, 2)
}

fn parse_decimal_number(input: &str) -> ParseResult<i32> {
    fn digits(input: &str) -> ParseResult<&str> {
        recognize(many1(one_of("0123456789")))(input)
    }

    parse_number_with_radix(input, digits, 10)
}

fn parse_hexadecimal_number(input: &str) -> ParseResult<i32> {
    fn digits(input: &str) -> ParseResult<&str> {
        preceded(
            alt((tag("0x"), tag("0X"))),
            recognize(many1(one_of("0123456789abcdefABCDEF"))),
        )(input)
    }

    parse_number_with_radix(input, digits, 16)
}

pub(crate) fn parse_number(input: &str) -> ParseResult<i32> {
    // Number formats with prefixes should go first!
    alt((
        parse_binary_number,
        parse_hexadecimal_number,
        parse_decimal_number,
    ))(input)
}

pub(crate) fn parse_identifier(input: &str) -> ParseResult<&str> {
    alpha1(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binary_number() {
        assert_eq!(parse_binary_number("0b1011"), Ok(("", 11i32)));
        assert_eq!(parse_binary_number("0B1011"), Ok(("", 11i32)));
        assert_eq!(parse_binary_number("-0b1011"), Ok(("", -11i32)));
    }

    #[test]
    fn decimal_number() {
        assert_eq!(parse_decimal_number("123"), Ok(("", 123i32)));
        assert_eq!(parse_decimal_number("-123"), Ok(("", -123i32)));
        assert_eq!(parse_decimal_number("1-23"), Ok(("-23", 1i32)));
    }

    #[test]
    fn hexadecimal_number() {
        assert_eq!(parse_hexadecimal_number("0x10ab"), Ok(("", 4267i32)));
        assert_eq!(parse_hexadecimal_number("0X10aB"), Ok(("", 4267i32)));
        assert_eq!(parse_hexadecimal_number("-0X10aB"), Ok(("", -4267i32)));
    }

    #[test]
    fn number() {
        assert_eq!(parse_number("0b1011"), Ok(("", 11i32)));
        assert_eq!(parse_number("0B1011"), Ok(("", 11i32)));
        assert_eq!(parse_number("-0b1011"), Ok(("", -11i32)));

        assert_eq!(parse_number("123"), Ok(("", 123i32)));
        assert_eq!(parse_number("-123"), Ok(("", -123i32)));
        assert_eq!(parse_number("1-23"), Ok(("-23", 1i32)));

        assert_eq!(parse_number("0x10ab"), Ok(("", 4267i32)));
        assert_eq!(parse_number("0X10aB"), Ok(("", 4267i32)));
        assert_eq!(parse_number("-0X10aB"), Ok(("", -4267i32)));
    }

    #[test]
    fn identifier() {
        assert_eq!(parse_identifier("hello, world!"), Ok((", world!", "hello")));
    }
}
