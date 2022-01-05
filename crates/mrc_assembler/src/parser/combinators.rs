use crate::parser::ParseResult;
use nom::character::complete::space0;
use nom::sequence::delimited;
use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{char, one_of},
    combinator::{map_res, recognize},
    multi::{many0, many1},
    sequence::preceded,
    IResult,
};

pub(crate) fn trim<'i, I, O, E>(inner: I) -> impl FnMut(&'i str) -> IResult<&'i str, O, E>
where
    I: Fn(&'i str) -> IResult<&'i str, O, E>,
    E: nom::error::ParseError<&'i str>,
{
    delimited(space0, inner, space0)
}

fn parse_number_with_radix(
    input: &str,
    digits: fn(input: &str) -> ParseResult<&str>,
    radix: u32,
) -> ParseResult<i32> {
    alt((
        map_res(preceded(char('-'), digits), |res| {
            i32::from_str_radix(res, radix).map(|n| -n)
        }),
        map_res(digits, |res| i32::from_str_radix(res, radix)),
    ))(input)
}

fn parse_binary_number(input: &str) -> ParseResult<i32> {
    fn digits(input: &str) -> ParseResult<&str> {
        preceded(tag_no_case("0b"), recognize(many1(one_of("01"))))(input)
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
            tag_no_case("0x"),
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
    fn first_char(input: &str) -> ParseResult<&str> {
        recognize(many1(one_of(
            "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_",
        )))(input)
    }

    fn other_char(input: &str) -> ParseResult<&str> {
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
        assert_eq!(parse_identifier("HELLO, WORLD!"), Ok((", WORLD!", "HELLO")));
        assert_eq!(parse_identifier("_is_valid"), Ok(("", "_is_valid")));
        assert_eq!(parse_identifier("isValid78"), Ok(("", "isValid78")));
        assert!(parse_identifier("1notvalid").is_err());
    }
}
