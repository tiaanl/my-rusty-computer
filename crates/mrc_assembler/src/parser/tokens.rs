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

/*
#[derive(Debug)]
pub(crate) enum LiteralKind<'a> {
    String(&'a str),
    Number(i32),
}

#[derive(Debug)]
pub(crate) enum TokenKind<'a> {
    NewLine,
    Literal(LiteralKind<'a>),
    Identifier(&'a str),
}

#[derive(Debug)]
pub(crate) struct Token<'a> {
    offset: usize,
    line: usize,
    pub(crate) token: TokenKind<'a>,
}

impl<'a> Token<'a> {
    fn new(offset: usize, line: usize, token: TokenKind<'a>) -> Self {
        Self {
            offset,
            line,
            token,
        }
    }
}
*/

fn parse_number_with_radix(
    input: Span,
    digits: fn(input: Span) -> ParseResult<Span>,
    radix: u32,
) -> ParseResult<i32> {
    map_res(pair(opt(char('-')), digits), |(maybe_neg, span)| {
        i32::from_str_radix(span.fragment(), radix).map(
            |n| {
                if maybe_neg.is_some() {
                    -n
                } else {
                    n
                }
            },
        )
    })(input)
}

fn parse_binary_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        preceded(tag_no_case("0b"), recognize(many1(one_of("01"))))(input)
    }

    parse_number_with_radix(input, digits, 2)
}

fn parse_decimal_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        recognize(many1(one_of("0123456789")))(input)
    }

    parse_number_with_radix(input, digits, 10)
}

fn parse_hexadecimal_number(input: Span) -> ParseResult<i32> {
    fn digits(input: Span) -> ParseResult<Span> {
        preceded(
            tag_no_case("0x"),
            recognize(many1(one_of("0123456789abcdefABCDEF"))),
        )(input)
    }

    parse_number_with_radix(input, digits, 16)
}

pub(crate) fn parse_number(input: Span) -> ParseResult<i32> {
    // Number formats with prefixes should go first!
    alt((
        parse_binary_number,
        parse_hexadecimal_number,
        parse_decimal_number,
    ))(input)
}

pub(crate) fn parse_identifier(input: Span) -> ParseResult<Span> {
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
    use crate::test_span;

    #[test]
    fn binary_number() {
        test_span!(parse_binary_number, "0b1011", 6, 1, "", 11i32);
        test_span!(parse_binary_number, "0B1011", 6, 1, "", 11i32);
        test_span!(parse_binary_number, "-0b1011", 7, 1, "", -11i32);
    }

    #[test]
    fn decimal_number() {
        test_span!(parse_decimal_number, "123", 3, 1, "", 123i32);
        test_span!(parse_decimal_number, "-123", 4, 1, "", -123i32);
        test_span!(parse_decimal_number, "1-23", 1, 1, "-23", 1i32);
    }

    #[test]
    fn hexadecimal_number() {
        test_span!(parse_hexadecimal_number, "0x10ab", 6, 1, "", 4267i32);
        test_span!(parse_hexadecimal_number, "0X10aB", 6, 1, "", 4267i32);
        test_span!(parse_hexadecimal_number, "-0X10aB", 7, 1, "", -4267i32);
    }

    #[test]
    fn number() {
        test_span!(parse_number, "0b1011", 6, 1, "", 11i32);
        test_span!(parse_number, "0B1011", 6, 1, "", 11i32);
        test_span!(parse_number, "-0b1011", 7, 1, "", -11i32);

        test_span!(parse_number, "123", 3, 1, "", 123i32);
        test_span!(parse_number, "-123", 4, 1, "", -123i32);
        test_span!(parse_number, "1-23", 1, 1, "-23", 1i32);

        test_span!(parse_number, "0x10ab", 6, 1, "", 4267i32);
        test_span!(parse_number, "0X10aB", 6, 1, "", 4267i32);
        test_span!(parse_number, "-0X10aB", 7, 1, "", -4267i32);
    }

    #[test]
    fn identifier() {
        test_span!(
            parse_identifier,
            "hello, world!",
            5,
            1,
            ", world!",
            Span::new("hello")
        );
        test_span!(
            parse_identifier,
            "HELLO, WORLD!",
            5,
            1,
            ", WORLD!",
            Span::new("HELLO")
        );
        test_span!(
            parse_identifier,
            "_is_valid",
            9,
            1,
            "",
            Span::new("_is_valid")
        );
        test_span!(
            parse_identifier,
            "isValid78",
            9,
            1,
            "",
            Span::new("isValid78")
        );
        assert!(parse_identifier(Span::new("1notvalid")).is_err());
    }
}
