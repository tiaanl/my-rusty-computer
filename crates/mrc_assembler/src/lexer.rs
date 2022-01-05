#![allow(unused)]

use nom::{
    branch::alt,
    character::complete::{alpha1, alphanumeric0, char, line_ending, one_of, space0},
    combinator::{map, map_res, recognize},
    multi::many1,
    sequence::{pair, preceded, terminated},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
enum Token<'a> {
    NewLine,
    Identifier(&'a str),
    Number(i32),
}

fn identifier(input: &str) -> IResult<&str, Token> {
    map(recognize(pair(alpha1, alphanumeric0)), |res| {
        Token::Identifier(res)
    })(input)
}

fn number_with_radix(
    input: &str,
    digits: fn(input: &str) -> IResult<&str, &str>,
    radix: u32,
) -> IResult<&str, i32> {
    alt((
        map_res(preceded(char('-'), digits), |res| {
            i32::from_str_radix(res, radix)
                .map(|n| -n)
                .map_err(|_| nom::Err::Error(()))
        }),
        map_res(digits, |res| {
            i32::from_str_radix(res, radix).map_err(nom::Err::Error)
        }),
    ))(input)
}

fn decimal_number(input: &str) -> IResult<&str, i32> {
    fn digits(input: &str) -> IResult<&str, &str> {
        recognize(many1(one_of("0123456789")))(input)
    }

    number_with_radix(input, digits, 10)
}

fn number(input: &str) -> IResult<&str, Token> {
    map(decimal_number, Token::Number)(input)
}

fn source(input: &str) -> IResult<&str, Vec<Token>> {
    many1(terminated(
        alt((map(line_ending, |_| Token::NewLine), identifier, number)),
        space0,
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_source() {
        assert_eq!(
            source("id\nis 10"),
            Ok((
                "",
                vec![
                    Token::Identifier("id"),
                    Token::NewLine,
                    Token::Identifier("is"),
                    Token::Number(10),
                ]
            ))
        );
    }
}
