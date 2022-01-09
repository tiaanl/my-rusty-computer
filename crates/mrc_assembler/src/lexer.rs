#![allow(dead_code)]

use nom::{
    branch::alt,
    bytes::complete::tag_no_case,
    character::complete::{alpha1, alphanumeric0, char, line_ending, one_of},
    combinator::{map, map_res, opt, recognize, value},
    multi::many1,
    sequence::{pair, preceded},
    IResult,
};
use std::fmt::Formatter;

#[derive(Clone, Copy)]
struct Location {
    line: usize,
    col: usize,
}

impl Location {
    fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

struct Input<'s> {
    location: Location,
    slice: &'s str,
}

impl<'s> Input<'s> {
    fn new(slice: &'s str) -> Self {
        Self {
            location: Location::default(),
            slice,
        }
    }
}

impl<'s> std::fmt::Display for Input<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}:{}) {}",
            self.location.line,
            self.location.col,
            &self.slice[..10],
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PunctuationKind {
    Colon,
    Comma,
    LeftBracket,
    Plus,
    RightBracket,
    SemiColon,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenKind<'a> {
    NewLine,
    Identifier(&'a str),
    Number(i32),
    Punctuation(PunctuationKind),
}

pub(crate) struct Token<'a> {
    location: Location,
    token: TokenKind<'a>,
}

fn new_line(input: &str) -> IResult<&str, TokenKind> {
    map(line_ending, |_| TokenKind::NewLine)(input)
}

fn punctuation(input: &str) -> IResult<&str, TokenKind> {
    use PunctuationKind::*;
    use TokenKind::Punctuation;

    alt((
        value(Punctuation(Colon), char(':')),
        value(Punctuation(Comma), char(',')),
        value(Punctuation(LeftBracket), char('[')),
        value(Punctuation(Plus), char('+')),
        value(Punctuation(RightBracket), char(']')),
        value(Punctuation(SemiColon), char(';')),
    ))(input)
}

fn identifier(input: &str) -> IResult<&str, TokenKind> {
    map(recognize(pair(alpha1, alphanumeric0)), |res| {
        TokenKind::Identifier(res)
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

fn binary_number(input: &str) -> IResult<&str, i32> {
    fn digits(input: &str) -> IResult<&str, &str> {
        preceded(tag_no_case("0b"), recognize(many1(one_of("01"))))(input)
    }

    number_with_radix(input, digits, 2)
}

fn decimal_number(input: &str) -> IResult<&str, i32> {
    fn digits(input: &str) -> IResult<&str, &str> {
        recognize(preceded(opt(char('-')), many1(one_of("0123456789"))))(input)
    }

    number_with_radix(input, digits, 10)
}

fn hexadecimal_number(input: &str) -> IResult<&str, i32> {
    fn digits(input: &str) -> IResult<&str, &str> {
        preceded(
            tag_no_case("0x"),
            recognize(many1(one_of("0123456789abcdefABCDEF"))),
        )(input)
    }

    number_with_radix(input, digits, 16)
}

fn number(input: &str) -> IResult<&str, TokenKind> {
    map(
        alt((binary_number, hexadecimal_number, decimal_number)),
        TokenKind::Number,
    )(input)
}

#[derive(Debug)]
enum ParseError {
    Unknown,
}

fn tokenize(input: &str) -> IResult<&str, Vec<TokenKind>> {
    let input = Input::new(input);

    identifier(input);

    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_new_line() {
        assert_eq!(new_line("\n"), Ok(("", TokenKind::NewLine))); // Unix-style
        assert_eq!(new_line("\r\n"), Ok(("", TokenKind::NewLine))); // Windows-style
        assert_eq!(new_line("\n\r"), Ok(("\r", TokenKind::NewLine))); // \r is invalid
    }

    #[test]
    fn parse_punctuation() {
        macro_rules! test_punctuation {
            ($s:expr,$t:ident) => {{
                assert_eq!(
                    punctuation($s),
                    Ok(("", TokenKind::Punctuation(PunctuationKind::$t)))
                );
            }};
        }

        test_punctuation!(":", Colon);
        test_punctuation!(",", Comma);
        test_punctuation!("[", LeftBracket);
        test_punctuation!("+", Plus);
        test_punctuation!("]", RightBracket);
        test_punctuation!(";", SemiColon);
    }

    #[test]
    fn parse_identifier() {
        assert_eq!(identifier("mov"), Ok(("", TokenKind::Identifier("mov"))));
        assert_eq!(identifier("pow2"), Ok(("", TokenKind::Identifier("pow2"))));
        assert!(identifier("2invalid").is_err());
    }

    #[test]
    fn parse_number() {
        assert_eq!(number("10"), Ok(("", TokenKind::Number(10))));
        assert_eq!(number("-10"), Ok(("", TokenKind::Number(-10))));
        assert_eq!(number("10-"), Ok(("-", TokenKind::Number(10))));
        assert_eq!(number("0x10"), Ok(("", TokenKind::Number(16))));
        assert_eq!(number("0b10"), Ok(("", TokenKind::Number(2))));
    }

    #[test]
    fn parse_strcmp() {
        let source = String::from_utf8_lossy(include_bytes!("../../../samples/strcmp.asm"));
        let (rest, tokens) = tokenize(source.as_ref()).unwrap();
        println!("{:?}", rest);
        println!("{:?}", tokens);
    }

    #[test]
    fn tokenizing() {
        assert_eq!(
            tokenize(
                r"push cs
                pop ds
                mov byte [bx:si + 8], cl"
            ),
            Ok((
                "",
                vec![
                    TokenKind::Identifier("push"),
                    TokenKind::Identifier("cs"),
                    TokenKind::NewLine,
                    TokenKind::Identifier("pop"),
                    TokenKind::Identifier("ds"),
                    TokenKind::NewLine,
                    TokenKind::Identifier("mov"),
                    TokenKind::Identifier("byte"),
                    TokenKind::Punctuation(PunctuationKind::LeftBracket),
                    TokenKind::Identifier("bx"),
                    TokenKind::Punctuation(PunctuationKind::Colon),
                    TokenKind::Identifier("si"),
                    TokenKind::Punctuation(PunctuationKind::Plus),
                    TokenKind::Number(8),
                    TokenKind::Punctuation(PunctuationKind::RightBracket),
                    TokenKind::Punctuation(PunctuationKind::Comma),
                    TokenKind::Identifier("cl"),
                ]
            ))
        );
    }
}
