//! Tokenizes a &str input into [Token] instances.

#![allow(dead_code)] // TODO: Remove me!

use nom::Err;

#[derive(Debug)]
pub enum Error {
    Unknown,
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind<'a> {
    Number(i32),
    String(&'a str),
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PunctuationKind {
    Colon,
    Comma,
    Dot,
    LeftBracket,
    Minus,
    Plus,
    RightBracket,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind<'a> {
    NewLine,
    Comment(&'a str),
    Identifier(&'a str),
    Literal(LiteralKind<'a>),
    Punctuation(PunctuationKind),
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub column: usize,
    pub line: usize,
    pub token: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(column: usize, line: usize, token: TokenKind<'a>) -> Self {
        Self {
            column,
            line,
            token,
        }
    }
}

mod parse {
    use super::*;
    use nom::multi::many0;
    use nom::{
        branch::alt,
        bytes::complete::{tag_no_case, take},
        character::complete::{alpha1, alphanumeric0, char, line_ending, one_of, space0},
        combinator::{map, map_res, opt, recognize},
        multi::many1,
        sequence::{pair, preceded, terminated},
        IResult, InputTakeAtPosition,
    };
    use nom_locate::LocatedSpan;

    type Span<'s> = LocatedSpan<&'s str>;

    type TokenResult<'a> = IResult<Span<'a>, Token<'a>>;

    fn parse_new_line(input: Span) -> TokenResult {
        map(line_ending, |span: Span<'_>| {
            Token::new(
                span.get_column(),
                span.location_line() as usize,
                TokenKind::NewLine,
            )
        })(input)
    }

    fn parse_comment(input: Span) -> TokenResult {
        let (_, _) = char(';')(input)?;

        let (input, comment) = input.split_at_position_complete(|c| c == '\n')?;

        Ok((
            input,
            Token::new(
                comment.get_column(),
                comment.location_line() as usize,
                TokenKind::Comment(comment.fragment()),
            ),
        ))
    }

    fn parse_identifier(input: Span) -> TokenResult {
        map(
            recognize(preceded(alpha1, alphanumeric0)),
            |span: Span<'_>| {
                Token::new(
                    span.get_column(),
                    span.location_line() as usize,
                    TokenKind::Identifier(span.fragment()),
                )
            },
        )(input)
    }

    fn parse_number_with_radix(
        input: Span,
        digits: fn(input: Span) -> IResult<Span, Span>,
        radix: u32,
    ) -> TokenResult {
        map_res(pair(opt(char('-')), digits), |(maybe_neg, span)| {
            i32::from_str_radix(span.fragment(), radix).map(|n| {
                Token::new(
                    span.get_column(),
                    span.location_line() as usize,
                    TokenKind::Literal(LiteralKind::Number(if maybe_neg.is_some() {
                        -n
                    } else {
                        n
                    })),
                )
            })
        })(input)
    }

    fn parse_binary_number(input: Span) -> TokenResult {
        fn digits(input: Span) -> IResult<Span, Span> {
            preceded(tag_no_case("0b"), recognize(many1(one_of("01"))))(input)
        }

        parse_number_with_radix(input, digits, 2)
    }

    fn parse_decimal_number(input: Span) -> TokenResult {
        fn digits(input: Span) -> IResult<Span, Span> {
            recognize(many1(one_of("0123456789")))(input)
        }

        parse_number_with_radix(input, digits, 10)
    }

    fn parse_hexadecimal_number(input: Span) -> TokenResult {
        fn digits(input: Span) -> IResult<Span, Span> {
            preceded(
                tag_no_case("0x"),
                recognize(many1(one_of("0123456789abcdefABCDEF"))),
            )(input)
        }

        parse_number_with_radix(input, digits, 16)
    }

    fn parse_number(input: Span) -> TokenResult {
        // Number formats with prefixes should go first!
        alt((
            parse_binary_number,
            parse_hexadecimal_number,
            parse_decimal_number,
        ))(input)
    }

    fn parse_literal(input: Span) -> TokenResult {
        // TODO: Parse string literals.
        parse_number(input)
    }

    fn parse_punctuation(input: Span) -> TokenResult {
        use PunctuationKind::*;
        const TABLE: [(&str, PunctuationKind); 7] = [
            (":", Colon),
            (",", Comma),
            (".", Dot),
            ("[", LeftBracket),
            ("-", Minus),
            ("+", Plus),
            ("]", RightBracket),
        ];

        map_res(take(1usize), |c: Span<'_>| {
            if let Some((_, p)) = TABLE.iter().find(|(s, _)| c.fragment() == s) {
                Ok(Token::new(
                    c.get_column(),
                    c.location_line() as usize,
                    TokenKind::Punctuation(*p),
                ))
            } else {
                Err(nom::Err::Error(()))
            }
        })(input)
    }

    fn parse_token(input: Span) -> TokenResult {
        alt((
            parse_comment,
            parse_literal,
            parse_punctuation,
            parse_new_line,
            parse_identifier,
        ))(input)
    }

    pub fn tokenize(input: &'_ str) -> IResult<Span, Vec<Token<'_>>> {
        many0(terminated(parse_token, space0))(Span::new(input))
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        macro_rules! span_at {
            ($offset:expr,$line:expr,$fragment:expr) => {{
                unsafe { Span::new_from_raw_offset($offset, $line, $fragment, ()) }
            }};
        }

        #[test]
        fn test_parse_comment() {
            assert_eq!(
                parse_comment(Span::new("; This is a comment")).unwrap(),
                (
                    span_at!(19, 1, ""),
                    Token::new(1, 1, TokenKind::Comment("; This is a comment"))
                )
            );
            assert_eq!(
                parse_comment(Span::new("; This is a comment\nmore")).unwrap(),
                (
                    span_at!(19, 1, "\nmore"),
                    Token::new(1, 1, TokenKind::Comment("; This is a comment"))
                )
            );
        }
    }
}

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<Token<'a>>, Error> {
    let (_, tokens) = parse::tokenize(input).map_err(|err| {
        match err {
            Err::Incomplete(err) => eprintln!("Incomplete: {:?}", err),
            Err::Error(err) => eprintln!("Error: {:?}", err),
            Err::Failure(err) => eprintln!("Failure: {:?}", err),
        }

        Error::Unknown
    })?;

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize() {
        // println!("{:#?}", tokenize("This is a\nsentence.").unwrap());
        println!("{:#?}", tokenize("mov ax, bx").unwrap());
    }
}
