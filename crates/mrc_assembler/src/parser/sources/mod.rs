pub mod directive;
pub mod instruction;

use crate::parser::sources::directive::{parse_directive, Directive};
use crate::parser::sources::instruction::{parse_source_instruction, SourceInstruction};
use crate::{parse_identifier, ParseResult};
use nom::bytes::complete::{take_till, take_until};
use nom::character::complete::{alphanumeric0, alphanumeric1, multispace1, one_of};
use nom::combinator::{eof, recognize};
use nom::multi::many1;
use nom::sequence::preceded;
use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, combinator::map,
    sequence::terminated,
};
use std::fmt::Formatter;

fn parse_label(input: &str) -> ParseResult<&str> {
    terminated(terminated(parse_identifier, multispace0), tag(":"))(input)
}

fn parse_comment(input: &str) -> ParseResult<&str> {
    recognize(preceded(tag(";"), take_till(|c| c == '\n')))(input)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Line {
    Directive(Directive),
    Label(String),
    Instruction(SourceInstruction),
    Comment(String),
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Directive(directive) => write!(f, "{}", directive),
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "{:?}", instruction),
            Line::Comment(comment) => write!(f, "{}", comment),
        }
    }
}

pub(crate) fn parse_line(input: &str) -> ParseResult<Line> {
    terminated(
        alt((
            map(parse_label, |label| Line::Label(label.to_string())),
            map(parse_directive, Line::Directive),
            map(parse_source_instruction, Line::Instruction),
            map(parse_comment, |comment| Line::Comment(comment.to_string())),
        )),
        multispace0,
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn label() {
        assert_eq!(parse_label("test:"), Ok(("", "test")));
        assert_eq!(parse_label("test :"), Ok(("", "test")));
    }

    #[test]
    fn comment() {
        assert_eq!(
            parse_comment("; this is a comment"),
            Ok(("", "; this is a comment"))
        );
        assert_eq!(
            parse_comment("; this is a comment\nother stuff"),
            Ok(("\nother stuff", "; this is a comment"))
        );
    }

    #[test]
    fn line() {
        assert_eq!(
            parse_line("start:"),
            Ok(("", Line::Label("start".to_string())))
        );
        assert_eq!(
            parse_line("org 100"),
            Ok(("", Line::Directive(Directive::Org(100))))
        );
    }
}
