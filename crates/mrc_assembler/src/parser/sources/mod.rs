pub(crate) mod directive;
pub(crate) mod instruction;

use crate::source::SourceInstruction;
use crate::{
    parse_identifier,
    parser::sources::{
        directive::{parse_directive, Directive},
        instruction::parse_source_instruction,
    },
    ParseResult,
};
use nom::{
    branch::alt,
    bytes::complete::take_till,
    character::{complete::char, complete::multispace0, complete::space0},
    combinator::{map, recognize},
    sequence::{preceded, terminated},
};
use std::fmt::Formatter;

fn parse_label(input: &str) -> ParseResult<&str> {
    terminated(terminated(parse_identifier, space0), char(':'))(input)
}

fn parse_comment(input: &str) -> ParseResult<&str> {
    recognize(preceded(char(';'), take_till(|c| c == '\n')))(input)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Line {
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
