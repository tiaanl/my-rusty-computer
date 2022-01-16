pub(crate) mod directive;
pub(crate) mod instruction;

use crate::{
    identifier,
    parser::{
        sources::{
            directive::{parse_directive, Directive},
            instruction::parse_source_instruction,
        },
        Span,
    },
    ast::Instruction,
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

// #[derive(Debug)]
// pub(crate) enum CodeError {
//     UnexpectedToken,
// }

fn parse_label(input: Span) -> ParseResult<Span> {
    terminated(terminated(identifier, space0), char(':'))(input)
}

fn parse_comment(input: Span) -> ParseResult<Span> {
    recognize(preceded(char(';'), take_till(|c| c == '\n')))(input)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Line {
    Directive(Directive),
    Label(String),
    Instruction(Instruction),
    Comment(String),
}

impl<'a> std::fmt::Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Directive(directive) => write!(f, "{}", directive),
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "{:?}", instruction),
            Line::Comment(comment) => write!(f, "{}", comment),
        }
    }
}

pub(crate) fn parse_line(input: Span) -> ParseResult<Line> {
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
    use crate::{span_at, test_span};

    #[test]
    fn label() {
        assert_eq!(
            parse_label(Span::new("test:")),
            Ok((Span::new(""), Span::new("test")))
        );
        assert_eq!(
            parse_label(Span::new("test :")),
            Ok((Span::new(""), Span::new("test")))
        );
    }

    #[test]
    fn comment() {
        test_span!(
            parse_comment,
            "; this is a comment",
            19,
            1,
            "",
            Span::new("; this is a comment")
        );
        assert_eq!(
            parse_comment(Span::new("; this is a comment\nother stuff")),
            Ok((Span::new("\nother stuff"), Span::new("; this is a comment")))
        );
    }

    #[test]
    fn line() {
        // assert_eq!(
        //     parse_line(Span::new("start:")),
        //     Ok((Span::new(""), Line::Label("start".to_string())))
        // );
        assert_eq!(
            parse_line(Span::new("org 100")),
            Ok((span_at!(7, 1, ""), Line::Directive(Directive::Org(100))))
        );
    }
}
