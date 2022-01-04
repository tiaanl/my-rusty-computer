mod directive;
mod operand;

pub(crate) use directive::{parse_directive, Directive};

use crate::parser::sources::operand::MaybeOperand;
use crate::{parse_identifier, parser::ParseResult};
use mrc_instruction::Displacement;
use nom::character::complete::multispace1;
use nom::{
    branch::alt, bytes::complete::tag, character::complete::multispace0, combinator::map,
    sequence::terminated,
};
use std::fmt::Formatter;

fn parse_label(input: &str) -> ParseResult<&str> {
    terminated(terminated(parse_identifier, multispace0), tag(":"))(input)
}

#[derive(Clone, Debug, PartialEq)]
enum MaybeOperandSet {
    None,
    Destination(MaybeOperand),
    DestinationAndSource(MaybeOperand, MaybeOperand),
    Displacement(Displacement),
    SegmentAndOffset(u16, u16),
}

fn parse_maybe_operand_set(input: &str) -> ParseResult<MaybeOperandSet> {
    map(multispace1, |_| MaybeOperandSet::None)(input)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Line {
    Directive(Directive),
    Label(String),
    //Instruction(Instruction),
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Directive(directive) => write!(f, "{}", directive),
            Line::Label(label) => write!(f, "{}:", label),
            // Line::Instruction(instruction) => write!(f, "{}", instruction),
        }
    }
}

pub(crate) fn parse_line(input: &str) -> ParseResult<Line> {
    terminated(
        alt((
            map(parse_label, |s| Line::Label(s.to_string())),
            map(parse_directive, Line::Directive),
            // map(parse_instruction, Line::Instruction),
        )),
        multispace0,
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::{Operand, OperandSize, OperandType, Register};

    #[test]
    fn label() {
        assert_eq!(parse_label("test:"), Ok(("", "test")));
        assert_eq!(parse_label("test :"), Ok(("", "test")));
    }

    #[test]
    fn maybe_operand_set() {
        assert_eq!(
            parse_maybe_operand_set("ax, bx"),
            Ok((
                "",
                MaybeOperandSet::DestinationAndSource(
                    MaybeOperand::Full(Operand(
                        OperandType::Register(Register::AlAx),
                        OperandSize::Word
                    )),
                    MaybeOperand::Full(Operand(
                        OperandType::Register(Register::BlBx),
                        OperandSize::Word
                    ))
                )
            ))
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
