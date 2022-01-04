use crate::parser::instructions::parse_operand;
use crate::parser::ParseResult;
use crate::{parse_identifier, parse_register};
use mrc_instruction::{AddressingMode, Operand};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::{map, recognize};
use nom::sequence::{delimited, terminated, tuple};
use std::str::FromStr;

pub(crate) fn parse_label(input: &str) -> ParseResult<&str> {
    terminated(terminated(parse_identifier, multispace0), tag(":"))(input)
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum MaybeOperand {
    Full(Operand),
    MaybeDirect(String),
    MaybeIndirect(AddressingMode),
}

fn parse_maybe_direct_indirect_operand(input: &str) -> ParseResult<MaybeOperand> {
    map(
        delimited(
            tag("["),
            delimited(
                multispace0,
                alt((
                    recognize(tuple((
                        parse_register,
                        delimited(multispace0, tag("+"), multispace0),
                        parse_register,
                    ))),
                    parse_identifier,
                )),
                multispace0,
            ),
            tag("]"),
        ),
        |res| match AddressingMode::from_str(res) {
            Ok(addressing_mode) => MaybeOperand::MaybeIndirect(addressing_mode),
            Err(_) => MaybeOperand::MaybeDirect(res.to_string()),
        },
    )(input)
}

pub(crate) fn parse_maybe_operand(input: &str) -> ParseResult<MaybeOperand> {
    alt((
        map(parse_operand, MaybeOperand::Full),
        parse_maybe_direct_indirect_operand,
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::{OperandSize, OperandType, Register, Segment};

    #[test]
    fn label() {
        assert_eq!(parse_label("test:"), Ok(("", "test")));
        assert_eq!(parse_label("test :"), Ok(("", "test")));
    }

    #[test]
    fn maybe_direct_indirect_operand() {
        assert_eq!(
            parse_maybe_direct_indirect_operand("[addr]"),
            Ok(("", MaybeOperand::MaybeDirect("addr".to_string())))
        );
        assert_eq!(
            parse_maybe_direct_indirect_operand("[ addr  ]"),
            Ok(("", MaybeOperand::MaybeDirect("addr".to_string())))
        );
        assert_eq!(
            parse_maybe_direct_indirect_operand("[some_addr ]"),
            Ok(("", MaybeOperand::MaybeDirect("some_addr".to_string())))
        );
        assert_eq!(
            parse_maybe_direct_indirect_operand("[bx+si]"),
            Ok(("", MaybeOperand::MaybeIndirect(AddressingMode::BxSi)))
        );
        assert_eq!(
            parse_maybe_direct_indirect_operand("[ bp + di ]"),
            Ok(("", MaybeOperand::MaybeIndirect(AddressingMode::BpDi)))
        );
    }

    #[test]
    fn maybe_operand() {
        assert_eq!(
            parse_maybe_operand("ax"),
            Ok((
                "",
                MaybeOperand::Full(Operand(
                    OperandType::Register(Register::AlAx),
                    OperandSize::Word
                ))
            ))
        );
        assert_eq!(
            parse_maybe_operand("cs"),
            Ok((
                "",
                MaybeOperand::Full(Operand(
                    OperandType::Segment(Segment::CS),
                    OperandSize::Word
                ))
            ))
        );
        assert_eq!(
            parse_maybe_operand("42"),
            Ok((
                "",
                MaybeOperand::Full(Operand(OperandType::Immediate(42), OperandSize::Word))
            ))
        );
        assert_eq!(
            parse_maybe_operand("[addr]"),
            Ok(("", MaybeOperand::MaybeDirect("addr".to_string())))
        );
        assert_eq!(
            parse_maybe_operand("[bx+si]"),
            Ok(("", MaybeOperand::MaybeIndirect(AddressingMode::BxSi)))
        );
    }
}
