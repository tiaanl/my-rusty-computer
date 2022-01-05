use crate::parser::combinators::parse_number;
use crate::parser::ParseResult;
use crate::{parse_identifier, ParseError};
use mrc_instruction::{
    AddressingMode, Operand, OperandSize, OperandType, Operation, Segment, SizedRegister,
};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::multispace0;
use nom::combinator::{map, map_res, recognize};
use nom::sequence::{delimited, separated_pair};
use std::str::FromStr;

pub(crate) fn parse_operation(input: &str) -> ParseResult<Operation> {
    map_res(parse_identifier, |res| {
        Operation::from_str(res).map_err(|_| ParseError::InvalidOperation)
    })(input)
}

pub(crate) fn parse_register(input: &str) -> ParseResult<SizedRegister> {
    map_res(parse_identifier, |res| {
        SizedRegister::from_str(res).map_err(|_| ParseError::InvalidRegister)
    })(input)
}

pub(crate) fn parse_segment(input: &str) -> ParseResult<Segment> {
    map_res(parse_identifier, |res| {
        Segment::from_str(res).map_err(|_| ParseError::InvalidSegment)
    })(input)
}

pub(crate) fn parse_addressing_mode(input: &str) -> ParseResult<AddressingMode> {
    map_res(
        alt((
            recognize(separated_pair(
                parse_register,
                delimited(multispace0, tag("+"), multispace0),
                parse_register,
            )),
            parse_identifier,
        )),
        |res| match AddressingMode::from_str(res) {
            Ok(addressing_mode) => Ok(addressing_mode),
            Err(_) => Err(ParseError::InvalidAddressingMode),
        },
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::Register;

    #[test]
    fn operation() {
        assert_eq!(parse_operation("mov"), Ok(("", Operation::MOV)));
        assert_eq!(parse_operation("Add"), Ok(("", Operation::ADD)));
    }

    #[test]
    fn register() {
        assert_eq!(
            parse_register("ax"),
            Ok(("", SizedRegister(Register::AlAx, OperandSize::Word)))
        );
        assert_eq!(
            parse_register("Bl"),
            Ok(("", SizedRegister(Register::BlBx, OperandSize::Byte)))
        );
    }

    #[test]
    fn segment() {
        assert_eq!(parse_segment("cs"), Ok(("", Segment::CS)));
        assert_eq!(parse_segment("Ds"), Ok(("", Segment::DS)));
    }

    #[test]
    fn addressing_mode() {
        assert_eq!(
            parse_addressing_mode("si"),
            Ok(("", AddressingMode::Si))
        );
        assert_eq!(
            parse_addressing_mode("bp+di"),
            Ok(("", AddressingMode::BpDi))
        );
        assert_eq!(
            parse_addressing_mode("bx + si"),
            Ok(("", AddressingMode::BxSi))
        );
    }
}
