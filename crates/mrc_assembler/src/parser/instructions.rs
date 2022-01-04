use crate::parser::ParseResult;
use crate::{parse_identifier, parse_number, ParseError};
use mrc_instruction::{Operand, OperandSize, OperandType, Operation, Segment, SizedRegister};
use nom::branch::alt;
use nom::combinator::{map, map_res};
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

pub(crate) fn parse_operand(input: &str) -> ParseResult<Operand> {
    alt((
        map(parse_register, |SizedRegister(register, size)| {
            Operand(OperandType::Register(register), size)
        }),
        map(parse_segment, |segment| {
            Operand(OperandType::Segment(segment), OperandSize::Word)
        }),
        map(parse_number, |number| {
            Operand(OperandType::Immediate(number as u16), OperandSize::Word)
        }),
    ))(input)
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
    fn operand() {
        assert_eq!(
            parse_operand("bl",),
            Ok((
                "",
                Operand(OperandType::Register(Register::BlBx), OperandSize::Byte)
            ))
        );
        assert_eq!(
            parse_operand("cs",),
            Ok((
                "",
                Operand(OperandType::Segment(Segment::CS), OperandSize::Word)
            ))
        );
        assert_eq!(
            parse_operand("17",),
            Ok(("", Operand(OperandType::Immediate(17), OperandSize::Word)))
        );
    }
}
