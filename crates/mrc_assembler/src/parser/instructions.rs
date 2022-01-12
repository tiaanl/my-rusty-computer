use crate::parser::Span;
use crate::{parse_identifier, parser::ParseResult};
use mrc_instruction::{AddressingMode, Operation, Segment, SizedRegister};
use nom::character::complete::space0;
use nom::sequence::delimited;
use nom::{
    branch::alt,
    character::complete::char,
    combinator::{map_res, recognize},
    sequence::separated_pair,
};
use std::str::FromStr;

pub(crate) fn parse_operation(input: Span) -> ParseResult<Operation> {
    map_res(parse_identifier, |span| {
        Operation::from_str(span.fragment())
    })(input)
}

pub(crate) fn parse_register(input: Span) -> ParseResult<SizedRegister> {
    map_res(parse_identifier, |span| {
        SizedRegister::from_str(span.fragment())
    })(input)
}

pub(crate) fn parse_segment(input: Span) -> ParseResult<Segment> {
    map_res(parse_identifier, |span| Segment::from_str(span.fragment()))(input)
}

pub(crate) fn parse_addressing_mode(input: Span) -> ParseResult<AddressingMode> {
    map_res(
        alt((
            recognize(separated_pair(
                parse_register,
                delimited(space0, char('+'), space0),
                parse_register,
            )),
            parse_identifier,
        )),
        |span| AddressingMode::from_str(span.fragment()),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::{OperandSize, Register};
    use crate::test_span;

    #[test]
    fn operation() {
        test_span!(parse_operation, "mov", 1, 1, "", Operation::MOV);
        test_span!(parse_operation, "Add", 1, 1, "", Operation::ADD);
    }

    #[test]
    fn register() {
        assert_eq!(
            parse_register(Span::new("ax")),
            Ok((
                Span::new(""),
                SizedRegister(Register::AlAx, OperandSize::Word)
            ))
        );
        assert_eq!(
            parse_register(Span::new("Bl")),
            Ok((
                Span::new(""),
                SizedRegister(Register::BlBx, OperandSize::Byte)
            ))
        );
    }

    #[test]
    fn segment() {
        assert_eq!(
            parse_segment(Span::new("cs")),
            Ok((Span::new(""), Segment::CS))
        );
        assert_eq!(
            parse_segment(Span::new("Ds")),
            Ok((Span::new(""), Segment::DS))
        );
    }

    #[test]
    fn addressing_mode() {
        assert_eq!(
            parse_addressing_mode(Span::new("si")),
            Ok((Span::new(""), AddressingMode::Si))
        );
        assert_eq!(
            parse_addressing_mode(Span::new("bp+di")),
            Ok((Span::new(""), AddressingMode::BpDi))
        );
        assert_eq!(
            parse_addressing_mode(Span::new("bx + si")),
            Ok((Span::new(""), AddressingMode::BxSi))
        );
    }
}
