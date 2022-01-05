use crate::{
    parse_identifier, parse_register,
    parser::{
        combinators::{parse_number, trim},
        instructions::{parse_addressing_mode, parse_operation, parse_segment},
    },
    ParseResult,
};
use mrc_instruction::{AddressingMode, OperandSize, Operation, Segment, SizedRegister};
use nom::combinator::opt;
use nom::{
    branch::alt,
    character::complete::{char, space0},
    combinator::{map, map_res},
    sequence::{delimited, separated_pair, terminated},
};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ValueOrLabel {
    Value(i32),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SourceOperand {
    Direct(ValueOrLabel, Option<OperandSize>),
    Indirect(AddressingMode, Option<OperandSize>),
    Register(SizedRegister),
    Segment(Segment),
    // SegmentAndOffset(u16, u16),
    Immediate(ValueOrLabel),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SourceOperandSet {
    None,
    Destination(SourceOperand),
    DestinationAndSource(SourceOperand, SourceOperand),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct SourceInstruction {
    pub(crate) operation: Operation,
    pub(crate) operand_set: SourceOperandSet,
}

fn parse_operand_size(input: &str) -> ParseResult<OperandSize> {
    map_res(parse_identifier, |res| match res.to_lowercase().as_str() {
        "byte" => Ok(OperandSize::Byte),
        "word" => Ok(OperandSize::Word),
        _ => Err(()),
    })(input)
}

fn parse_value_or_label(input: &str) -> ParseResult<ValueOrLabel> {
    alt((
        map(parse_number, ValueOrLabel::Value),
        map(parse_identifier, |label| {
            ValueOrLabel::Label(label.to_string())
        }),
    ))(input)
}

fn parse_source_direct_operand(input: &str) -> ParseResult<SourceOperand> {
    alt((map(
        separated_pair(
            opt(parse_operand_size),
            space0,
            delimited(char('['), trim(parse_value_or_label), char(']')),
        ),
        |(operand_size, value_or_label)| SourceOperand::Direct(value_or_label, operand_size),
    ),))(input)
}

fn parse_source_indirect_operand(input: &str) -> ParseResult<SourceOperand> {
    fn inner(input: &str) -> ParseResult<AddressingMode> {
        delimited(char('['), trim(parse_addressing_mode), char(']'))(input)
    }

    alt((
        map(
            separated_pair(parse_operand_size, space0, inner),
            |(operand_size, addressing_mode)| {
                SourceOperand::Indirect(addressing_mode, Some(operand_size))
            },
        ),
        map(inner, |addressing_mode| {
            SourceOperand::Indirect(addressing_mode, None)
        }),
    ))(input)
}

fn parse_source_register_operand(input: &str) -> ParseResult<SourceOperand> {
    map(parse_register, SourceOperand::Register)(input)
}

fn parse_source_segment_operand(input: &str) -> ParseResult<SourceOperand> {
    map(parse_segment, SourceOperand::Segment)(input)
}

fn parse_source_immediate_operand(input: &str) -> ParseResult<SourceOperand> {
    map(parse_value_or_label, SourceOperand::Immediate)(input)
}

fn parse_source_operand(input: &str) -> ParseResult<SourceOperand> {
    alt((
        parse_source_register_operand,
        parse_source_segment_operand,
        parse_source_indirect_operand,
        parse_source_direct_operand,
        parse_source_immediate_operand,
    ))(input)
}

fn parse_source_operand_set(input: &str) -> ParseResult<SourceOperandSet> {
    alt((
        // DestinationAndSource
        map(
            separated_pair(parse_source_operand, trim(char(',')), parse_source_operand),
            |(destination, source)| SourceOperandSet::DestinationAndSource(destination, source),
        ),
        // Destination
        map(parse_source_operand, SourceOperandSet::Destination),
    ))(input)
}

pub(crate) fn parse_source_instruction(input: &str) -> ParseResult<SourceInstruction> {
    let (input, operation) = terminated(parse_operation, space0)(input)?;

    let (input, operand_set) = match parse_source_operand_set(input) {
        Ok((input, operand_set)) => (input, operand_set),
        Err(_) => (input, SourceOperandSet::None),
    };

    Ok((
        input,
        SourceInstruction {
            operation,
            operand_set,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::{OperandSize, Register};

    #[test]
    fn source_direct_operand() {
        assert_eq!(
            parse_source_direct_operand("[addr]"),
            Ok((
                "",
                SourceOperand::Direct(ValueOrLabel::Label("addr".to_string()), None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand("[ addr  ]"),
            Ok((
                "",
                SourceOperand::Direct(ValueOrLabel::Label("addr".to_string()), None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand("[some_addr ]"),
            Ok((
                "",
                SourceOperand::Direct(ValueOrLabel::Label("some_addr".to_string()), None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand("[0x1000]"),
            Ok(("", SourceOperand::Direct(ValueOrLabel::Value(0x1000), None),))
        );
    }

    #[test]
    fn source_indirect_operand() {
        assert_eq!(
            parse_source_indirect_operand("[bx+si]"),
            Ok(("", SourceOperand::Indirect(AddressingMode::BxSi, None)))
        );
        assert_eq!(
            parse_source_indirect_operand("[ bp + di ]"),
            Ok(("", SourceOperand::Indirect(AddressingMode::BpDi, None)))
        );
        assert_eq!(
            parse_source_indirect_operand("byte [ bp + di ]"),
            Ok((
                "",
                SourceOperand::Indirect(AddressingMode::BpDi, Some(OperandSize::Byte))
            ))
        );
    }

    #[test]
    fn source_operand() {
        assert_eq!(
            parse_source_operand("ax"),
            Ok((
                "",
                SourceOperand::Register(SizedRegister(Register::AlAx, OperandSize::Word))
            ))
        );

        assert_eq!(
            parse_source_operand("cs"),
            Ok(("", SourceOperand::Segment(Segment::CS)))
        );

        assert_eq!(
            parse_source_operand("42"),
            Ok(("", SourceOperand::Immediate(ValueOrLabel::Value(42))))
        );

        assert_eq!(
            parse_source_operand("[addr]"),
            Ok((
                "",
                SourceOperand::Direct(ValueOrLabel::Label("addr".to_string()), None)
            ))
        );

        assert_eq!(
            parse_source_operand("[bx+si]"),
            Ok(("", SourceOperand::Indirect(AddressingMode::BxSi, None)))
        );
    }

    #[test]
    fn source_operand_set() {
        assert_eq!(
            parse_source_operand_set("ax, bx"),
            Ok((
                "",
                SourceOperandSet::DestinationAndSource(
                    SourceOperand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                    SourceOperand::Register(SizedRegister(Register::BlBx, OperandSize::Word)),
                )
            ))
        );

        assert_eq!(
            parse_source_operand_set("ax"),
            Ok((
                "",
                SourceOperandSet::Destination(SourceOperand::Register(SizedRegister(
                    Register::AlAx,
                    OperandSize::Word
                ))),
            ))
        );

        assert_eq!(
            parse_source_operand_set("[si]"),
            Ok((
                "",
                SourceOperandSet::Destination(SourceOperand::Indirect(AddressingMode::Si, None))
            ))
        );

        assert_eq!(
            parse_source_operand_set("func"),
            Ok((
                "",
                SourceOperandSet::Destination(SourceOperand::Immediate(ValueOrLabel::Label(
                    "func".to_string()
                )))
            ))
        );
    }

    #[test]
    fn source_instruction() {
        assert_eq!(
            parse_source_instruction("mov ax, bx"),
            Ok((
                "",
                SourceInstruction {
                    operation: Operation::MOV,
                    operand_set: SourceOperandSet::DestinationAndSource(
                        SourceOperand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                        SourceOperand::Register(SizedRegister(Register::BlBx, OperandSize::Word)),
                    )
                }
            ))
        );
    }
}
