use crate::parser::Span;
use crate::{
    ast::{Instruction, Operand, OperandSet, ValueOrLabel},
    identifier, parse_register,
    parser::{
        base::number,
        instructions::{parse_addressing_mode, parse_operation, parse_segment},
    },
    ParseResult,
};
use mrc_instruction::{AddressingMode, OperandSize};
use nom::{
    branch::alt,
    character::complete::{char, space0},
    combinator::{map, map_res, opt},
    sequence::{delimited, separated_pair, terminated},
};

fn parse_operand_size(input: Span) -> ParseResult<OperandSize> {
    map_res(identifier, |res| match res.to_lowercase().as_str() {
        "byte" => Ok(OperandSize::Byte),
        "word" => Ok(OperandSize::Word),
        _ => Err(()),
    })(input)
}

fn parse_value_or_label(input: Span) -> ParseResult<ValueOrLabel> {
    alt((
        map(number, ValueOrLabel::Value),
        map(identifier, |span| {
            ValueOrLabel::Label(span.fragment().to_string())
        }),
    ))(input)
}

fn parse_source_direct_operand(input: Span) -> ParseResult<Operand> {
    alt((map(
        separated_pair(
            opt(parse_operand_size),
            space0,
            delimited(
                char('['),
                delimited(space0, parse_value_or_label, space0),
                char(']'),
            ),
        ),
        |(operand_size, value_or_label)| Operand::Direct(value_or_label, operand_size, None),
    ),))(input)
}

fn parse_source_indirect_operand(input: Span) -> ParseResult<Operand> {
    fn inner(input: Span) -> ParseResult<AddressingMode> {
        delimited(
            char('['),
            delimited(space0, parse_addressing_mode, space0),
            char(']'),
        )(input)
    }

    alt((
        map(
            separated_pair(parse_operand_size, space0, inner),
            |(operand_size, addressing_mode)| {
                Operand::Indirect(addressing_mode, Some(operand_size), None)
            },
        ),
        map(inner, |addressing_mode| {
            Operand::Indirect(addressing_mode, None, None)
        }),
    ))(input)
}

fn parse_source_register_operand(input: Span) -> ParseResult<Operand> {
    map(parse_register, Operand::Register)(input)
}

fn parse_source_segment_operand(input: Span) -> ParseResult<Operand> {
    map(parse_segment, Operand::Segment)(input)
}

fn parse_source_immediate_operand(input: Span) -> ParseResult<Operand> {
    map(parse_value_or_label, Operand::Immediate)(input)
}

fn parse_source_operand(input: Span) -> ParseResult<Operand> {
    alt((
        parse_source_register_operand,
        parse_source_segment_operand,
        parse_source_indirect_operand,
        parse_source_direct_operand,
        parse_source_immediate_operand,
    ))(input)
}

fn parse_source_operand_set(input: Span) -> ParseResult<OperandSet> {
    alt((
        // DestinationAndSource
        map(
            separated_pair(
                parse_source_operand,
                delimited(space0, char(','), space0),
                parse_source_operand,
            ),
            |(destination, source)| OperandSet::DestinationAndSource(destination, source),
        ),
        // Destination
        map(parse_source_operand, OperandSet::Destination),
    ))(input)
}

pub(crate) fn parse_source_instruction(input: Span) -> ParseResult<Instruction> {
    let (input, operation) = terminated(parse_operation, space0)(input)?;

    let (input, operand_set) = match parse_source_operand_set(input) {
        Ok((input, operand_set)) => (input, operand_set),
        Err(_) => (input, OperandSet::None),
    };

    Ok((input, Instruction::new(operation, operand_set)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use mrc_instruction::{OperandSize, Operation, Register, Segment, SizedRegister};

    #[test]
    fn source_direct_operand() {
        assert_eq!(
            parse_source_direct_operand(Span::new("[addr]")),
            Ok((
                Span::new(""),
                Operand::Direct(ValueOrLabel::Label("addr".to_string()), None, None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand(Span::new("[ addr  ]")),
            Ok((
                Span::new(""),
                Operand::Direct(ValueOrLabel::Label("addr".to_string()), None, None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand(Span::new("[some_addr ]")),
            Ok((
                Span::new(""),
                Operand::Direct(ValueOrLabel::Label("some_addr".to_string()), None, None)
            ))
        );
        assert_eq!(
            parse_source_direct_operand(Span::new("[0x1000]")),
            Ok((
                Span::new(""),
                Operand::Direct(ValueOrLabel::Value(0x1000), None, None),
            ))
        );
    }

    #[test]
    fn source_indirect_operand() {
        assert_eq!(
            parse_source_indirect_operand(Span::new("[bx+si]")),
            Ok((
                Span::new(""),
                Operand::Indirect(AddressingMode::BxSi, None, None)
            ))
        );
        assert_eq!(
            parse_source_indirect_operand(Span::new("[ bp + di ]")),
            Ok((
                Span::new(""),
                Operand::Indirect(AddressingMode::BpDi, None, None)
            ))
        );
        assert_eq!(
            parse_source_indirect_operand(Span::new("byte [ bp + di ]")),
            Ok((
                Span::new(""),
                Operand::Indirect(AddressingMode::BpDi, Some(OperandSize::Byte), None)
            ))
        );
    }

    #[test]
    fn source_operand() {
        assert_eq!(
            parse_source_operand(Span::new("ax")),
            Ok((
                Span::new(""),
                Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word))
            ))
        );

        assert_eq!(
            parse_source_operand(Span::new("cs")),
            Ok((Span::new(""), Operand::Segment(Segment::CS)))
        );

        assert_eq!(
            parse_source_operand(Span::new("42")),
            Ok((Span::new(""), Operand::Immediate(ValueOrLabel::Value(42))))
        );

        assert_eq!(
            parse_source_operand(Span::new("[addr]")),
            Ok((
                Span::new(""),
                Operand::Direct(ValueOrLabel::Label("addr".to_string()), None, None)
            ))
        );

        assert_eq!(
            parse_source_operand(Span::new("[bx+si]")),
            Ok((
                Span::new(""),
                Operand::Indirect(AddressingMode::BxSi, None, None)
            ))
        );
    }

    #[test]
    fn source_operand_set() {
        assert_eq!(
            parse_source_operand_set(Span::new("ax, bx")),
            Ok((
                Span::new(""),
                OperandSet::DestinationAndSource(
                    Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                    Operand::Register(SizedRegister(Register::BlBx, OperandSize::Word)),
                )
            ))
        );

        assert_eq!(
            parse_source_operand_set(Span::new("ax")),
            Ok((
                Span::new(""),
                OperandSet::Destination(Operand::Register(SizedRegister(
                    Register::AlAx,
                    OperandSize::Word
                ))),
            ))
        );

        assert_eq!(
            parse_source_operand_set(Span::new("[si]")),
            Ok((
                Span::new(""),
                OperandSet::Destination(Operand::Indirect(AddressingMode::Si, None, None))
            ))
        );

        assert_eq!(
            parse_source_operand_set(Span::new("func")),
            Ok((
                Span::new(""),
                OperandSet::Destination(Operand::Immediate(ValueOrLabel::Label(
                    "func".to_string()
                )))
            ))
        );
    }

    #[test]
    fn source_instruction() {
        assert_eq!(
            parse_source_instruction(Span::new("mov ax, bx")),
            Ok((
                Span::new(""),
                Instruction::new(
                    Operation::MOV,
                    OperandSet::DestinationAndSource(
                        Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                        Operand::Register(SizedRegister(Register::BlBx, OperandSize::Word)),
                    )
                )
            ))
        );
    }
}
