mod parser;

use crate::parser::combinators::{parse_identifier, parse_number};
use crate::parser::ParseError;
use mrc_instruction::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::alpha1;
use nom::character::complete::alphanumeric1;
use nom::character::complete::multispace0;
use nom::character::complete::multispace1;
use nom::combinator::map;
use nom::combinator::map_res;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use std::{fmt::Formatter, str::FromStr};

#[derive(Debug, PartialEq)]
enum Token {
    Mnemonic(mrc_instruction::Operation),
    Number(i32),
}

fn parse_operation(input: &str) -> IResult<&str, Operation> {
    map_res(alpha1, |s| {
        if let Ok(operation) = Operation::from_str(s) {
            Ok(operation)
        } else {
            Err(ParseError::Unknown)
        }
    })(input)
}

fn parse_register_operand(input: &str) -> IResult<&str, Operand> {
    map_res(alphanumeric1, |s| {
        match String::from(s).to_lowercase().as_str() {
            "ax" => Ok(Operand(
                OperandType::Register(Register::AlAx),
                OperandSize::Word,
            )),
            "bx" => Ok(Operand(
                OperandType::Register(Register::BlBx),
                OperandSize::Word,
            )),
            "cx" => Ok(Operand(
                OperandType::Register(Register::ClCx),
                OperandSize::Word,
            )),
            "dx" => Ok(Operand(
                OperandType::Register(Register::DlDx),
                OperandSize::Word,
            )),
            "bp" => Ok(Operand(
                OperandType::Register(Register::ChBp),
                OperandSize::Word,
            )),
            "sp" => Ok(Operand(
                OperandType::Register(Register::AhSp),
                OperandSize::Word,
            )),
            "si" => Ok(Operand(
                OperandType::Register(Register::DhSi),
                OperandSize::Word,
            )),
            "di" => Ok(Operand(
                OperandType::Register(Register::BhDi),
                OperandSize::Word,
            )),
            "al" => Ok(Operand(
                OperandType::Register(Register::AlAx),
                OperandSize::Byte,
            )),
            "bl" => Ok(Operand(
                OperandType::Register(Register::BlBx),
                OperandSize::Byte,
            )),
            "cl" => Ok(Operand(
                OperandType::Register(Register::ClCx),
                OperandSize::Byte,
            )),
            "dl" => Ok(Operand(
                OperandType::Register(Register::DlDx),
                OperandSize::Byte,
            )),
            "ah" => Ok(Operand(
                OperandType::Register(Register::ChBp),
                OperandSize::Byte,
            )),
            "bh" => Ok(Operand(
                OperandType::Register(Register::AhSp),
                OperandSize::Byte,
            )),
            "ch" => Ok(Operand(
                OperandType::Register(Register::DhSi),
                OperandSize::Byte,
            )),
            "dh" => Ok(Operand(
                OperandType::Register(Register::BhDi),
                OperandSize::Byte,
            )),
            _ => Err(ParseError::Unknown),
        }
    })(input)
}

fn parse_segment_operand(input: &str) -> IResult<&str, Operand> {
    map_res(alphanumeric1, |s| {
        match String::from(s).to_lowercase().as_str() {
            "cs" => Ok(Operand(
                OperandType::Segment(Segment::Cs),
                OperandSize::Word,
            )),
            "es" => Ok(Operand(
                OperandType::Segment(Segment::Es),
                OperandSize::Word,
            )),
            "ss" => Ok(Operand(
                OperandType::Segment(Segment::Ss),
                OperandSize::Word,
            )),
            "ds" => Ok(Operand(
                OperandType::Segment(Segment::Ds),
                OperandSize::Word,
            )),
            _ => Err(ParseError::Unknown),
        }
    })(input)
}

fn parse_number_operand(input: &str) -> IResult<&str, Operand> {
    map(parse_number, |number| {
        Operand(OperandType::Immediate(number as u16), OperandSize::Word)
    })(input)
}

fn parse_operand(input: &str) -> IResult<&str, Operand> {
    // println!("-- input: \"{}\"", input);
    // println!("{:?}", parse_register_operand(input));
    // println!("{:?}", parse_segment_operand(input));
    // println!("{:?}", parse_number_operand(input));

    alt((
        parse_register_operand,
        parse_segment_operand,
        parse_number_operand,
    ))(input)
}

fn parse_destination_and_source_operand_set(input: &str) -> IResult<&str, OperandSet> {
    // println!("parse_destination_and_source_operand_set: \"{}\"", input);
    // println!(
    //     "{:?}",
    //     tuple((
    //         terminated(parse_operand, multispace0),
    //         terminated(tag(","), multispace0),
    //         parse_operand,
    //     ))(input)
    // );

    map(
        tuple((
            terminated(parse_operand, multispace0),
            terminated(tag(","), multispace0),
            parse_operand,
        )),
        |(destination, _, source)| OperandSet::DestinationAndSource(destination, source),
    )(input)
}

fn parse_destination_operand_set(input: &str) -> IResult<&str, OperandSet> {
    // println!("parse_destination_operand_set: \"{}\"", input);
    // println!(
    //     "{:?}",
    //     map(parse_operand, |destination| {
    //         OperandSet::Destination(destination)
    //     })(input)
    // );

    map(parse_operand, |destination| {
        OperandSet::Destination(destination)
    })(input)
}

#[derive(Debug)]
enum Directive {
    Bits(u16),
    Org(u32),
}

impl std::fmt::Display for Directive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Bits(bits) => write!(f, "BITS {}", bits),
            Directive::Org(origin) => write!(f, "ORG {}", origin),
        }
    }
}

#[derive(Debug)]
enum Line {
    Directive(Directive),
    Label(String),
    Instruction(Instruction),
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Directive(directive) => write!(f, "{}", directive),
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "{}", instruction),
        }
    }
}

fn parse_directive(input: &str) -> IResult<&str, Directive> {
    map_res(
        tuple((terminated(alphanumeric1, multispace1), parse_number)),
        |(s, v)| match String::from(s).to_lowercase().as_str() {
            "bits" => Ok(Directive::Bits(v as u16)),
            "org" => Ok(Directive::Org(v as u32)),
            _ => Err(()),
        },
    )(input)
}

fn parse_operand_set(input: &str) -> IResult<&str, OperandSet> {
    // println!("parse_operand_set: \"{}\"", input);
    // println!("{:?}", alt((
    //     parse_destination_and_source_operand_set,
    //     parse_destination_operand_set,
    // ))(input));

    alt((
        parse_destination_and_source_operand_set,
        parse_destination_operand_set,
    ))(input)
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    let res = map(
        tuple((
            terminated(parse_operation, multispace0),
            terminated(parse_operand_set, multispace0),
        )),
        |(operation, operand_set)| Instruction::new(operation, operand_set),
    )(input);

    // println!("parse_instruction: \"{}\"", input);
    // println!("{:?}", res);

    res
}

fn parse_label(input: &str) -> IResult<&str, &str> {
    terminated(terminated(parse_identifier, multispace0), tag(":"))(input)
}

fn parse_line(input: &str) -> IResult<&str, Line> {
    let res = terminated(
        alt((
            map(parse_label, |s| Line::Label(s.to_string())),
            map(parse_directive, Line::Directive),
            map(parse_instruction, Line::Instruction),
        )),
        multispace0,
    )(input);

    println!("parse_line: [{}]", input);
    println!("{:?}", res);

    res
}

fn parse_source(input: &str) -> IResult<&str, Vec<Line>> {
    delimited(multispace0, many1(parse_line), multispace0)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mnemonic() {
        assert_eq!(parse_operation("add").unwrap(), ("", Operation::ADD));
    }

    #[test]
    fn register() {
        assert_eq!(
            parse_register_operand("ax").unwrap(),
            (
                "",
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word,)
            )
        );
    }

    #[test]
    fn operand() {
        assert_eq!(
            parse_operand("ax, cs").unwrap(),
            (
                ", cs",
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word)
            )
        );
    }

    #[test]
    fn operand_set() {
        assert_eq!(
            parse_operand_set("ax, cs").unwrap(),
            (
                "",
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Segment(Segment::Cs), OperandSize::Word),
                )
            )
        );
    }

    #[test]
    fn instruction() {
        let source = "mov     ax, cs\r\n";

        assert_eq!(
            parse_instruction(source).unwrap(),
            (
                "",
                Instruction {
                    operation: Operation::MOV,
                    operands: OperandSet::DestinationAndSource(
                        Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                        Operand(OperandType::Segment(Segment::Cs), OperandSize::Word),
                    ),
                    repeat: None,
                    lock: false,
                    address: None
                }
            )
        );
    }

    #[test]
    fn full_test() {
        let source = String::from_utf8_lossy(include_bytes!("../../../samples/call.asm"));
        if let Ok((_, lines)) = parse_source(source.as_ref()) {
            lines.iter().for_each(|l| println!("{}", l))
        }
    }

    // #[test]
    // fn tt() {
    //     println!(
    //         "{:?}",
    //         many1(terminated(parse_instruction, multispace0))("sub ax , bx sub ax").unwrap()
    //     );
    // }
}
