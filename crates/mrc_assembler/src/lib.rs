mod parser;
pub(crate) mod source;

use crate::parser::{sources::parse_line, ParseResult, Span};
use mrc_instruction::Instruction;
use nom::{character::complete::multispace0, multi::many1, sequence::preceded};
use parser::{tokens::parse_identifier, instructions::parse_register, sources::Line};

fn parse_source(input: Span) -> ParseResult<Vec<Line>> {
    preceded(multispace0, many1(parse_line))(input)
}

pub fn parse(input: Span) -> Vec<Instruction> {
    parse_source(input).unwrap();
    vec![]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::sources::directive::Directive;
    use crate::source::{SourceInstruction, SourceOperand, SourceOperandSet, SourceValueOrLabel};
    use mrc_instruction::{OperandSize, Operation, Register, Segment, SizedRegister};

    // #[test]
    fn _source() {
        let source = r"
            BITS            16
            ORG             0x0100

            start:
                    mov     ax, cs
                    mov     ds, ax

                    call    add_one
                    call    add_two

            add_one:
                    inc     byte [count]            ; increase the count
                    ret

            add_two:
                    mov     al, [count]
                    add     al, 2
                    mov     [count], al
                    ret

            exit:
                    mov     ax, 0x4C00
                    int     0x21

        ";

        /*
            count:  db      0x01                    ; stores a sample count
        */

        assert_eq!(
            parse_source(Span::new(source)),
            Ok((
                Span::new(""),
                vec![
                    Line::Directive(Directive::Bits(16)),
                    Line::Directive(Directive::Org(0x0100)),
                    Line::Label("start".to_string()),
                    // mov     ax, cs
                    Line::Instruction(SourceInstruction::new(
                        Operation::MOV,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Word
                            )),
                            SourceOperand::Segment(Segment::CS),
                        )
                    )),
                    // mov     ds, ax
                    Line::Instruction(SourceInstruction::new(
                        Operation::MOV,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Segment(Segment::DS),
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Word
                            ))
                        )
                    )),
                    // call    add_one
                    Line::Instruction(SourceInstruction::new(
                        Operation::CALL,
                        SourceOperandSet::Destination(SourceOperand::Immediate(
                            SourceValueOrLabel::Label("add_one".to_string())
                        ))
                    )),
                    // call    add_two
                    Line::Instruction(SourceInstruction::new(
                        Operation::CALL,
                        SourceOperandSet::Destination(SourceOperand::Immediate(
                            SourceValueOrLabel::Label("add_two".to_string())
                        ))
                    )),
                    // add_one:
                    Line::Label("add_one".to_string()),
                    // inc     byte [count]
                    Line::Instruction(SourceInstruction::new(
                        Operation::INC,
                        SourceOperandSet::Destination(SourceOperand::Direct(
                            SourceValueOrLabel::Label("count".to_string()),
                            Some(OperandSize::Byte)
                        )),
                    )),
                    // ; increase the count
                    Line::Comment("; increase the count".to_string()),
                    // ret
                    Line::Instruction(SourceInstruction::new(
                        Operation::RET,
                        SourceOperandSet::None
                    )),
                    // add_two:
                    Line::Label("add_two".to_string()),
                    // mov     al, [count]
                    Line::Instruction(SourceInstruction::new(
                        Operation::MOV,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Byte
                            )),
                            SourceOperand::Direct(
                                SourceValueOrLabel::Label("count".to_string()),
                                None
                            ),
                        )
                    )),
                    // add     al, 2
                    Line::Instruction(SourceInstruction::new(
                        Operation::ADD,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Byte
                            )),
                            SourceOperand::Immediate(SourceValueOrLabel::Value(2)),
                        )
                    )),
                    // mov     [count], al
                    Line::Instruction(SourceInstruction::new(
                        Operation::MOV,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Direct(
                                SourceValueOrLabel::Label("count".to_string()),
                                None
                            ),
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Byte
                            ))
                        ),
                    )),
                    // ret
                    Line::Instruction(SourceInstruction::new(
                        Operation::RET,
                        SourceOperandSet::None,
                    )),
                    // exit:
                    Line::Label("exit".to_string()),
                    // mov     ax, 0x4C00
                    Line::Instruction(SourceInstruction::new(
                        Operation::MOV,
                        SourceOperandSet::DestinationAndSource(
                            SourceOperand::Register(SizedRegister(
                                Register::AlAx,
                                OperandSize::Word
                            )),
                            SourceOperand::Immediate(SourceValueOrLabel::Value(0x4C00)),
                        ),
                    )),
                    // int     0x21
                    Line::Instruction(SourceInstruction::new(
                        Operation::INT,
                        SourceOperandSet::Destination(SourceOperand::Immediate(
                            SourceValueOrLabel::Value(0x21)
                        ),),
                    )),
                ]
            ))
        );
    }
}
