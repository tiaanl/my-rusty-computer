mod ast;
mod parser;
pub(crate) mod tokenizer;

use crate::parser::{sources::parse_line, ParseResult, Span};
use mrc_instruction::Instruction;
use nom::{character::complete::multispace0, multi::many1, sequence::preceded};
use parser::{base::identifier, instructions::parse_register, sources::Line};

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
    use crate::ast::{Instruction, Operand, OperandSet, ValueOrLabel};
    use crate::parser::sources::directive::Directive;
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
                    Line::Instruction(Instruction::new(
                        Operation::MOV,
                        OperandSet::DestinationAndSource(
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                            Operand::Segment(Segment::CS),
                        )
                    )),
                    // mov     ds, ax
                    Line::Instruction(Instruction::new(
                        Operation::MOV,
                        OperandSet::DestinationAndSource(
                            Operand::Segment(Segment::DS),
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word))
                        )
                    )),
                    // call    add_one
                    Line::Instruction(Instruction::new(
                        Operation::CALL,
                        OperandSet::Destination(Operand::Immediate(ValueOrLabel::Label(
                            "add_one".to_string()
                        )))
                    )),
                    // call    add_two
                    Line::Instruction(Instruction::new(
                        Operation::CALL,
                        OperandSet::Destination(Operand::Immediate(ValueOrLabel::Label(
                            "add_two".to_string()
                        )))
                    )),
                    // add_one:
                    Line::Label("add_one".to_string()),
                    // inc     byte [count]
                    Line::Instruction(Instruction::new(
                        Operation::INC,
                        OperandSet::Destination(Operand::Direct(
                            ValueOrLabel::Label("count".to_string()),
                            Some(OperandSize::Byte),
                            None
                        )),
                    )),
                    // ; increase the count
                    Line::Comment("; increase the count".to_string()),
                    // ret
                    Line::Instruction(Instruction::new(Operation::RET, OperandSet::None)),
                    // add_two:
                    Line::Label("add_two".to_string()),
                    // mov     al, [count]
                    Line::Instruction(Instruction::new(
                        Operation::MOV,
                        OperandSet::DestinationAndSource(
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte)),
                            Operand::Direct(ValueOrLabel::Label("count".to_string()), None, None),
                        )
                    )),
                    // add     al, 2
                    Line::Instruction(Instruction::new(
                        Operation::ADD,
                        OperandSet::DestinationAndSource(
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte)),
                            Operand::Immediate(ValueOrLabel::Value(2)),
                        )
                    )),
                    // mov     [count], al
                    Line::Instruction(Instruction::new(
                        Operation::MOV,
                        OperandSet::DestinationAndSource(
                            Operand::Direct(ValueOrLabel::Label("count".to_string()), None, None),
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte))
                        ),
                    )),
                    // ret
                    Line::Instruction(Instruction::new(Operation::RET, OperandSet::None,)),
                    // exit:
                    Line::Label("exit".to_string()),
                    // mov     ax, 0x4C00
                    Line::Instruction(Instruction::new(
                        Operation::MOV,
                        OperandSet::DestinationAndSource(
                            Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                            Operand::Immediate(ValueOrLabel::Value(0x4C00)),
                        ),
                    )),
                    // int     0x21
                    Line::Instruction(Instruction::new(
                        Operation::INT,
                        OperandSet::Destination(Operand::Immediate(ValueOrLabel::Value(0x21)),),
                    )),
                ]
            ))
        );
    }
}
