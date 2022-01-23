#![allow(dead_code)]

mod parse {
    use crate::{
        lines,
        parser::{
            base::{identifier, number},
            ParseResult, Span,
        },
    };
    use mrc_instruction::{AddressingMode, OperandSize, Operation, Segment, SizedRegister};
    use nom::{
        branch::alt,
        bytes::{complete::tag_no_case, complete::take},
        character::complete::{char, multispace0, space0, space1},
        combinator::{cut, map, map_res, opt, recognize},
        error::ParseError,
        multi::many0,
        sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
        FindSubstring, Slice,
    };
    use std::str::FromStr;

    fn value_or_label(input: Span) -> ParseResult<lines::ValueOrLabel> {
        alt((
            map(
                delimited(char('\''), take(1usize), char('\'')),
                |res: Span| {
                    let c = res.chars().next().unwrap();
                    lines::ValueOrLabel::Value(c as i32)
                },
            ),
            map(number, lines::ValueOrLabel::Value),
            map(identifier, |res| {
                lines::ValueOrLabel::Label(*res.fragment())
            }),
        ))(input)
    }

    fn label(input: Span) -> ParseResult<&str> {
        map(
            terminated(terminated(identifier, space0), char(':')),
            |res| *res.fragment(),
        )(input)
    }

    fn register_operand(input: Span) -> ParseResult<lines::Operand> {
        map_res(identifier, |res| {
            match SizedRegister::from_str(res.fragment()) {
                Ok(sized_register) => Ok(lines::Operand::Register(sized_register)),
                Err(_) => Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    input,
                    nom::error::ErrorKind::Eof,
                ))),
            }
        })(input)
    }

    fn segment_operand(input: Span) -> ParseResult<lines::Operand> {
        let (input, segment) = identifier(input)?;

        match Segment::from_str(segment.fragment()) {
            Ok(segment) => Ok((input, lines::Operand::Segment(segment))),
            Err(_) => Err(nom::Err::Error(nom::error::Error::from_error_kind(
                input,
                nom::error::ErrorKind::Eof,
            ))),
        }
    }

    fn immediate_operand(input: Span) -> ParseResult<lines::Operand> {
        map(value_or_label, lines::Operand::Immediate)(input)
    }

    fn operand_size(input: Span) -> ParseResult<OperandSize> {
        map_res(identifier, |res| match *res.fragment() {
            "byte" => Ok(OperandSize::Byte),
            "word" => Ok(OperandSize::Word),
            _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(
                input,
                nom::error::ErrorKind::Eof,
            ))),
        })(input)
    }

    enum DirectOrIndirect<'s> {
        Direct(lines::ValueOrLabel<'s>),
        Indirect(AddressingMode),
    }

    fn direct_or_indirect(input: Span) -> ParseResult<DirectOrIndirect> {
        alt((
            map(
                map_res(
                    recognize(pair(
                        identifier,
                        opt(preceded(delimited(space0, char('+'), space0), identifier)),
                    )),
                    |res| AddressingMode::from_str(res.fragment()),
                ),
                DirectOrIndirect::Indirect,
            ),
            map(value_or_label, DirectOrIndirect::Direct),
        ))(input)
    }

    fn direct_or_indirect_operand(input: Span) -> ParseResult<lines::Operand> {
        map(
            tuple((
                opt(terminated(operand_size, space1)),
                opt(terminated(
                    terminated(
                        map_res(identifier, |res| Segment::from_str(res.fragment())),
                        char(':'),
                    ),
                    space0,
                )),
                delimited(
                    char('['),
                    delimited(space0, direct_or_indirect, space0),
                    char(']'),
                ),
            )),
            |(maybe_operand_size, segment_override, direct_or_indirect)| match direct_or_indirect {
                DirectOrIndirect::Direct(value_or_label) => {
                    lines::Operand::Direct(value_or_label, maybe_operand_size, segment_override)
                }
                DirectOrIndirect::Indirect(addressing_mode) => {
                    lines::Operand::Indirect(addressing_mode, maybe_operand_size, segment_override)
                }
            },
        )(input)
    }

    fn operand(input: Span) -> ParseResult<lines::Operand> {
        alt((
            direct_or_indirect_operand,
            register_operand,
            segment_operand,
            immediate_operand,
        ))(input)
    }

    fn operand_set(input: Span) -> ParseResult<lines::OperandSet> {
        map_res(
            tuple((
                opt(operand),
                opt(tuple((delimited(space0, char(','), space0), operand))),
            )),
            |res| match res {
                (Some(destination), Some((_, source))) => {
                    Ok(lines::OperandSet::DestinationAndSource(destination, source))
                }
                (Some(destination), None) => Ok(lines::OperandSet::Destination(destination)),
                (None, None) => Ok(lines::OperandSet::None),
                _ => Err(nom::Err::Error(nom::error::Error::from_error_kind(
                    input,
                    nom::error::ErrorKind::Eof,
                ))),
            },
        )(input)
    }

    fn instruction(input: Span) -> ParseResult<lines::Instruction> {
        let (input, operation) = map_res(identifier, |res| {
            Operation::from_str(res.fragment()).map_err(|_| {
                nom::Err::Error(nom::error::Error::from_error_kind(
                    input,
                    nom::error::ErrorKind::Eof,
                ))
            })
        })(input)?;

        // If we pass this point, we should return a failure is we don't get an operand set.

        let (input, operand_set) = cut(preceded(opt(space1), operand_set))(input)?;

        Ok((input, lines::Instruction::new(operation, operand_set)))
    }

    fn r#const(input: Span) -> ParseResult<lines::Const> {
        map(
            separated_pair(
                identifier,
                delimited(space1, tag_no_case("equ"), space1),
                number,
            ),
            |(name, value)| lines::Const {
                name: name.fragment(),
                value,
            },
        )(input)
    }

    fn comment<'s>(input: Span<'s>) -> ParseResult<&'s str> {
        let (temp, _) = char(';')(input)?;

        let mut split = match temp.find_substring("\r\n") {
            None => match temp.find_substring("\n") {
                None => temp.len(),
                Some(pos) => pos,
            },
            Some(pos) => pos,
        };

        // Increment the split to compensate for the char we consumed earlier.
        split += 1;

        Ok((input.slice(split..), input.slice(0..split).fragment()))
    }

    fn line(input: Span) -> ParseResult<lines::Line> {
        alt((
            map(terminated(comment, opt(multispace0)), |comment| {
                lines::Line::Comment(comment)
            }),
            map(terminated(r#const, opt(multispace0)), |c| {
                lines::Line::Const(c)
            }),
            map(terminated(label, opt(multispace0)), |label| {
                lines::Line::Label(label)
            }),
            map(terminated(instruction, opt(multispace0)), |instruction| {
                lines::Line::Instruction(instruction)
            }),
        ))(input)
    }

    pub fn program(input: Span) -> ParseResult<Vec<lines::Line>> {
        preceded(multispace0, many0(line))(input)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use mrc_instruction::{AddressingMode, OperandSize, Register};

        #[test]
        fn parse_value_or_label() {
            assert_eq!(
                value_or_label(Span::new("0xB800")).unwrap().1,
                lines::ValueOrLabel::Value(0xB800)
            );
            assert_eq!(
                value_or_label(Span::new("label")).unwrap().1,
                lines::ValueOrLabel::Label("label")
            );
        }

        #[test]
        fn parse_direct_or_indirect_operand() {
            assert_eq!(
                direct_or_indirect_operand(Span::new("[10]")).unwrap().1,
                lines::Operand::Direct(lines::ValueOrLabel::Value(10), None, None)
            );

            assert_eq!(
                direct_or_indirect_operand(Span::new("byte [10]"))
                    .unwrap()
                    .1,
                lines::Operand::Direct(
                    lines::ValueOrLabel::Value(10),
                    Some(OperandSize::Byte),
                    None
                )
            );

            assert_eq!(
                direct_or_indirect_operand(Span::new("byte cs:[10]"))
                    .unwrap()
                    .1,
                lines::Operand::Direct(
                    lines::ValueOrLabel::Value(10),
                    Some(OperandSize::Byte),
                    Some(Segment::CS)
                )
            );

            assert_eq!(
                direct_or_indirect_operand(Span::new("byte [test]"))
                    .unwrap()
                    .1,
                lines::Operand::Direct(
                    lines::ValueOrLabel::Label("test"),
                    Some(OperandSize::Byte),
                    None
                )
            );

            assert_eq!(
                direct_or_indirect_operand(Span::new("[si]")).unwrap().1,
                lines::Operand::Indirect(AddressingMode::Si, None, None)
            );
            assert_eq!(
                direct_or_indirect_operand(Span::new("byte [bx+si]"))
                    .unwrap()
                    .1,
                lines::Operand::Indirect(AddressingMode::BxSi, Some(OperandSize::Byte), None)
            );
        }

        #[test]
        fn parse_operand() {
            // Immediate
            assert_eq!(
                operand(Span::new("label")).unwrap().1,
                lines::Operand::Immediate(lines::ValueOrLabel::Label("label"))
            );

            // Register
            assert_eq!(
                operand(Span::new("ax")).unwrap().1,
                lines::Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word))
            );

            // Segment
            assert_eq!(
                operand(Span::new("cs")).unwrap().1,
                lines::Operand::Segment(Segment::CS)
            );

            // Direct
            assert_eq!(
                operand(Span::new("[0xB800]")).unwrap().1,
                lines::Operand::Direct(lines::ValueOrLabel::Value(0xB800), None, None)
            );

            // Direct
            assert_eq!(
                operand(Span::new("word [label]")).unwrap().1,
                lines::Operand::Direct(
                    lines::ValueOrLabel::Label("label"),
                    Some(OperandSize::Word),
                    None
                )
            );

            // Indirect
            assert_eq!(
                operand(Span::new("[bx+si]")).unwrap().1,
                lines::Operand::Indirect(AddressingMode::BxSi, None, None),
            );
        }

        #[test]
        fn parse_operand_set() {
            assert_eq!(
                operand_set(Span::new("ax, [bx+si]")).unwrap().1,
                lines::OperandSet::DestinationAndSource(
                    lines::Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                    lines::Operand::Indirect(AddressingMode::BxSi, None, None),
                )
            );

            assert_eq!(
                operand_set(Span::new("ax")).unwrap().1,
                lines::OperandSet::Destination(lines::Operand::Register(SizedRegister(
                    Register::AlAx,
                    OperandSize::Word
                )))
            );

            assert_eq!(
                operand_set(Span::new("")).unwrap().1,
                lines::OperandSet::None
            );
        }

        #[test]
        fn parse_instruction() {
            assert_eq!(
                instruction(Span::new("mov ax, bx")).unwrap().1,
                lines::Instruction::new(
                    Operation::MOV,
                    lines::OperandSet::DestinationAndSource(
                        lines::Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                        lines::Operand::Register(SizedRegister(Register::BlBx, OperandSize::Word))
                    )
                )
            );

            // TODO: This should be an error!
            // assert_eq!(
            //     instruction(Span::new("mov ax bx")),
            //     Err(nom::Err::Error(nom::error::Error::from_error_kind(
            //         Span::new(""),
            //         nom::error::ErrorKind::Eof
            //     )))
            // );
        }

        #[test]
        fn parse_label() {
            assert_eq!(label(Span::new("label:")).unwrap().1, "label".to_string());
        }

        #[test]
        fn parse_const() {
            assert_eq!(
                r#const(Span::new("address equ 0x0A")).unwrap().1,
                lines::Const {
                    name: "address",
                    value: 10,
                }
            );
        }

        #[test]
        fn parse_comment() {
            assert_eq!(comment(Span::new("; comment")).unwrap().1, "; comment");
            assert_eq!(
                comment(Span::new("; first comment\n  ; another comment  "))
                    .unwrap()
                    .1,
                "; first comment"
            );
        }

        #[test]
        fn parse_line() {
            assert_eq!(
                line(Span::new("label:\n")).unwrap().1,
                lines::Line::Label("label")
            );

            assert_eq!(
                line(Span::new("mov ax, bx:\n")).unwrap().1,
                lines::Line::Instruction(lines::Instruction::new(
                    Operation::MOV,
                    lines::OperandSet::DestinationAndSource(
                        lines::Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word)),
                        lines::Operand::Register(SizedRegister(Register::BlBx, OperandSize::Word)),
                    )
                ))
            );
        }

        #[test]
        fn parse_program() {
            let source = r"
                push    bx
                push    cx
                push    dx
                push    si
                push    di

            loop_strcmp_loop1:
                mov     al, byte cs:[si]
                inc     si
                cmp     al, bl
                jbe     loop_strcmp_great
                cmp     al, bl
                jl      loop_strcmp_less
                cmp     al, '$'
                je      loop_strcmp_quit
                cmp     bl, '$'
                je      loop_strcmp_quit
                jmp     loop_strcmp_loop1

            loop_strcmp_quit:
                mov     al, 1
                jmp     done

            loop_strcmp_great:
                mov     al, 2
                jmp     done

            loop_strcmp_less:
                mov     al, 0
                jmp     done

            done:
                pop     di
                pop     si
                pop     dx
                pop     cx
                pop     bx
                ret
            ";

            let p = program(Span::new(source));
            println!("{:#?}", p);
        }
    }
}

use crate::{
    lines,
    parser::{ParseResult, Span},
};

pub fn parse_lines(input: &str) -> ParseResult<Vec<lines::Line>> {
    parse::program(Span::new(input))
}
