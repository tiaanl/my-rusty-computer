use crate::common::{
    immediate_to_accumulator, immediate_to_register_or_memory, register_or_memory_and_register,
    register_or_memory_and_segment, Direction,
};
use crate::errors::Result;
use crate::traits::{OpCodeExt, ReadExt};
use crate::{DecodeError, TryFromEncoding};
use mrc_instruction::{
    Address, Immediate, Instruction, Operand, OperandSet, OperandSize, Operation, RegisterEncoding,
    Repeat, Segment, SizedRegisterEncoding,
};

fn group1_operation(op_code: u8) -> Operation {
    match (op_code >> 3) & 0b111 {
        0b000 => Operation::ADD,
        0b001 => Operation::OR,
        0b010 => Operation::ADC,
        0b011 => Operation::SBB,
        0b100 => Operation::AND,
        0b101 => Operation::SUB,
        0b110 => Operation::XOR,
        0b111 => Operation::CMP,

        _ => unreachable!(),
    }
}

fn group2_operation(low_bits: u8) -> Operation {
    assert!(low_bits <= 0b111);

    match (low_bits) & 0b111 {
        0b000 => Operation::ROL,
        0b001 => Operation::ROR,
        0b010 => Operation::RCL,
        0b011 => Operation::RCR,
        0b100 => Operation::SHL,
        0b101 => Operation::SHR,
        0b110 => Operation::SHL,
        0b111 => Operation::SAR,

        _ => unreachable!(),
    }
}

fn group80_operation(bits: u8) -> Operation {
    debug_assert!(bits <= 0b111);

    match bits {
        0b000 => Operation::ADD,
        0b001 => Operation::OR,
        0b010 => Operation::ADC,
        0b011 => Operation::SBB,
        0b100 => Operation::AND,
        0b101 => Operation::SUB,
        0b110 => Operation::XOR,
        0b111 => Operation::CMP,

        _ => unreachable!(),
    }
}

/// Takes a byte slice and tries to convert it into an [Instruction].
pub fn decode_instruction(it: &mut impl Iterator<Item = u8>) -> Result<Instruction> {
    let op_code = it.read_u8().map_err(|e| match e {
        DecodeError::CouldNotReadExtraBytes => DecodeError::EndOfInput,
        err => err,
    })?;

    match op_code {
        0x00 | 0x01 | 0x02 | 0x03 | 0x08 | 0x09 | 0x0A | 0x0B | 0x10 | 0x11 | 0x12 | 0x13
        | 0x18 | 0x19 | 0x1A | 0x1B | 0x20 | 0x21 | 0x22 | 0x23 | 0x28 | 0x29 | 0x2A | 0x2B
        | 0x30 | 0x31 | 0x32 | 0x33 | 0x38 | 0x39 | 0x3A | 0x3B => register_or_memory_and_register(
            group1_operation(op_code),
            Direction::Detect,
            None,
            op_code,
            it,
        ),

        0x04 | 0x05 | 0x0C | 0x0D | 0x14 | 0x15 | 0x1C | 0x1D | 0x24 | 0x25 | 0x2C | 0x2D
        | 0x34 | 0x35 | 0x3C | 0x3D => {
            immediate_to_accumulator(group1_operation(op_code), op_code, it)
        }

        0x06 | 0x07 | 0x0E | 0x0F | 0x16 | 0x17 | 0x1E | 0x1F => Ok(Instruction::new(
            match op_code & 1 {
                0 => Operation::PUSH,
                _ => Operation::POP,
            },
            OperandSet::Destination(Operand::Segment(Segment::try_from_encoding(
                op_code >> 3 & 0b111,
            )?)),
        )),

        prefix @ (0x26 | 0x2E | 0x36 | 0x3E) => {
            fn override_segment(operand: &mut Operand, segment: Segment) {
                match operand {
                    Operand::Direct(ref mut seg, ..) | Operand::Indirect(ref mut seg, ..) => {
                        *seg = segment
                    }
                    _ => {}
                }
            }

            let segment = Segment::try_from_encoding((op_code >> 3) & 0b11)?;

            match decode_instruction(it) {
                Ok(mut instruction) => {
                    match instruction.operands {
                        OperandSet::Destination(ref mut destination) => {
                            override_segment(destination, segment);
                        }

                        OperandSet::DestinationAndSource(ref mut destination, ref mut source) => {
                            override_segment(destination, segment);
                            override_segment(source, segment);
                        }

                        _ => return Err(DecodeError::InvalidPrefix(prefix)),
                    }

                    Ok(instruction)
                }

                Err(_) => Err(DecodeError::InvalidPrefix(prefix)),
            }
        }

        0x27 => Ok(Instruction::new(Operation::DAA, OperandSet::None)),

        0x2F => Ok(Instruction::new(Operation::DAS, OperandSet::None)),

        0x37 => Ok(Instruction::new(Operation::AAA, OperandSet::None)),

        0x3F => Ok(Instruction::new(Operation::AAS, OperandSet::None)),

        0x40..=0x47 => Ok(Instruction::new(
            Operation::INC,
            OperandSet::Destination(Operand::Register(SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                OperandSize::Word,
            ))),
        )),

        0x48..=0x4F => Ok(Instruction::new(
            Operation::DEC,
            OperandSet::Destination(Operand::Register(SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                OperandSize::Word,
            ))),
        )),

        0x50..=0x57 => Ok(Instruction::new(
            Operation::PUSH,
            OperandSet::Destination(Operand::Register(SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                OperandSize::Word,
            ))),
        )),

        0x58..=0x5F => Ok(Instruction::new(
            Operation::POP,
            OperandSet::Destination(Operand::Register(SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                OperandSize::Word,
            ))),
        )),

        0x70 => Ok(Instruction::new(
            Operation::JO,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x71 => Ok(Instruction::new(
            Operation::JNO,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x72 => Ok(Instruction::new(
            Operation::JB,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x73 => Ok(Instruction::new(
            Operation::JNB,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x74 => Ok(Instruction::new(
            Operation::JE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x75 => Ok(Instruction::new(
            Operation::JNE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x76 => Ok(Instruction::new(
            Operation::JBE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x77 => Ok(Instruction::new(
            Operation::JNBE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x78 => Ok(Instruction::new(
            Operation::JS,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x79 => Ok(Instruction::new(
            Operation::JNS,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7A => Ok(Instruction::new(
            Operation::JP,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7B => Ok(Instruction::new(
            Operation::JNP,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7C => Ok(Instruction::new(
            Operation::JL,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7D => Ok(Instruction::new(
            Operation::JNL,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7E => Ok(Instruction::new(
            Operation::JLE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x7F => Ok(Instruction::new(
            Operation::JNLE,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0x80..=0x82 => immediate_to_register_or_memory(
            |mrrm_byte| group80_operation((mrrm_byte >> 3) & 0b111),
            op_code,
            it,
        ),

        0x83 => {
            let operand_size = op_code.operand_size();
            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            let destination = mrrm.register_or_memory.into_operand(operand_size);
            // Sign-extended value.
            let source = Immediate::Word(it.read_u8()? as u16 as u16).into();

            Ok(Instruction::new(
                group80_operation((mrrm_byte >> 3) & 0b111),
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0x84 | 0x85 => {
            register_or_memory_and_register(Operation::TEST, Direction::Detect, None, op_code, it)
        }

        0x86 | 0x87 => register_or_memory_and_register(
            Operation::XCHG,
            Direction::RegMemFirst,
            None,
            op_code,
            it,
        ),

        0x88..=0x8B => {
            register_or_memory_and_register(Operation::MOV, Direction::Detect, None, op_code, it)
        }

        0x8C | 0x8E => register_or_memory_and_segment(Operation::MOV, op_code, it),

        0x8D => {
            register_or_memory_and_register(Operation::LEA, Direction::RegFirst, None, op_code, it)
        }

        0x8F => {
            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            if (mrrm_byte >> 3) & 0b111 != 0 {
                return Err(DecodeError::InvalidModRegRMEncoding(mrrm_byte));
            }

            Ok(Instruction::new(
                Operation::POP,
                OperandSet::Destination(mrrm.register_or_memory.into_operand(OperandSize::Word)),
            ))
        }

        0x90 => Ok(Instruction::new(Operation::NOP, OperandSet::None)),

        0x91..=0x97 => {
            let operand_size = OperandSize::Word;

            let destination =
                Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size));
            let source = Operand::Register(SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                operand_size,
            ));
            Ok(Instruction::new(
                Operation::XCHG,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0x98 => Ok(Instruction::new(Operation::CBW, OperandSet::None)),

        0x99 => Ok(Instruction::new(Operation::CWD, OperandSet::None)),

        0x9A => {
            let offset = it.read_u16()?;
            let segment = it.read_u16()?;
            Ok(Instruction::new(
                Operation::CALL,
                OperandSet::Destination(Operand::SegmentAndOffset(Address::new(segment, offset))),
            ))
        }

        0x9B => Ok(Instruction::new(Operation::WAIT, OperandSet::None)),

        0x9C => Ok(Instruction::new(Operation::PUSHF, OperandSet::None)),

        0x9D => Ok(Instruction::new(Operation::POPF, OperandSet::None)),

        0x9E => Ok(Instruction::new(Operation::SAHF, OperandSet::None)),

        0x9F => Ok(Instruction::new(Operation::LAHF, OperandSet::None)),

        0xA0..=0xA3 => {
            let operand_size = op_code.operand_size();
            let direct_first = op_code >> 1 & 0b1 == 1;

            let address = it.read_u16()?;

            let reg =
                Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size));
            let direct = Operand::Direct(Segment::DS, address, operand_size);

            Ok(Instruction::new(
                Operation::MOV,
                if direct_first {
                    OperandSet::DestinationAndSource(direct, reg)
                } else {
                    OperandSet::DestinationAndSource(reg, direct)
                },
            ))
        }

        0xA4 => Ok(Instruction::new(Operation::MOVSB, OperandSet::None)),

        0xA5 => Ok(Instruction::new(Operation::MOVSW, OperandSet::None)),

        0xA6 => Ok(Instruction::new(Operation::CMPSB, OperandSet::None)),

        0xA7 => Ok(Instruction::new(Operation::CMPSW, OperandSet::None)),

        0xA8 | 0xA9 => immediate_to_accumulator(Operation::TEST, op_code, it),

        0xAA => Ok(Instruction::new(Operation::STOSB, OperandSet::None)),

        0xAB => Ok(Instruction::new(Operation::STOSW, OperandSet::None)),

        0xAC => Ok(Instruction::new(Operation::LODSB, OperandSet::None)),

        0xAD => Ok(Instruction::new(Operation::LODSW, OperandSet::None)),

        0xAE => Ok(Instruction::new(Operation::SCASB, OperandSet::None)),

        0xAF => Ok(Instruction::new(Operation::SCASW, OperandSet::None)),

        0xB0..=0xBF => {
            // Special case for reading the [OperandSize] from the 4th bit.
            let operand_size = (op_code >> 3).operand_size();

            let destination = SizedRegisterEncoding(
                RegisterEncoding::try_from_encoding(op_code & 0b111)?,
                operand_size,
            )
            .into();
            let source = it.read_immediate(operand_size)?.into();

            Ok(Instruction::new(
                Operation::MOV,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xC0 | 0xC1 => {
            let operand_size = op_code.operand_size();

            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            let reg_mem = mrrm.register_or_memory.into_operand(operand_size);
            let immediate = it.read_immediate(OperandSize::Byte)?.into();

            Ok(Instruction::new(
                group2_operation((mrrm_byte >> 3) & 0b111),
                OperandSet::DestinationAndSource(reg_mem, immediate),
            ))
        }

        0xC2 => Ok(Instruction::new(
            Operation::RET,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Word)?,
            )),
        )),

        0xC3 => Ok(Instruction::new(Operation::RET, OperandSet::None)),

        0xC4 => register_or_memory_and_register(
            Operation::LES,
            Direction::RegFirst,
            Some(OperandSize::Word),
            op_code,
            it,
        ),

        0xC5 => register_or_memory_and_register(
            Operation::LDS,
            Direction::RegFirst,
            Some(OperandSize::Word),
            op_code,
            it,
        ),

        0xC6 | 0xC7 => immediate_to_register_or_memory(|_| Operation::MOV, op_code, it),

        0xCA => Ok(Instruction::new(
            Operation::RET,
            OperandSet::Destination(it.read_immediate(OperandSize::Word)?.into()),
        )),

        0xCB => Ok(Instruction::new(Operation::RET, OperandSet::None)),

        0xCC => Ok(Instruction::new(Operation::INT3, OperandSet::None)),

        0xCD => Ok(Instruction::new(
            Operation::INT,
            OperandSet::Destination(it.read_immediate(OperandSize::Byte)?.into()),
        )),

        0xCE => Ok(Instruction::new(Operation::INTO, OperandSet::None)),

        0xCF => Ok(Instruction::new(Operation::IRET, OperandSet::None)),

        0xD0..=0xD3 => {
            let operand_size = op_code.operand_size();
            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            let destination = mrrm.register_or_memory.into_operand(operand_size);

            // if v = 0 then "count" = 1; if v = 1 then "count" in (CL)
            let source = if (op_code >> 1) & 0b1 == 1 {
                Operand::Register(SizedRegisterEncoding(RegisterEncoding::ClCx, operand_size))
            } else {
                Operand::Immediate(Immediate::Byte(1))
            };

            Ok(Instruction::new(
                group2_operation((mrrm_byte >> 3) & 0b111),
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xD4 => Ok(Instruction::new(
            Operation::AAM,
            OperandSet::Destination(it.read_immediate(OperandSize::Byte)?.into()),
        )),

        0xD5 => Ok(Instruction::new(
            Operation::AAD,
            OperandSet::Destination(it.read_immediate(OperandSize::Byte)?.into()),
        )),

        0xD6 => Ok(Instruction::new(Operation::SALC, OperandSet::None)),

        0xD7 => Ok(Instruction::new(Operation::XLAT, OperandSet::None)),

        0xD8..=0xDF => {
            // This operation is executed in conjunction with a FPU.  From the CPU's point of view,
            // it just consumes the byte.
            let _ = it.read_u8()?;
            Ok(Instruction::new(Operation::ESC, OperandSet::None))
        }

        0xE0 => Ok(Instruction::new(
            Operation::LOOPNZ,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0xE1 => Ok(Instruction::new(
            Operation::LOOPZ,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0xE2 => Ok(Instruction::new(
            Operation::LOOP,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0xE3 => Ok(Instruction::new(
            Operation::JCXZ,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0xE4 | 0xE5 => {
            let operand_size = op_code.operand_size();

            Ok(Instruction::new(
                Operation::IN,
                OperandSet::DestinationAndSource(
                    Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)),
                    it.read_immediate(OperandSize::Byte)?.into(),
                ),
            ))
        }

        0xE6 | 0xE7 => {
            let operand_size = op_code.operand_size();

            Ok(Instruction::new(
                Operation::OUT,
                OperandSet::DestinationAndSource(
                    it.read_immediate(OperandSize::Byte)?.into(),
                    Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)),
                ),
            ))
        }

        0xE8 => Ok(Instruction::new(
            Operation::CALL,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Word)?,
            )),
        )),

        0xE9 => Ok(Instruction::new(
            Operation::JMP,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Word)?,
            )),
        )),

        0xEA => {
            // These two reads should not be inlined, because they are read in a specific order.
            let offset = it.read_u16()?;
            let segment = it.read_u16()?;

            Ok(Instruction::new(
                Operation::JMP,
                OperandSet::Destination(Operand::SegmentAndOffset(Address::new(segment, offset))),
            ))
        }

        0xEB => Ok(Instruction::new(
            Operation::JMP,
            OperandSet::Destination(Operand::Displacement(
                it.read_displacement(OperandSize::Byte)?,
            )),
        )),

        0xEC | 0xED => {
            let operand_size = op_code.operand_size();

            Ok(Instruction::new(
                Operation::IN,
                OperandSet::DestinationAndSource(
                    Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)),
                    Operand::Register(SizedRegisterEncoding(
                        RegisterEncoding::DlDx,
                        OperandSize::Word,
                    )),
                ),
            ))
        }

        0xEE | 0xEF => {
            let operand_size = op_code.operand_size();

            Ok(Instruction::new(
                Operation::OUT,
                OperandSet::DestinationAndSource(
                    Operand::Register(SizedRegisterEncoding(
                        RegisterEncoding::DlDx,
                        OperandSize::Word,
                    )),
                    Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)),
                ),
            ))
        }
        0xF0 => Ok(Instruction::new(Operation::LOCK, OperandSet::None)),

        0xF1 => Ok(Instruction::new(Operation::INT1, OperandSet::None)),

        0xF2 | 0xF3 => match decode_instruction(it) {
            Ok(mut instruction) => {
                instruction.repeat = Some(match op_code & 1 {
                    0 => Repeat::NotEqual,
                    _ => Repeat::Equal,
                });
                Ok(instruction)
            }
            Err(err) => Err(err),
        },

        0xF4 => Ok(Instruction::new(Operation::HLT, OperandSet::None)),

        0xF5 => Ok(Instruction::new(Operation::CMC, OperandSet::None)),

        0xF6 | 0xF7 => {
            // TEST     1 1 1 1 0 1 1 w     mod 0 0 0 r/m   data data if w e 1
            //                                  0 0 1
            // NOT      1 1 1 1 0 1 1 w     mod 0 1 0 r/m
            // NEG      1 1 1 1 0 1 1 w     mod 0 1 1 r/m
            // MUL      1 1 1 1 0 1 1 w     mod 1 0 0 r/m
            // IMUL     1 1 1 1 0 1 1 w     mod 1 0 1 r/m
            // DIV      1 1 1 1 0 1 1 w     mod 1 1 0 r/m
            // IDIV     1 1 1 1 0 1 1 w     mod 1 1 1 r/m

            let operand_size = op_code.operand_size();
            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            let destination = mrrm.register_or_memory.into_operand(operand_size);

            let op = (mrrm_byte >> 3) & 0b111;
            let operation = match op {
                0b000 => Operation::TEST,
                // 0b001 => ,
                0b010 => Operation::NOT,
                0b011 => Operation::NEG,
                0b100 => Operation::MUL,
                0b101 => Operation::IMUL,
                0b110 => Operation::DIV,
                0b111 => Operation::IDIV,
                _ => return Err(DecodeError::InvalidOpCode(op)),
            };

            if operation == Operation::TEST {
                Ok(Instruction::new(
                    operation,
                    OperandSet::DestinationAndSource(
                        destination,
                        it.read_immediate(operand_size)?.into(),
                    ),
                ))
            } else {
                Ok(Instruction::new(
                    operation,
                    OperandSet::Destination(destination),
                ))
            }
        }

        0xF8 => Ok(Instruction::new(Operation::CLC, OperandSet::None)),

        0xF9 => Ok(Instruction::new(Operation::STC, OperandSet::None)),

        0xFA => Ok(Instruction::new(Operation::CLI, OperandSet::None)),

        0xFB => Ok(Instruction::new(Operation::STI, OperandSet::None)),

        0xFC => Ok(Instruction::new(Operation::CLD, OperandSet::None)),

        0xFD => Ok(Instruction::new(Operation::STD, OperandSet::None)),

        0xFE | 0xFF => {
            let operand_size = op_code.operand_size();
            let (mrrm, mrrm_byte) = it.read_mrrm()?;

            let destination = mrrm.register_or_memory.into_operand(operand_size);

            Ok(Instruction::new(
                match (mrrm_byte >> 3) & 0b111 {
                    0b000 => Operation::INC,
                    0b001 => Operation::DEC,
                    0b010 => Operation::CALL,
                    0b011 => Operation::CALL,
                    0b100 => Operation::JMP,
                    0b101 => Operation::JMP,
                    0b110 => Operation::PUSH,
                    0b111 => Operation::PUSH,
                    _ => unreachable!(),
                },
                OperandSet::Destination(destination),
            ))
        }

        _ => Err(DecodeError::InvalidOpCode(op_code)),
    }
}

#[cfg(test)]
#[path = "_tests/decode.rs"]
mod tests;
