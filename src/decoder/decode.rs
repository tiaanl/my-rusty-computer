pub use crate::decoder::{ByteReader, Modrm};
use crate::decoder::{Error, Result};
use crate::instructions::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};

trait ByteAndExtra<'a> {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]);
}

impl<'a> ByteAndExtra<'a> for &[u8] {
    fn byte_and_extra(&'a self) -> (u8, &'a [u8]) {
        let (code, extra) = self.split_at(1);
        (code[0], extra)
    }
}

/// Holds the result for a call to [decode_instruction].
pub struct DecodeResult {
    pub bytes_read: usize,
    pub instruction: Instruction,
}

/// Takes a byte slice and tries to convert it into an [Instruction].
pub fn decode_instruction(data: &[u8]) -> Result<DecodeResult> {
    let (op_code, extra_bytes) = data.byte_and_extra();
    // println!(
    //     "op_code, extra_bytes = {} ({:#04X}) {:?}",
    //     op_code, op_code, extra_bytes
    // );

    match op_code {
        // Arithmetic

        // ADD -> Add

        // Register/Memory with register to either
        // 0 0 0 0 0 0 d w | mod reg r/m
        0x00 | 0x01 | 0x02 | 0x03 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let (mod_rm_byte, mod_rm_extra_bytes) = extra_bytes.byte_and_extra();
            let (mod_rm, mod_rm_extra_bytes_read) =
                Modrm::try_from_mod_rm_byte(mod_rm_byte, mod_rm_extra_bytes)?;

            Ok(DecodeResult {
                bytes_read: 2 + mod_rm_extra_bytes_read,
                instruction: Instruction::new(
                    Operation::Add,
                    OperandSet::DestinationAndSource(
                        Operand(OperandType::Register(mod_rm.register), operand_size),
                        Operand(mod_rm.register_or_memory.into(), operand_size),
                    ),
                ),
            })
        }

        // Immediate to register/memory
        // 1 0 0 0 0 0 s w | mod 0 0 0 r/m | data | data if sw = 01
        0x80 | 0x81 | 0x82 | 0x83 => {
            let mut bytes_read: usize = 1; // op_code

            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            bytes_read += operand_size.in_bytes(); // we are going to read an immediate

            let (mod_rm_byte, extra_bytes) = extra_bytes.byte_and_extra();
            bytes_read += 1; // mod r/m byte

            let (mod_rm, extra_bytes_read) = Modrm::try_from_mod_rm_byte(mod_rm_byte, extra_bytes)?;
            bytes_read += extra_bytes_read; // bytes used by mod r/m

            let (_, extra_bytes) = extra_bytes.split_at(extra_bytes_read);

            let operation = match (mod_rm_byte >> 3) & 0b111 {
                0b000 => Operation::Add,
                0b001 => Operation::Adc,
                0b010 => Operation::And,
                0b011 => Operation::Xor,
                0b100 => Operation::Or,
                0b101 => Operation::Sbb,
                0b110 => Operation::Sub,
                0b111 => Operation::Cmp,
                _ => unreachable!(),
            };

            Ok(DecodeResult {
                bytes_read,
                instruction: Instruction::new(
                    operation,
                    OperandSet::DestinationAndSource(
                        Operand(mod_rm.register_or_memory.into(), operand_size),
                        Operand(
                            OperandType::Immediate(match operand_size {
                                OperandSize::Byte => extra_bytes.read_u8()?.into(),
                                OperandSize::Word => extra_bytes.read_u16()?,
                            }),
                            operand_size,
                        ),
                    ),
                ),
            })
        }

        // DEC = Decrement

        // Register
        // 0 1 0 0 1 reg
        0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(
                Operation::Dec,
                OperandSet::Destination(Operand(
                    OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                    OperandSize::Word,
                )),
            ),
        }),

        // Data transfer

        // MOV -> Move

        // Register/memory to/from register
        // 1 0 0 0 1 0 d w
        0x88 | 0x89 | 0x8A | 0x8B => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let direction = op_code >> 1 & 0b1;
            let (mod_rm_byte, mod_rm_extra_bytes) = extra_bytes.byte_and_extra();
            let (mod_rm, mod_rm_extra_bytes_read) =
                Modrm::try_from_mod_rm_byte(mod_rm_byte, mod_rm_extra_bytes)?;

            let destination = Operand(mod_rm.register.into(), operand_size);
            let source = Operand(mod_rm.register_or_memory.into(), operand_size);

            Ok(DecodeResult {
                bytes_read: 2 + mod_rm_extra_bytes_read,
                instruction: Instruction::new(
                    Operation::Mov,
                    match direction {
                        0 => OperandSet::DestinationAndSource(source, destination),
                        _ => OperandSet::DestinationAndSource(destination, source),
                    },
                ),
            })
        }

        // Immediate to register
        // 1 0 1 1 w reg
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB
        | 0xBC | 0xBD | 0xBE | 0xBF => {
            let operand_size = OperandSize::try_from_low_bits(op_code >> 3 & 0b1)?;
            let destination = Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                operand_size,
            );
            let source = Operand(
                OperandType::Immediate(match operand_size {
                    OperandSize::Byte => extra_bytes.read_u8()?.into(),
                    OperandSize::Word => extra_bytes.read_u16()?,
                }),
                operand_size,
            );

            Ok(DecodeResult {
                bytes_read: 1 + operand_size.in_bytes(),
                instruction: Instruction::new(
                    Operation::Mov,
                    OperandSet::DestinationAndSource(destination, source),
                ),
            })
        }

        // Register/memory to segment register
        // 1 0 0 0 1 1 1 0 | mod 0 segment r/m
        0x8E => {
            let (modrm_byte, extra_bytes) = extra_bytes.byte_and_extra();
            let (modrm, extra_bytes_read) = Modrm::try_from_mod_rm_byte(modrm_byte, extra_bytes)?;

            let destination: Operand = Operand(
                OperandType::Segment(Segment::try_from_low_bits(modrm_byte >> 3 & 0b111)?),
                OperandSize::Word,
            );

            let source = Operand(modrm.register_or_memory.into(), OperandSize::Word);

            Ok(DecodeResult {
                bytes_read: 2 + extra_bytes_read,
                instruction: Instruction::new(
                    Operation::Mov,
                    OperandSet::DestinationAndSource(destination, source),
                ),
            })
        }

        // Segment register to register/memory
        // 1 0 0 0 1 1 0 0 | mod 0 segment r/m
        0x8C => {
            let (mod_rm_byte, extra_bytes) = extra_bytes.byte_and_extra();
            let (mod_rm, extra_bytes_read) = Modrm::try_from_mod_rm_byte(mod_rm_byte, extra_bytes)?;

            let destination = Operand(mod_rm.register_or_memory.into(), OperandSize::Word);

            let source: Operand = Operand(
                OperandType::Segment(Segment::try_from_low_bits(mod_rm_byte >> 3 & 0b111)?),
                OperandSize::Word,
            );

            Ok(DecodeResult {
                bytes_read: 2 + extra_bytes_read,
                instruction: Instruction::new(
                    Operation::Mov,
                    OperandSet::DestinationAndSource(destination, source),
                ),
            })
        }

        // PUSH = Push

        // Register
        // 0 1 0 1 1 reg
        0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(
                Operation::Push,
                OperandSet::Destination(Operand(
                    OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                    OperandSize::Word,
                )),
            ),
        }),

        // Segment register
        // 0 0 0 reg 1 1 0
        0x06 | 0x0E | 0x16 | 0x1E => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(
                Operation::Push,
                OperandSet::Destination(Operand(
                    OperandType::Segment(Segment::try_from_low_bits(op_code >> 3 & 0b111)?),
                    OperandSize::Word,
                )),
            ),
        }),

        // POP = Pop

        // Register
        // 0 1 0 1 1 reg
        0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5E | 0x5F => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(
                Operation::Pop,
                OperandSet::Destination(Operand(
                    OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                    OperandSize::Word,
                )),
            ),
        }),

        // Segment register
        // 0 0 0 0 segment 1 1 1
        0x07 | 0x0F | 0x17 | 0x1F => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(
                Operation::Pop,
                OperandSet::Destination(Operand(
                    OperandType::Segment(Segment::try_from_low_bits(op_code >> 3 & 0b111)?),
                    OperandSize::Word,
                )),
            ),
        }),

        // IN - Input from

        // Fixed port
        // 1 1 1 0 0 1 0 w
        0xE4 | 0xE5 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let port = extra_bytes.read_u8()?;

            Ok(DecodeResult {
                bytes_read: 2,
                instruction: Instruction::new(
                    Operation::In,
                    OperandSet::DestinationAndSource(
                        Operand(OperandType::Register(Register::AlAx), operand_size),
                        Operand(OperandType::Immediate(port.into()), operand_size),
                    ),
                ),
            })
        }

        // Variable port
        // 1 1 1 0 1 1 0 w
        0xEC | 0xED => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            Ok(DecodeResult {
                bytes_read: 1,
                instruction: Instruction::new(
                    Operation::In,
                    OperandSet::DestinationAndSource(
                        Operand(OperandType::Register(Register::AlAx), operand_size),
                        Operand(OperandType::Register(Register::DlDx), OperandSize::Word),
                    ),
                ),
            })
        }

        // Logic

        // TEST = And function to flags, no result

        // Immediate data to accumulator
        // 1 0 1 0 1 0 0 w
        0xA8 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            Ok(DecodeResult {
                bytes_read: 1 + match operand_size {
                    OperandSize::Byte => 1,
                    OperandSize::Word => 2,
                },
                instruction: Instruction::new(
                    Operation::Test,
                    OperandSet::DestinationAndSource(
                        Operand(OperandType::Register(Register::AlAx), operand_size),
                        Operand(
                            OperandType::Immediate(match operand_size {
                                OperandSize::Byte => extra_bytes.read_u8()?.into(),
                                OperandSize::Word => extra_bytes.read_u16()?,
                            }),
                            operand_size,
                        ),
                    ),
                ),
            })
        }

        // Control transfer

        // CALL = Call

        // Direct within segment
        // 1 1 1 0 1 0 0 0 | displacement low | displacement high
        0xE8 => Ok(DecodeResult {
            bytes_read: 3,
            instruction: Instruction::new(
                Operation::Call,
                OperandSet::Offset(extra_bytes.read_u16()?),
            ),
        }),

        // JMP = Unconditional jump

        // Direct intersegment
        // 1 1 1 0 1 0 1 0
        0xEA => {
            let (offset, segment) = extra_bytes.split_at(2);
            let offset = offset.read_u16().unwrap();
            let segment = segment.read_u16().unwrap();

            Ok(DecodeResult {
                bytes_read: 5,
                instruction: Instruction::new(
                    Operation::Jmp,
                    OperandSet::SegmentAndOffset(segment, offset),
                ),
            })
        }

        // RET - Return from CALL

        // Within segment
        // 1 1 0 0 0 0 1 1
        0xC3 => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(Operation::Ret, OperandSet::None),
        }),

        // JE/JZ = Jump on equal/zero
        // 0 1 1 1 0 1 0 0 | disp
        0x74 => Ok(DecodeResult {
            bytes_read: 2,
            instruction: Instruction::new(
                Operation::Je,
                OperandSet::Offset(extra_bytes.read_u8()?.into()),
            ),
        }),

        // JNE/JNZ = Jump not equal/not zero
        // 0 1 1 1 0 1 0 1 | disp
        0x75 => Ok(DecodeResult {
            bytes_read: 2,
            instruction: Instruction::new(
                Operation::Jne,
                OperandSet::Offset(extra_bytes.read_u8()?.into()),
            ),
        }),

        // INT = Interrupt

        // Type specified
        // 1 1 0 0 1 1 0 1 | type
        0xCD => Ok(DecodeResult {
            bytes_read: 2,
            instruction: Instruction::new(
                Operation::Int,
                OperandSet::Destination(Operand(
                    OperandType::Immediate(extra_bytes.read_u8()?.into()),
                    OperandSize::Byte,
                )),
            ),
        }),

        // Processor control

        // CLI

        // Clear interrupt
        // 1 1 1 1 1 0 1 0
        0xFA => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(Operation::Cli, OperandSet::None),
        }),

        // STI

        // Set interrupt
        // 1 1 1 1 1 0 1 0
        0xFB => Ok(DecodeResult {
            bytes_read: 1,
            instruction: Instruction::new(Operation::Sti, OperandSet::None),
        }),

        _ => Err(Error::InvalidOpCode(op_code)),
    }
}
