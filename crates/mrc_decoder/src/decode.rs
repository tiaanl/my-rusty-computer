use crate::errors::Result;
use crate::{operations, ByteReader, Error, LowBitsDecoder};
use mrc_x86::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};

pub trait ByteAndExtra<'a> {
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
        0x00 | 0x01 | 0x02 | 0x03 => {
            operations::arithmetic::add::register_memory_with_register_to_either(
                op_code,
                extra_bytes,
            )
        }
        0x80 | 0x81 | 0x82 | 0x83 => {
            operations::arithmetic::add::immediate_to_register_memory(op_code, extra_bytes)
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
        0x88 | 0x89 | 0x8A | 0x8B => {
            operations::data_transfer::mov::register_memory_to_from_register(op_code, extra_bytes)
        }
        0x8C => operations::data_transfer::mov::segment_register_to_register_memory(
            op_code,
            extra_bytes,
        ),
        0x8E => operations::data_transfer::mov::register_memory_to_segment_register(
            op_code,
            extra_bytes,
        ),
        0xC6 | 0xC7 => {
            operations::data_transfer::mov::immediate_to_register_memory(op_code, extra_bytes)
        }
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB
        | 0xBC | 0xBD | 0xBE | 0xBF => {
            operations::data_transfer::mov::immediate_to_register(op_code, extra_bytes)
        }
        0xA0 | 0xA1 => operations::data_transfer::mov::memory_to_accumulator(op_code, extra_bytes),
        0xA2 | 0xA3 => operations::data_transfer::mov::accumulator_to_memory(op_code, extra_bytes),

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
