#[allow(unused_imports)]
use crate::errors::Result;
use crate::{it_read_byte, it_read_word, operations, Error, LowBitsDecoder, Modrm};
use mrc_x86::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Repeat,
    Segment,
};

pub type DecodeFn<It> = fn(u8, &mut It) -> Result<Instruction>;
pub type DecodeFnMap<It> = [Option<DecodeFn<It>>; 256];

#[macro_export]
macro_rules! decode_fn_map_operations {
    () => {{
        [mrc_decoder::operations::immediate_to_register_memory]
    }};
}

#[macro_export]
macro_rules! decode_fn_map {
    ($iterator_type:tt,$name:ident) => {
        const $name: mrc_decoder::DecodeFnMap<$iterator_type> =
            mrc_decoder::decode_fn_map_operations!();
    };
}

/// Takes a byte slice and tries to convert it into an [Instruction].
pub fn decode_instruction<It: Iterator<Item = u8>>(it: &mut It) -> Result<Instruction> {
    let op_code = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };

    match op_code {
        // Multi
        0x80 | 0x81 | 0x82 | 0x83 => operations::immediate_to_register_memory(op_code, it),

        0x08 | 0x09 | 0x0A | 0x0B => {
            operations::register_memory_and_register_to_either(Operation::Or, op_code, it)
        }

        0x0C | 0x0D => operations::immediate_to_accumulator(Operation::Or, op_code, it),

        0xC0 | 0xC1 | 0xD0 | 0xD1 | 0xD2 | 0xD3 => {
            // 0xC0 - 0b11000000
            // 0xC1 - 0b11000001
            // 0xD0 - 0b11010000
            // 0xD1 - 0b11010001
            // 0xD2 - 0b11010010
            // 0xD3 - 0b11010011
            let is_immediate = (op_code >> 4) & 0b1 == 0;
            let use_cl = (op_code >> 1) & 0b1 != 0;
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            let modrm_byte = match it.next() {
                Some(byte) => byte,
                None => return Err(Error::CouldNotReadExtraBytes),
            };

            let operation = match (modrm_byte >> 3) & 0b111 {
                0b000 => Operation::Rol,
                0b001 => Operation::Ror,
                0b010 => Operation::Rcl,
                0b011 => Operation::Rcr,
                0b100 => Operation::Shl,
                0b101 => Operation::Shr,
                0b111 => Operation::Sar,
                _ => unreachable!(),
            };

            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            Ok(Instruction::new(
                operation,
                OperandSet::DestinationAndSource(
                    Operand(modrm.register_or_memory.into(), operand_size),
                    if is_immediate {
                        let value = match it.next() {
                            Some(byte) => byte,
                            None => return Err(Error::CouldNotReadExtraBytes),
                        };
                        Operand(OperandType::Immediate(value as u16), OperandSize::Byte)
                    } else if use_cl {
                        Operand(OperandType::Register(Register::ClCx), OperandSize::Byte)
                    } else {
                        Operand(OperandType::Immediate(1), OperandSize::Byte)
                    },
                ),
            ))
        }

        // Arithmetic

        // ADD -> Add
        0x00 | 0x01 | 0x02 | 0x03 => {
            operations::arithmetic::add::register_memory_with_register_to_either(op_code, it)
        }

        0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 => {
            operations::arithmetic::inc::register(op_code, it)
        }

        // DEC = Decrement

        // Register
        // 0 1 0 0 1 reg
        0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F => Ok(Instruction::new(
            Operation::Dec,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                OperandSize::Word,
            )),
        )),

        // Data transfer

        // MOV -> Move
        0x88 | 0x89 | 0x8A | 0x8B => {
            operations::data_transfer::mov::register_memory_to_from_register(op_code, it)
        }
        0x8C => operations::data_transfer::mov::segment_register_to_register_memory(op_code, it),
        0x8E => operations::data_transfer::mov::register_memory_to_segment_register(op_code, it),
        0xC6 | 0xC7 => operations::data_transfer::mov::immediate_to_register_memory(op_code, it),
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB
        | 0xBC | 0xBD | 0xBE | 0xBF => {
            operations::data_transfer::mov::immediate_to_register(op_code, it)
        }
        0xA0 | 0xA1 => operations::data_transfer::mov::memory_to_accumulator(op_code, it),
        0xA2 | 0xA3 => operations::data_transfer::mov::accumulator_to_memory(op_code, it),

        // PUSH = Push

        // Register
        // 0 1 0 1 1 reg
        0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 => Ok(Instruction::new(
            Operation::Push,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                OperandSize::Word,
            )),
        )),

        // Segment register
        // 0 0 0 reg 1 1 0
        0x06 | 0x0E | 0x16 | 0x1E => Ok(Instruction::new(
            Operation::Push,
            OperandSet::Destination(Operand(
                OperandType::Segment(Segment::try_from_low_bits(op_code >> 3 & 0b111)?),
                OperandSize::Word,
            )),
        )),

        // POP = Pop

        // Register
        // 0 1 0 1 1 reg
        0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5E | 0x5F => Ok(Instruction::new(
            Operation::Pop,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                OperandSize::Word,
            )),
        )),

        // Segment register
        // 0 0 0 0 segment 1 1 1
        0x07 | 0x0F | 0x17 | 0x1F => Ok(Instruction::new(
            Operation::Pop,
            OperandSet::Destination(Operand(
                OperandType::Segment(Segment::try_from_low_bits(op_code >> 3 & 0b111)?),
                OperandSize::Word,
            )),
        )),

        // IN - Input from

        // Fixed port
        // 1 1 1 0 0 1 0 w
        0xE4 | 0xE5 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let port = match it.next() {
                Some(port) => port,
                None => return Err(Error::CouldNotReadExtraBytes),
            };

            Ok(Instruction::new(
                Operation::In,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Immediate(port.into()), operand_size),
                ),
            ))
        }

        // Variable port
        // 1 1 1 0 1 1 0 w
        0xEC | 0xED => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            Ok(Instruction::new(
                Operation::In,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Register(Register::DlDx), OperandSize::Word),
                ),
            ))
        }

        // Logic

        // TEST = And function to flags, no result

        // Immediate data to accumulator
        // 1 0 1 0 1 0 0 w
        0xA8 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            Ok(Instruction::new(
                Operation::Test,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(
                        OperandType::Immediate(match operand_size {
                            OperandSize::Byte => it_read_byte(it).unwrap().into(),
                            OperandSize::Word => it_read_word(it).unwrap(),
                        }),
                        operand_size,
                    ),
                ),
            ))
        }

        // Control transfer

        // CALL = Call

        // Direct within segment
        // 1 1 1 0 1 0 0 0 | displacement low | displacement high
        0xE8 => {
            let offset = it_read_word(it).unwrap();
            Ok(Instruction::new(
                Operation::Call,
                OperandSet::Offset(offset),
            ))
        }

        // JMP = Unconditional jump

        // Direct intersegment
        // 1 1 1 0 1 0 1 0
        0xEA => {
            let offset = it_read_word(it).unwrap();
            let segment = it_read_word(it).unwrap();

            Ok(Instruction::new(
                Operation::Jmp,
                OperandSet::SegmentAndOffset(segment, offset),
            ))
        }

        // RET - Return from CALL

        // Within segment
        // 1 1 0 0 0 0 1 1
        0xC3 => Ok(Instruction::new(Operation::Ret, OperandSet::None)),

        // JE/JZ = Jump on equal/zero
        // 0 1 1 1 0 1 0 0 | disp
        0x74 => Ok(Instruction::new(
            Operation::Je,
            OperandSet::Offset(it_read_byte(it).unwrap().into()),
        )),

        // JNE/JNZ = Jump not equal/not zero
        // 0 1 1 1 0 1 0 1 | disp
        0x75 => Ok(Instruction::new(
            Operation::Jne,
            OperandSet::Offset(it_read_byte(it).unwrap().into()),
        )),

        // INT = Interrupt

        // Type specified
        // 1 1 0 0 1 1 0 1 | type
        0xCD => Ok(Instruction::new(
            Operation::Int,
            OperandSet::Destination(Operand(
                OperandType::Immediate(it_read_byte(it).unwrap().into()),
                OperandSize::Byte,
            )),
        )),

        // Processor control
        0xD8 | 0xD9 | 0xDA | 0xDB | 0xDC | 0xDD | 0xDE | 0xDF => {
            operations::processor_control::escape_to_external_device(op_code, it)
        }
        0x9B => operations::processor_control::wait(op_code, it),
        0xF0 => operations::processor_control::bus_lock_prefix(op_code, it),
        0xF4 => operations::processor_control::halt(op_code, it),
        0xF5 => operations::processor_control::complimentary_carry(op_code, it),
        0xF8 => operations::processor_control::clear_carry(op_code, it),
        0xF9 => operations::processor_control::set_carry(op_code, it),
        0xFA => operations::processor_control::clear_interrupt(op_code, it),
        0xFB => operations::processor_control::set_interrupt(op_code, it),
        0xFC => operations::processor_control::clear_direction(op_code, it),
        0xFD => operations::processor_control::set_direction(op_code, it),

        0xFE => operations::arithmetic::inc::register_memory(op_code, it),

        // String manipulation
        0xA4 | 0xA5 => operations::string_manipulation::movs::move_byte_word(op_code, it),

        // Overrides
        0xF2 => {
            let mut instruction = decode_instruction(it)?;
            instruction.repeat = Some(Repeat::NotEqual);
            Ok(instruction)
        }

        0xF3 => {
            let mut instruction = decode_instruction(it)?;
            instruction.repeat = Some(Repeat::Equal);
            Ok(instruction)
        }

        _ => Err(Error::InvalidOpCode(op_code)),
    }
}

mod test {
    #![allow(unused_imports)]
    #![allow(unused_macros)]

    use crate::modrm::RegisterOrMemory;
    use crate::Modrm;
    use mrc_x86::{
        Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register,
    };

    struct TestIterator {
        data: Vec<u8>,
        position: usize,
    }

    impl TestIterator {
        #[allow(dead_code)]
        fn from_bytes(bytes: &[u8]) -> Self {
            Self {
                data: Vec::from(bytes),
                position: 0,
            }
        }
    }

    impl Iterator for TestIterator {
        type Item = u8;

        fn next(&mut self) -> Option<Self::Item> {
            if self.position >= self.data.len() {
                None
            } else {
                let byte = self.data[self.position];
                self.position += 1;
                Some(byte)
            }
        }
    }

    macro_rules! test_decoder {
        ($bytes:expr, $expected:expr) => {
            let mut it = TestIterator::from_bytes($bytes);
            let actual = crate::decode::decode_instruction(&mut it);
            assert_eq!(Ok($expected), actual);
        };
    }

    #[test]
    fn test_00() {
        let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

        test_decoder!(
            &[0x00, modrm.as_byte()],
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_01() {
        let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

        test_decoder!(
            &[0x01, modrm.as_byte()],
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Word),
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                )
            )
        );
    }

    #[test]
    fn test_02() {
        let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

        test_decoder!(
            &[0x02, modrm.as_byte()],
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_03() {
        let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

        test_decoder!(
            &[0x03, modrm.as_byte()],
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Word),
                )
            )
        );
    }

    #[test]
    fn test_c0() {
        test_decoder!(
            &[0xC0, 0xE0, 0x04], // shl al, 4
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                    Operand(OperandType::Immediate(4), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_c1() {
        test_decoder!(
            &[0xC1, 0xE0, 0x04], // shl ax, 4
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Immediate(4), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_d0() {
        test_decoder!(
            &[0xD0, 0xE0], // shl al, 1
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                    Operand(OperandType::Immediate(1), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_d1() {
        test_decoder!(
            &[0xD1, 0xE0], // shl ax, 1
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Immediate(1), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_d2() {
        test_decoder!(
            &[0xD2, 0xE0], // shl al, cl
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                    Operand(OperandType::Register(Register::ClCx), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_d3() {
        test_decoder!(
            &[0xD3, 0xE0], // shl ax, cl
            Instruction::new(
                Operation::Shl,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Register(Register::ClCx), OperandSize::Byte),
                )
            )
        );
    }
}
