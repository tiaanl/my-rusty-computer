#[allow(unused_imports)]
use crate::errors::Result;
use crate::{it_read_byte, it_read_word, operations, Error, LowBitsDecoder, Modrm};
use mrc_x86::{
    Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register,
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

fn group1_operation(op_code: u8) -> Operation {
    match (op_code >> 3) & 0b111 {
        0b000 => Operation::Add,
        0b001 => Operation::Or,
        0b010 => Operation::Adc,
        0b011 => Operation::Sbb,
        0b100 => Operation::And,
        0b101 => Operation::Sub,
        0b110 => Operation::Xor,
        0b111 => Operation::Cmp,

        _ => unreachable!(),
    }
}

fn group2_operation(low_bits: u8) -> Operation {
    assert!(low_bits <= 0b111);

    match (low_bits) & 0b111 {
        0b000 => Operation::Rol,
        0b001 => Operation::Ror,
        0b010 => Operation::Rcl,
        0b011 => Operation::Rcr,
        0b100 => Operation::Shl,
        0b101 => Operation::Shr,
        // 0b110 => Operation::Nop,  // No encoding for this one.
        0b111 => Operation::Sar,
        _ => unreachable!(),
    }
}

fn jump_operation_from_low_bits(bits: u8) -> Operation {
    assert!(bits <= 0xF);

    match bits {
        0x0 => Operation::Jo,
        0x1 => Operation::Jno,
        0x2 => Operation::Jb,
        0x3 => Operation::Jnb,
        0x4 => Operation::Je,
        0x5 => Operation::Jne,
        0x6 => Operation::Jbe,
        0x7 => Operation::Jnbe,
        0x8 => Operation::Js,
        0x9 => Operation::Jns,
        0xA => Operation::Jp,
        0xB => Operation::Jnp,
        0xC => Operation::Jl,
        0xD => Operation::Jnl,
        0xE => Operation::Jle,
        0xF => Operation::Jnle,
        _ => unreachable!(),
    }
}

fn immediate_operand_from_it<It: Iterator<Item = u8>>(
    it: &mut It,
    operand_size: OperandSize,
) -> Result<Operand> {
    match operand_size {
        OperandSize::Byte => {
            let immediate = it_read_byte(it)?;
            Ok(Operand(
                OperandType::Immediate(immediate as u16),
                operand_size,
            ))
        }
        OperandSize::Word => {
            let immediate = it_read_word(it)?;
            Ok(Operand(OperandType::Immediate(immediate), operand_size))
        }
    }
}

fn displacement_byte_from_it<It: Iterator<Item = u8>>(it: &mut It) -> Result<OperandSet> {
    let displacement = it_read_byte(it)? as i8;
    Ok(OperandSet::Displacement(Displacement::Byte(displacement)))
}

fn displacement_word_from_it<It: Iterator<Item = u8>>(it: &mut It) -> Result<OperandSet> {
    let displacement = it_read_word(it)? as i16;
    Ok(OperandSet::Displacement(Displacement::Word(displacement)))
}

/// Takes a byte slice and tries to convert it into an [Instruction].
pub fn decode_instruction<It: Iterator<Item = u8>>(it: &mut It) -> Result<Instruction> {
    let op_code = it_read_byte(it)?;

    let maybe_instruction = match op_code {
        0x04 | 0x05 | 0x0C | 0x0D | 0x14 | 0x15 | 0x1C | 0x1D | 0x24 | 0x25 | 0x2C | 0x2D
        | 0x34 | 0x35 | 0x3C | 0x3D => {
            operations::immediate_to_accumulator(group1_operation(op_code), op_code, it)
        }

        0x00 | 0x01 | 0x02 | 0x03 | 0x08 | 0x09 | 0x0A | 0x0B | 0x10 | 0x11 | 0x12 | 0x13
        | 0x18 | 0x19 | 0x1A | 0x1B | 0x20 | 0x21 | 0x22 | 0x23 | 0x28 | 0x29 | 0x2A | 0x2B
        | 0x30 | 0x31 | 0x32 | 0x33 | 0x38 | 0x39 | 0x3A | 0x3B => {
            operations::register_memory_and_register_to_either(
                group1_operation(op_code),
                op_code,
                it,
            )
        }

        0x06 | 0x07 | 0x0E | 0x0F | 0x016 | 0x17 | 0x1E | 0x1F => {
            operations::push_pop_segment(op_code, it)
        }

        0x26 | 0x2E | 0x36 | 0x3E => match decode_instruction(it) {
            Ok(mut instruction) => {
                instruction.segment_override =
                    Some(Segment::try_from_low_bits((op_code >> 3) & 0b111)?);
                Ok(instruction)
            }
            Err(err) => Err(err),
        },

        0x27 | 0x2F | 0x37 | 0x3F => Ok(Instruction::new(
            match (op_code >> 3) & 0b111 {
                0b000 => Operation::Daa,
                0b001 => Operation::Das,
                0b010 => Operation::Aaa,
                0b011 => Operation::Aas,
                _ => return Err(Error::InvalidOpCode(op_code)),
            },
            OperandSet::None,
        )),

        0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4A | 0x4B
        | 0x4C | 0x4D | 0x4E | 0x4F => {
            let register = Register::try_from_low_bits(op_code & 0b111)?;

            Ok(Instruction::new(
                if (op_code >> 3) & 0b1 == 0 {
                    Operation::Inc
                } else {
                    Operation::Dec
                },
                OperandSet::Destination(Operand(
                    OperandType::Register(register),
                    OperandSize::Word,
                )),
            ))
        }

        0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 | 0x58 | 0x59 | 0x5A | 0x5B
        | 0x5C | 0x5D | 0x5E | 0x5F => Ok(Instruction::new(
            Operation::Push,
            OperandSet::Destination(Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                OperandSize::Word,
            )),
        )),

        0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77 | 0x78 | 0x79 | 0x7A | 0x7B
        | 0x7C | 0x7D | 0x7E | 0x7F => Ok(Instruction::new(
            jump_operation_from_low_bits(op_code & 0b111),
            displacement_byte_from_it(it)?,
        )),

        0x80 | 0x81 | 0x82 | 0x83 => operations::immediate_to_register_memory(op_code, it),

        0x84 | 0x85 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm = Modrm::try_from_iter(it)?;

            Ok(Instruction::new(
                Operation::Test,
                OperandSet::DestinationAndSource(
                    Operand(modrm.register_or_memory.into(), operand_size),
                    Operand(OperandType::Register(modrm.register), operand_size),
                ),
            ))
        }

        0x86 | 0x87 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm = Modrm::try_from_iter(it)?;

            Ok(Instruction::new(
                Operation::Xchg,
                OperandSet::DestinationAndSource(
                    Operand(modrm.register_or_memory.into(), operand_size),
                    Operand(OperandType::Register(modrm.register), operand_size),
                ),
            ))
        }

        0x88 | 0x89 | 0x8A | 0x8B => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let direction = op_code >> 1 & 0b1;
            let modrm = Modrm::try_from_iter(it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);
            let source = Operand(OperandType::Register(modrm.register), operand_size);

            Ok(Instruction::new(
                Operation::Mov,
                match direction {
                    0 => OperandSet::DestinationAndSource(destination, source),
                    _ => OperandSet::DestinationAndSource(source, destination),
                },
            ))
        }

        0x8C | 0x8E => {
            let operand_size = OperandSize::Word;
            let direction = op_code >> 1 & 0b1;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);
            let source = Operand(
                OperandType::Segment(Segment::try_from_low_bits((modrm_byte >> 3) & 0b111)?),
                operand_size,
            );

            Ok(Instruction::new(
                Operation::Mov,
                match direction {
                    0 => OperandSet::DestinationAndSource(destination, source),
                    _ => OperandSet::DestinationAndSource(source, destination),
                },
            ))
        }

        0x8D => {
            let operand_size = OperandSize::Word;
            let modrm = Modrm::try_from_iter(it)?;

            Ok(Instruction::new(
                Operation::Lea,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(modrm.register), operand_size),
                    Operand(modrm.register_or_memory.into(), operand_size),
                ),
            ))
        }

        0x8F => {
            let operand_size = OperandSize::Word;
            let modrm = Modrm::try_from_iter(it)?;

            Ok(Instruction::new(
                Operation::Pop,
                OperandSet::Destination(Operand(
                    OperandType::Register(modrm.register),
                    operand_size,
                )),
            ))
        }

        0x90 => Ok(Instruction::new(Operation::Nop, OperandSet::None)),

        0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97 => {
            let operand_size = OperandSize::Word;
            let destination = Operand(OperandType::Register(Register::AlAx), operand_size);
            let source = Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                operand_size,
            );
            Ok(Instruction::new(
                Operation::Xchg,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0x98 => Ok(Instruction::new(Operation::Cbw, OperandSet::None)),

        0x99 => Ok(Instruction::new(Operation::Cwd, OperandSet::None)),

        0x9B => Ok(Instruction::new(Operation::Wait, OperandSet::None)),

        0x9C => Ok(Instruction::new(Operation::Pushf, OperandSet::None)),

        0x9D => Ok(Instruction::new(Operation::Popf, OperandSet::None)),

        0x9E => Ok(Instruction::new(Operation::Sahf, OperandSet::None)),

        0x9F => Ok(Instruction::new(Operation::Lahf, OperandSet::None)),

        0xA0 | 0xA1 | 0xA2 | 0xA3 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            let address = it_read_word(it).unwrap();

            Ok(Instruction::new(
                Operation::Mov,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Direct(address), operand_size),
                ),
            ))
        }

        0xA4 => Ok(Instruction::new(Operation::Movsb, OperandSet::None)),

        0xA5 => Ok(Instruction::new(Operation::Movsw, OperandSet::None)),

        0xA6 => Ok(Instruction::new(Operation::Cmpsb, OperandSet::None)),

        0xA7 => Ok(Instruction::new(Operation::Cmpsw, OperandSet::None)),

        0xA8 | 0xA9 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            let destination = Operand(OperandType::Register(Register::AlAx), operand_size);
            let source = immediate_operand_from_it(it, operand_size)?;

            Ok(Instruction::new(
                Operation::Test,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xAA | 0xAB => {
            let operand_size = OperandSize::try_from_low_bits(op_code >> 3 & 0b1)?;
            Ok(Instruction::new(
                Operation::Stos,
                OperandSet::Destination(Operand(
                    OperandType::Register(Register::AlAx),
                    operand_size,
                )),
            ))
        }

        // 0xAC | 0xAD | 0xAE | 0xAF => {
        //     todo!()
        // }
        0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7 | 0xB8 | 0xB9 | 0xBA | 0xBB
        | 0xBC | 0xBD | 0xBE | 0xBF => {
            let operand_size = OperandSize::try_from_low_bits(op_code >> 3 & 0b1)?;

            let destination = Operand(
                OperandType::Register(Register::try_from_low_bits(op_code & 0b111)?),
                operand_size,
            );
            let source = match operand_size {
                OperandSize::Byte => {
                    let immediate = it_read_byte(it)?;
                    Operand(OperandType::Immediate(immediate as u16), operand_size)
                }
                OperandSize::Word => {
                    let immediate = it_read_word(it)?;
                    Operand(OperandType::Immediate(immediate), operand_size)
                }
            };

            Ok(Instruction::new(
                Operation::Mov,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xC0 | 0xC1 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);
            let source = immediate_operand_from_it(it, OperandSize::Byte)?;

            Ok(Instruction::new(
                group2_operation((modrm_byte >> 3) & 0b111),
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xC2 => Ok(Instruction::new(
            Operation::Ret,
            OperandSet::Destination(immediate_operand_from_it(it, OperandSize::Word)?),
        )),

        0xC3 => Ok(Instruction::new(Operation::Ret, OperandSet::None)),

        0xC4 | 0xC5 => {
            let operand_size = OperandSize::Word;
            let modrm = Modrm::try_from_iter(it)?;

            let destination = Operand(OperandType::Register(modrm.register), operand_size);
            let source = Operand(modrm.register_or_memory.into(), operand_size);

            Ok(Instruction::new(
                match op_code & 0b1 {
                    0 => Operation::Lds,
                    _ => Operation::Les,
                },
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xC6 | 0xC7 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);
            let source = immediate_operand_from_it(it, operand_size)?;

            Ok(Instruction::new(
                Operation::Mov,
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xCA | 0xCB => {
            let with_immediate = op_code & 1 == 1;
            Ok(Instruction::new(
                Operation::Retf,
                if with_immediate {
                    let immediate = immediate_operand_from_it(it, OperandSize::Word)?;
                    OperandSet::Destination(immediate)
                } else {
                    OperandSet::None
                },
            ))
        }

        0xCC => Ok(Instruction::new(
            Operation::Int,
            OperandSet::Destination(Operand(OperandType::Immediate(3), OperandSize::Byte)),
        )),

        0xCD => {
            let code = it_read_byte(it)?;
            Ok(Instruction::new(
                Operation::Int,
                OperandSet::Destination(Operand(
                    OperandType::Immediate(code as u16),
                    OperandSize::Byte,
                )),
            ))
        }

        0xCF => Ok(Instruction::new(Operation::IRet, OperandSet::None)),

        0xD0 | 0xD1 | 0xD2 | 0xD3 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);

            // if v = 0 then "count" = 1; if v = 1 then "count" in (CL)
            let source = if (op_code >> 1) & 0b1 == 1 {
                Operand(OperandType::Register(Register::ClCx), OperandSize::Byte)
            } else {
                Operand(OperandType::Immediate(1), OperandSize::Byte)
            };

            Ok(Instruction::new(
                group2_operation((modrm_byte >> 3) & 0b111),
                OperandSet::DestinationAndSource(destination, source),
            ))
        }

        0xD4 | 0xD5 => {
            // For some reason there is a hard coded byte after each of these??!
            // 0x0A
            it.next().unwrap();

            Ok(Instruction::new(
                if op_code & 0b1 == 0 {
                    Operation::Aam
                } else {
                    Operation::Aad
                },
                OperandSet::None,
            ))
        }

        0xD6 => Ok(Instruction::new(Operation::Sahf, OperandSet::None)),

        0xD7 => Ok(Instruction::new(Operation::Xlat, OperandSet::None)),

        0xE0 | 0xE1 | 0xE2 | 0xE3 => Ok(Instruction::new(
            match op_code {
                0xE0 => Operation::Loopnz,
                0xE1 => Operation::Loopz,
                0xE2 => Operation::Loop,
                0xE3 => Operation::Jcxz,
                _ => unreachable!(),
            },
            displacement_byte_from_it(it)?,
        )),

        0xE4 | 0xE5 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            let port = it_read_byte(it)?;

            Ok(Instruction::new(
                Operation::In,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Immediate(port as u16), OperandSize::Byte),
                ),
            ))
        }

        0xE6 | 0xE7 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            let port = it_read_byte(it)?;

            Ok(Instruction::new(
                Operation::Out,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Immediate(port as u16), OperandSize::Byte),
                ),
            ))
        }

        0xE8 => Ok(Instruction::new(
            Operation::Call,
            displacement_word_from_it(it)?,
        )),

        0xE9 => Ok(Instruction::new(
            Operation::Jmp,
            displacement_word_from_it(it)?,
        )),

        0xEA => {
            let offset = it_read_word(it)?;
            let segment = it_read_word(it)?;

            Ok(Instruction::new(
                Operation::Jmp,
                OperandSet::SegmentAndOffset(segment, offset),
            ))
        }

        0xEB => Ok(Instruction::new(
            Operation::Jmp,
            displacement_byte_from_it(it)?,
        )),

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

        0xEE | 0xEF => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

            Ok(Instruction::new(
                Operation::Out,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), operand_size),
                    Operand(OperandType::Register(Register::DlDx), OperandSize::Word),
                ),
            ))
        }

        0xF0 => {
            let mut instruction = decode_instruction(it)?;
            instruction.lock = true;
            Ok(instruction)
        }

        0xF4 => Ok(Instruction::new(Operation::Hlt, OperandSet::None)),

        0xF5 => Ok(Instruction::new(Operation::Cmc, OperandSet::None)),

        0xF6 | 0xF7 => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);

            Ok(Instruction::new(
                match (modrm_byte >> 3) & 0b111 {
                    0b000 => Operation::Test,
                    // 0b001 => ,
                    0b010 => Operation::Not,
                    0b011 => Operation::Neg,
                    0b100 => Operation::Mul,
                    0b101 => Operation::Imul,
                    0b110 => Operation::Div,
                    0b111 => Operation::Idiv,
                    _ => unreachable!(),
                },
                OperandSet::Destination(destination),
            ))
        }

        0xFE | 0xFF => {
            let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
            let modrm_byte = it_read_byte(it)?;
            let modrm = Modrm::try_from_byte(modrm_byte, it)?;

            let destination = Operand(modrm.register_or_memory.into(), operand_size);

            Ok(Instruction::new(
                match (modrm_byte >> 3) & 0b111 {
                    0b000 => Operation::Inc,
                    0b001 => Operation::Dec,
                    0b010 => Operation::Call,
                    0b011 => Operation::Call,
                    0b100 => Operation::Jmp,
                    0b101 => Operation::Jmp,
                    0b110 => Operation::Push,
                    0b111 => Operation::Push,
                    _ => unreachable!(),
                },
                OperandSet::Destination(destination),
            ))
        }

        _ => Err(Error::InvalidOpCode(op_code)),
    };

    maybe_instruction

    /*
    if let Ok(instruction) = maybe_instruction {
        return Ok(instruction);
    }

    match op_code {
        // Multi
        0x80 | 0x81 | 0x82 | 0x83 => operations::immediate_to_register_memory(op_code, it),

        0x08 | 0x09 | 0x0A | 0x0B => {
            operations::register_memory_and_register_to_either(Operation::Or, op_code, it)
        }

        0x30 | 0x31 | 0x32 | 0x33 => {
            operations::register_memory_and_register_to_either(Operation::Xor, op_code, it)
        }

        0x34 | 0x35 => operations::immediate_to_accumulator(Operation::Xor, op_code, it),

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
    */
}

mod test {
    #![allow(unused_imports)]
    #![allow(unused_macros)]

    use crate::modrm::RegisterOrMemory;
    use crate::Modrm;
    use mrc_x86::{
        Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
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

    /*
    #[test]
    fn test_00() {
        test_decoder!(
            &[0x00, 0xC3],
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
        test_decoder!(
            &[0x01, 0xC3],
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
        test_decoder!(
            &[0x02, 0xC3],
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
        test_decoder!(
            &[0x03, 0xC3], // add ax, bx
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
    fn test_04() {
        test_decoder!(
            &[0x04, 0x08], // add al, 0x8
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                    Operand(OperandType::Immediate(8), OperandSize::Byte),
                )
            )
        );
    }

    #[test]
    fn test_05() {
        test_decoder!(
            &[0x05, 0xC3, 0xE4], // add ax, 0xe4c3
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                    Operand(OperandType::Immediate(0xE4C3), OperandSize::Word),
                )
            )
        );
    }

    #[test]
    fn test_06() {
        test_decoder!(
            &[0x06], // push es
            Instruction::new(
                Operation::Push,
                OperandSet::Destination(Operand(
                    OperandType::Segment(Segment::Es),
                    OperandSize::Word
                ))
            )
        );
    }

    #[test]
    fn test_07() {
        test_decoder!(
            &[0x07], // pop es
            Instruction::new(
                Operation::Pop,
                OperandSet::Destination(Operand(
                    OperandType::Segment(Segment::Es),
                    OperandSize::Word
                ))
            )
        );
    }

    #[test]
    fn test_08() {
        test_decoder!(
            &[0x08, 0xc3, 0xe4], // or bl, al
            Instruction::new(
                Operation::Or,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte)
                )
            )
        );
    }

    #[test]
    fn test_09() {
        test_decoder!(
            &[0x09, 0xc3, 0xe4], // or bl, al
            Instruction::new(
                Operation::Or,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Word),
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Word)
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
            &[0xC1, 0xE0, 0x04], // shl ax, 0x4
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
    */

    #[test]
    fn test_00() {
        test_decoder!(
            &[0x00, 0xF7, 0xDD, 0x92, 0x59, 0xA1], // ADD BH,DH
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
        test_decoder!(
            &[0x01, 0x93, 0x57, 0x4F, 0x9E, 0x15],
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
    fn test_02() {
        test_decoder!(
            &[0x02, 0xE6, 0x2E, 0xB2, 0x3A, 0xF8],
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
    fn test_03() {
        test_decoder!(
            &[0x03, 0x82, 0xC1, 0x05, 0x07, 0xD9],
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
    fn test_04() {
        test_decoder!(
            &[0x04, 0x27, 0x08, 0xA3, 0x4F, 0x34],
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
    fn test_05() {
        test_decoder!(
            &[0x05, 0xCA, 0x57, 0x6D, 0x22, 0x3D],
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
    fn test_06() {
        test_decoder!(
            &[0x06, 0x62, 0x41, 0x01, 0xAA, 0x72],
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
    fn test_07() {
        test_decoder!(
            &[0x07, 0xD5, 0x55, 0x36, 0xDA, 0x37],
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
    fn test_08() {
        test_decoder!(
            &[0x08, 0xE6, 0xC2, 0x15, 0xAF, 0xFF],
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
    fn test_09() {
        test_decoder!(
            &[0x09, 0x2F, 0x77, 0xF1, 0x59, 0x99],
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
    fn test_0a() {
        test_decoder!(
            &[0x0A, 0xE7, 0x1C, 0xDC, 0xE5, 0xC2],
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
    fn test_0b() {
        test_decoder!(
            &[0x0B, 0xD0, 0xD7, 0xAB, 0xC6, 0x31],
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
    fn test_0c() {
        test_decoder!(
            &[0x0C, 0x9F, 0x80, 0xA5, 0xEA, 0x2A],
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
    fn test_0d() {
        test_decoder!(
            &[0x0D, 0x3A, 0xA0, 0x87, 0x85, 0x91],
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
    fn test_0e() {
        test_decoder!(
            &[0x0E, 0x0D, 0x52, 0x52, 0x30, 0x59],
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
    fn test_0f() {
        test_decoder!(
            &[0x0F, 0x8A, 0x77, 0x66, 0x6A, 0xC4],
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
    fn test_10() {
        test_decoder!(
            &[0x10, 0xBE, 0x65, 0x69, 0xAC, 0xF7],
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
    fn test_11() {
        test_decoder!(
            &[0x11, 0x70, 0x80, 0x94, 0xC1, 0x0C],
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
    fn test_12() {
        test_decoder!(
            &[0x12, 0xCA, 0x03, 0xF6, 0xAF, 0x96],
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
    fn test_13() {
        test_decoder!(
            &[0x13, 0xEC, 0xE6, 0x37, 0xD9, 0x28],
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
    fn test_14() {
        test_decoder!(
            &[0x14, 0x15, 0x95, 0x0B, 0x87, 0x43],
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
    fn test_15() {
        test_decoder!(
            &[0x15, 0x76, 0x67, 0x37, 0x73, 0x43],
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
    fn test_16() {
        test_decoder!(
            &[0x16, 0x6A, 0xF1, 0x1A, 0x21, 0xA0],
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
    fn test_17() {
        test_decoder!(
            &[0x17, 0xEA, 0x3D, 0xDB, 0xF3, 0x1E],
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
    fn test_18() {
        test_decoder!(
            &[0x18, 0x15, 0xC0, 0x92, 0xA8, 0xED],
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
    fn test_19() {
        test_decoder!(
            &[0x19, 0x01, 0x7D, 0x0C, 0xFE, 0x52],
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
    fn test_1a() {
        test_decoder!(
            &[0x1A, 0x9B, 0x3D, 0x20, 0x95, 0x01],
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
    fn test_1b() {
        test_decoder!(
            &[0x1B, 0xED, 0x76, 0x9B, 0x79, 0xF1],
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
    fn test_1c() {
        test_decoder!(
            &[0x1C, 0x31, 0xA3, 0xE5, 0xF4, 0x00],
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
    fn test_1d() {
        test_decoder!(
            &[0x1D, 0x65, 0xD6, 0x1A, 0x67, 0x69],
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
    fn test_1e() {
        test_decoder!(
            &[0x1E, 0xAC, 0x77, 0x40, 0x36, 0xC1],
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
    fn test_1f() {
        test_decoder!(
            &[0x1F, 0x07, 0x9E, 0x32, 0x4F, 0xE2],
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
    fn test_20() {
        test_decoder!(
            &[0x20, 0x3C, 0x7C, 0x19, 0x60, 0xBC],
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
    fn test_21() {
        test_decoder!(
            &[0x21, 0x94, 0x34, 0x36, 0xCF, 0x8E],
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
    fn test_22() {
        test_decoder!(
            &[0x22, 0xD3, 0x4C, 0x6A, 0xC2, 0xD4],
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
    fn test_23() {
        test_decoder!(
            &[0x23, 0x19, 0x70, 0x5F, 0x49, 0xFA],
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
    fn test_24() {
        test_decoder!(
            &[0x24, 0x51, 0xA3, 0xEA, 0x9A, 0x7F],
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
    fn test_25() {
        test_decoder!(
            &[0x25, 0x27, 0x8B, 0x3D, 0xEB, 0x91],
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
    fn test_26() {
        test_decoder!(
            &[0x26, 0xDA, 0x52, 0xB2, 0x23, 0xC1],
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
    fn test_27() {
        test_decoder!(
            &[0x27, 0x9A, 0xE7, 0x98, 0x82, 0xF9],
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
    fn test_28() {
        test_decoder!(
            &[0x28, 0xA5, 0x61, 0x52, 0x22, 0x3D],
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
    fn test_29() {
        test_decoder!(
            &[0x29, 0x38, 0x3B, 0xC8, 0xAC, 0xD3],
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
    fn test_2a() {
        test_decoder!(
            &[0x2A, 0x4B, 0x9F, 0xFC, 0xA5, 0xCD],
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
    fn test_2b() {
        test_decoder!(
            &[0x2B, 0x3A, 0x55, 0xEB, 0x6E, 0x82],
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
    fn test_2c() {
        test_decoder!(
            &[0x2C, 0x6E, 0xFD, 0xC2, 0xF1, 0x1B],
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
    fn test_2d() {
        test_decoder!(
            &[0x2D, 0x49, 0x9F, 0x21, 0x9F, 0xAE],
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
    fn test_2e() {
        test_decoder!(
            &[0x2E, 0x55, 0x83, 0x80, 0xC5, 0x75],
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
    fn test_2f() {
        test_decoder!(
            &[0x2F, 0x13, 0x71, 0x65, 0xD4, 0xFC],
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
    fn test_30() {
        test_decoder!(
            &[0x30, 0xF5, 0xF7, 0x53, 0xB1, 0xB9],
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
    fn test_31() {
        test_decoder!(
            &[0x31, 0xFD, 0x9A, 0x71, 0x35, 0xE0],
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
    fn test_32() {
        test_decoder!(
            &[0x32, 0x30, 0xD6, 0xA2, 0xBF, 0x32],
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
    fn test_33() {
        test_decoder!(
            &[0x33, 0x6A, 0xCD, 0x8D, 0xF0, 0x03],
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
    fn test_34() {
        test_decoder!(
            &[0x34, 0x21, 0x81, 0xA3, 0x7A, 0x05],
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
    fn test_35() {
        test_decoder!(
            &[0x35, 0x37, 0x83, 0x79, 0xE0, 0xE9],
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
    fn test_36() {
        test_decoder!(
            &[0x36, 0x8A, 0x86, 0xEC, 0x5E, 0x32],
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
    fn test_37() {
        test_decoder!(
            &[0x37, 0xF2, 0xFB, 0x61, 0xF5, 0x1B],
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
    fn test_38() {
        test_decoder!(
            &[0x38, 0xF9, 0x81, 0x83, 0xF9, 0x10],
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
    fn test_39() {
        test_decoder!(
            &[0x39, 0xC6, 0xAD, 0xF1, 0xB0, 0x33],
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
    fn test_3a() {
        test_decoder!(
            &[0x3A, 0xB1, 0xC8, 0x04, 0x98, 0xDE],
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
    fn test_3b() {
        test_decoder!(
            &[0x3B, 0x6C, 0x3A, 0x14, 0x6B, 0x08],
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
    fn test_3c() {
        test_decoder!(
            &[0x3C, 0xAB, 0x51, 0x27, 0x6D, 0x9A],
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
    fn test_3d() {
        test_decoder!(
            &[0x3D, 0x7A, 0x69, 0xFC, 0xC4, 0x20],
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
    fn test_3e() {
        test_decoder!(
            &[0x3E, 0xBA, 0x7D, 0x24, 0x33, 0x92],
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
    fn test_3f() {
        test_decoder!(
            &[0x3F, 0x47, 0x97, 0xB4, 0x68, 0xC8],
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
    fn test_40() {
        test_decoder!(
            &[0x40, 0x5A, 0xC2, 0x6A, 0xF7, 0xB8],
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
    fn test_41() {
        test_decoder!(
            &[0x41, 0x33, 0xA5, 0x42, 0xBC, 0x09],
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
    fn test_42() {
        test_decoder!(
            &[0x42, 0x8E, 0x2B, 0xD8, 0x42, 0x45],
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
    fn test_43() {
        test_decoder!(
            &[0x43, 0x63, 0x45, 0xC4, 0x2F, 0x0B],
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
    fn test_44() {
        test_decoder!(
            &[0x44, 0x87, 0xC8, 0x19, 0xE9, 0x75],
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
    fn test_45() {
        test_decoder!(
            &[0x45, 0xC3, 0xD8, 0x50, 0xE8, 0xCA],
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
    fn test_46() {
        test_decoder!(
            &[0x46, 0x4E, 0xE9, 0xB9, 0xE5, 0x98],
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
    fn test_47() {
        test_decoder!(
            &[0x47, 0x53, 0x9B, 0x2E, 0x14, 0x4D],
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
    fn test_48() {
        test_decoder!(
            &[0x48, 0x15, 0x8D, 0x53, 0x84, 0x39],
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
    fn test_49() {
        test_decoder!(
            &[0x49, 0xB5, 0xA8, 0x3E, 0x0B, 0xD7],
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
    fn test_4a() {
        test_decoder!(
            &[0x4A, 0xD7, 0x4E, 0x7F, 0x6D, 0xE8],
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
    fn test_4b() {
        test_decoder!(
            &[0x4B, 0xAB, 0xD7, 0x16, 0x0C, 0x59],
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
    fn test_4c() {
        test_decoder!(
            &[0x4C, 0xBB, 0xFF, 0x5A, 0x86, 0xA7],
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
    fn test_4d() {
        test_decoder!(
            &[0x4D, 0x15, 0x08, 0x37, 0xCF, 0xB1],
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
    fn test_4e() {
        test_decoder!(
            &[0x4E, 0x84, 0xC5, 0x4E, 0x55, 0x82],
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
    fn test_4f() {
        test_decoder!(
            &[0x4F, 0x90, 0xD6, 0x62, 0x9D, 0x00],
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
    fn test_50() {
        test_decoder!(
            &[0x50, 0x07, 0xB8, 0x32, 0x4B, 0xF8],
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
    fn test_51() {
        test_decoder!(
            &[0x51, 0x92, 0xB0, 0xFF, 0x8E, 0x55],
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
    fn test_52() {
        test_decoder!(
            &[0x52, 0x50, 0xE1, 0x00, 0x28, 0x4B],
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
    fn test_53() {
        test_decoder!(
            &[0x53, 0xE5, 0x26, 0x32, 0x89, 0x00],
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
    fn test_54() {
        test_decoder!(
            &[0x54, 0x18, 0xE9, 0xC6, 0x71, 0x50],
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
    fn test_55() {
        test_decoder!(
            &[0x55, 0xA1, 0x49, 0x14, 0x00, 0xC7],
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
    fn test_56() {
        test_decoder!(
            &[0x56, 0xD7, 0xF7, 0x7C, 0xE8, 0x5A],
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
    fn test_57() {
        test_decoder!(
            &[0x57, 0x35, 0xBB, 0x67, 0x87, 0x32],
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
    fn test_58() {
        test_decoder!(
            &[0x58, 0xD0, 0x6C, 0x59, 0xB4, 0x41],
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
    fn test_59() {
        test_decoder!(
            &[0x59, 0x07, 0xFA, 0x7A, 0x5B, 0xB2],
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
    fn test_5a() {
        test_decoder!(
            &[0x5A, 0x4A, 0x30, 0x5D, 0x74, 0x76],
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
    fn test_5b() {
        test_decoder!(
            &[0x5B, 0xC1, 0xDC, 0x74, 0xE5, 0x51],
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
    fn test_5c() {
        test_decoder!(
            &[0x5C, 0x82, 0x12, 0x7C, 0x03, 0x2A],
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
    fn test_5d() {
        test_decoder!(
            &[0x5D, 0x83, 0xB7, 0xBC, 0xDE, 0xD4],
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
    fn test_5e() {
        test_decoder!(
            &[0x5E, 0xA4, 0x66, 0xC7, 0x4A, 0xE8],
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
    fn test_5f() {
        test_decoder!(
            &[0x5F, 0x49, 0x11, 0x74, 0x5C, 0xD7],
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
    fn test_60() {
        test_decoder!(
            &[0x60, 0x37, 0x96, 0x49, 0x49, 0xD5],
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
    fn test_61() {
        test_decoder!(
            &[0x61, 0x23, 0xAB, 0xD2, 0x38, 0xE6],
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
    fn test_62() {
        test_decoder!(
            &[0x62, 0x4D, 0xDD, 0x3B, 0x63, 0x1F],
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
    fn test_63() {
        test_decoder!(
            &[0x63, 0x01, 0x65, 0x66, 0xBD, 0x4C],
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
    fn test_64() {
        test_decoder!(
            &[0x64, 0x47, 0x28, 0x4C, 0xA6, 0xEF],
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
    fn test_65() {
        test_decoder!(
            &[0x65, 0xEC, 0x71, 0x2E, 0x02, 0x5C],
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
    fn test_66() {
        test_decoder!(
            &[0x66, 0x9B, 0xDB, 0xE0, 0x8D, 0xCC],
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
    fn test_67() {
        test_decoder!(
            &[0x67, 0x20, 0x44, 0x17, 0x00, 0x88],
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
    fn test_68() {
        test_decoder!(
            &[0x68, 0xDD, 0x7C, 0xB9, 0xC7, 0x05],
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
    fn test_69() {
        test_decoder!(
            &[0x69, 0xD5, 0xB1, 0x69, 0xAA, 0x49],
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
    fn test_6a() {
        test_decoder!(
            &[0x6A, 0x64, 0xC6, 0x5D, 0x58, 0x5B],
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
    fn test_6b() {
        test_decoder!(
            &[0x6B, 0xAD, 0x58, 0x3F, 0x35, 0x21],
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
    fn test_6c() {
        test_decoder!(
            &[0x6C, 0x71, 0x48, 0xAE, 0xB3, 0xAB],
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
    fn test_6d() {
        test_decoder!(
            &[0x6D, 0xB7, 0x1D, 0xAC, 0xBC, 0x73],
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
    fn test_6e() {
        test_decoder!(
            &[0x6E, 0x56, 0x3F, 0x26, 0xBD, 0x03],
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
    fn test_6f() {
        test_decoder!(
            &[0x6F, 0x08, 0x4E, 0x74, 0x6C, 0x77],
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
    fn test_70() {
        test_decoder!(
            &[0x70, 0x99, 0xDB, 0xCB, 0x45, 0x22],
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
    fn test_71() {
        test_decoder!(
            &[0x71, 0x55, 0x87, 0xFD, 0x1C, 0xDE],
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
    fn test_72() {
        test_decoder!(
            &[0x72, 0x9F, 0xA9, 0x37, 0x99, 0x10],
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
    fn test_73() {
        test_decoder!(
            &[0x73, 0xAF, 0x90, 0x5E, 0xD1, 0x33],
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
    fn test_74() {
        test_decoder!(
            &[0x74, 0x99, 0x7C, 0x0D, 0x5C, 0x0E],
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
    fn test_75() {
        test_decoder!(
            &[0x75, 0x38, 0x60, 0xBD, 0x6B, 0x09],
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
    fn test_76() {
        test_decoder!(
            &[0x76, 0x0A, 0xC2, 0x33, 0x7B, 0x23],
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
    fn test_77() {
        test_decoder!(
            &[0x77, 0xC3, 0xB9, 0x28, 0xF5, 0x64],
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
    fn test_78() {
        test_decoder!(
            &[0x78, 0x2E, 0xCB, 0x5A, 0x0A, 0x32],
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
    fn test_79() {
        test_decoder!(
            &[0x79, 0xB9, 0x3C, 0x7A, 0x28, 0xE6],
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
    fn test_7a() {
        test_decoder!(
            &[0x7A, 0x82, 0x6D, 0xFE, 0x49, 0x00],
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
    fn test_7b() {
        test_decoder!(
            &[0x7B, 0xFA, 0x30, 0x70, 0xB8, 0x34],
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
    fn test_7c() {
        test_decoder!(
            &[0x7C, 0x48, 0x07, 0x85, 0x69, 0xCF],
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
    fn test_7d() {
        test_decoder!(
            &[0x7D, 0xA7, 0xA8, 0x88, 0x33, 0xBB],
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
    fn test_7e() {
        test_decoder!(
            &[0x7E, 0xBB, 0xD3, 0x8F, 0x33, 0x88],
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
    fn test_7f() {
        test_decoder!(
            &[0x7F, 0x1F, 0x2F, 0x95, 0x30, 0x19],
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
    fn test_80() {
        test_decoder!(
            &[0x80, 0x50, 0xFD, 0xE9, 0x1F, 0xD7],
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
    fn test_81() {
        test_decoder!(
            &[0x81, 0x1F, 0xDB, 0xCB, 0xCB, 0xC9],
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
    fn test_82() {
        test_decoder!(
            &[0x82, 0xAB, 0x96, 0x5C, 0xD0, 0x9D],
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
    fn test_83() {
        test_decoder!(
            &[0x83, 0x47, 0x4E, 0x37, 0xB2, 0x9E],
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
    fn test_84() {
        test_decoder!(
            &[0x84, 0x8C, 0x1C, 0xED, 0x66, 0xE1],
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
    fn test_85() {
        test_decoder!(
            &[0x85, 0x4F, 0xC0, 0x93, 0x93, 0x64],
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
    fn test_86() {
        test_decoder!(
            &[0x86, 0x6B, 0x4E, 0xF5, 0x7F, 0x2D],
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
    fn test_87() {
        test_decoder!(
            &[0x87, 0x74, 0x34, 0x9C, 0x6B, 0x2E],
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
    fn test_88() {
        test_decoder!(
            &[0x88, 0xE8, 0xB6, 0x37, 0xB3, 0x15],
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
    fn test_89() {
        test_decoder!(
            &[0x89, 0x44, 0x82, 0x80, 0x7F, 0x72],
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
    fn test_8a() {
        test_decoder!(
            &[0x8A, 0xEE, 0x6A, 0x25, 0x2F, 0x4C],
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
    fn test_8b() {
        test_decoder!(
            &[0x8B, 0xF7, 0xF8, 0xF1, 0x43, 0xE6],
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
    fn test_8c() {
        test_decoder!(
            &[0x8C, 0x96, 0x23, 0xFA, 0x5C, 0x41],
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
    fn test_8d() {
        test_decoder!(
            &[0x8D, 0x5C, 0x1D, 0xB3, 0x81, 0xFE],
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
    fn test_8e() {
        test_decoder!(
            &[0x8E, 0xC8, 0x21, 0xB5, 0x67, 0x77],
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
    fn test_8f() {
        test_decoder!(
            &[0x8F, 0x49, 0x04, 0x87, 0x8C, 0x61],
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
    fn test_90() {
        test_decoder!(
            &[0x90, 0x9A, 0xAE, 0x2B, 0x72, 0xB3],
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
    fn test_91() {
        test_decoder!(
            &[0x91, 0x9A, 0x0C, 0x61, 0x44, 0x5D],
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
    fn test_92() {
        test_decoder!(
            &[0x92, 0x4E, 0x02, 0x8B, 0x6B, 0x3C],
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
    fn test_93() {
        test_decoder!(
            &[0x93, 0x7D, 0x21, 0xC0, 0x14, 0x30],
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
    fn test_94() {
        test_decoder!(
            &[0x94, 0x2A, 0xB3, 0x45, 0xA5, 0x80],
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
    fn test_95() {
        test_decoder!(
            &[0x95, 0xBF, 0x9E, 0x62, 0x2E, 0x38],
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
    fn test_96() {
        test_decoder!(
            &[0x96, 0x50, 0xDC, 0xD8, 0xA4, 0x10],
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
    fn test_97() {
        test_decoder!(
            &[0x97, 0xC5, 0x94, 0x02, 0x28, 0x83],
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
    fn test_98() {
        test_decoder!(
            &[0x98, 0xC6, 0x85, 0xBD, 0x79, 0x1C],
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
    fn test_99() {
        test_decoder!(
            &[0x99, 0xBB, 0x06, 0x21, 0x57, 0xDA],
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
    fn test_9a() {
        test_decoder!(
            &[0x9A, 0xB9, 0x42, 0xAF, 0x99, 0x26],
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
    fn test_9b() {
        test_decoder!(
            &[0x9B, 0x44, 0xD6, 0x88, 0x6B, 0xDD],
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
    fn test_9c() {
        test_decoder!(
            &[0x9C, 0x0C, 0x43, 0xE8, 0x90, 0x4F],
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
    fn test_9d() {
        test_decoder!(
            &[0x9D, 0xE5, 0x8B, 0xE7, 0x38, 0x25],
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
    fn test_9e() {
        test_decoder!(
            &[0x9E, 0x98, 0x9D, 0x54, 0x24, 0x7C],
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
    fn test_9f() {
        test_decoder!(
            &[0x9F, 0x47, 0xF8, 0xE4, 0xCC, 0x58],
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
    fn test_a0() {
        test_decoder!(
            &[0xA0, 0x2F, 0x38, 0x4B, 0xCB, 0x67],
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
    fn test_a1() {
        test_decoder!(
            &[0xA1, 0x16, 0x62, 0xE8, 0x52, 0xE8],
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
    fn test_a2() {
        test_decoder!(
            &[0xA2, 0x2C, 0x21, 0x96, 0xE0, 0x94],
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
    fn test_a3() {
        test_decoder!(
            &[0xA3, 0x40, 0x0D, 0x9A, 0xD2, 0x63],
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
    fn test_a4() {
        test_decoder!(
            &[0xA4, 0x53, 0x39, 0xC4, 0xCF, 0x7B],
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
    fn test_a5() {
        test_decoder!(
            &[0xA5, 0x0D, 0x9A, 0xCA, 0xCF, 0xB7],
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
    fn test_a6() {
        test_decoder!(
            &[0xA6, 0xB1, 0x86, 0x4E, 0xF2, 0x1F],
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
    fn test_a7() {
        test_decoder!(
            &[0xA7, 0xF1, 0x75, 0x4F, 0xF9, 0xF3],
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
    fn test_a8() {
        test_decoder!(
            &[0xA8, 0x80, 0x80, 0x0D, 0x8D, 0x00],
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
    fn test_a9() {
        test_decoder!(
            &[0xA9, 0xC9, 0x7B, 0x5D, 0xD6, 0xF8],
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
    fn test_aa() {
        test_decoder!(
            &[0xAA, 0x79, 0xFA, 0x9C, 0xDC, 0x70],
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
    fn test_ab() {
        test_decoder!(
            &[0xAB, 0x42, 0x34, 0x64, 0x5E, 0x46],
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
    fn test_ac() {
        test_decoder!(
            &[0xAC, 0x3D, 0xD5, 0xF8, 0x22, 0x05],
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
    fn test_ad() {
        test_decoder!(
            &[0xAD, 0x92, 0x05, 0xE4, 0xAF, 0xB9],
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
    fn test_ae() {
        test_decoder!(
            &[0xAE, 0x3A, 0xFE, 0x3C, 0x27, 0x02],
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
    fn test_af() {
        test_decoder!(
            &[0xAF, 0x12, 0x49, 0x14, 0xB5, 0xA5],
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
    fn test_b0() {
        test_decoder!(
            &[0xB0, 0x41, 0xE5, 0x66, 0x70, 0x3B],
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
    fn test_b1() {
        test_decoder!(
            &[0xB1, 0x2D, 0x36, 0x1E, 0x9D, 0xC8],
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
    fn test_b2() {
        test_decoder!(
            &[0xB2, 0x9A, 0x0B, 0x99, 0x6F, 0xF8],
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
    fn test_b3() {
        test_decoder!(
            &[0xB3, 0x80, 0x05, 0xE6, 0xEC, 0xAA],
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
    fn test_b4() {
        test_decoder!(
            &[0xB4, 0x72, 0x36, 0x7F, 0xDD, 0x8F],
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
    fn test_b5() {
        test_decoder!(
            &[0xB5, 0x70, 0xEB, 0xE6, 0x1B, 0x83],
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
    fn test_b6() {
        test_decoder!(
            &[0xB6, 0x37, 0x56, 0x19, 0x9E, 0x0B],
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
    fn test_b7() {
        test_decoder!(
            &[0xB7, 0xD5, 0xAF, 0x28, 0x3F, 0x3E],
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
    fn test_b8() {
        test_decoder!(
            &[0xB8, 0x07, 0x1B, 0xEF, 0xD9, 0x2D],
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
    fn test_b9() {
        test_decoder!(
            &[0xB9, 0x3D, 0x35, 0x5E, 0xAD, 0x8E],
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
    fn test_ba() {
        test_decoder!(
            &[0xBA, 0x8A, 0x98, 0xD7, 0x91, 0x0F],
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
    fn test_bb() {
        test_decoder!(
            &[0xBB, 0xAD, 0x1D, 0x80, 0xC9, 0xD3],
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
    fn test_bc() {
        test_decoder!(
            &[0xBC, 0x61, 0xFB, 0x83, 0x06, 0xD4],
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
    fn test_bd() {
        test_decoder!(
            &[0xBD, 0x92, 0x49, 0x4F, 0xA0, 0xDB],
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
    fn test_be() {
        test_decoder!(
            &[0xBE, 0x0F, 0xC7, 0x19, 0x12, 0xFC],
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
    fn test_bf() {
        test_decoder!(
            &[0xBF, 0x6E, 0xA0, 0x04, 0x41, 0x1F],
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
    fn test_c0() {
        test_decoder!(
            &[0xC0, 0x5C, 0x5C, 0x27, 0xED, 0x3C],
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
    fn test_c1() {
        test_decoder!(
            &[0xC1, 0x84, 0x1E, 0xE9, 0x87, 0x03],
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
    fn test_c2() {
        test_decoder!(
            &[0xC2, 0x9F, 0xD8, 0x0C, 0xC9, 0x0F],
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
    fn test_c3() {
        test_decoder!(
            &[0xC3, 0x4B, 0xDC, 0x05, 0x55, 0x46],
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
    fn test_c4() {
        test_decoder!(
            &[0xC4, 0xC4, 0xC7, 0x97, 0x0B, 0x67],
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
    fn test_c5() {
        test_decoder!(
            &[0xC5, 0xE1, 0x11, 0xAB, 0x71, 0x69],
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
    fn test_c6() {
        test_decoder!(
            &[0xC6, 0x55, 0x5C, 0x0D, 0xF2, 0x54],
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
    fn test_c7() {
        test_decoder!(
            &[0xC7, 0xAD, 0xBD, 0x3C, 0x1B, 0x76],
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
    fn test_c8() {
        test_decoder!(
            &[0xC8, 0xA8, 0xF6, 0xC1, 0xF2, 0x63],
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
    fn test_c9() {
        test_decoder!(
            &[0xC9, 0xD3, 0xC7, 0x24, 0xE6, 0xD3],
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
    fn test_ca() {
        test_decoder!(
            &[0xCA, 0x99, 0xB4, 0x5F, 0xB3, 0xBB],
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
    fn test_cb() {
        test_decoder!(
            &[0xCB, 0xF8, 0x83, 0xBF, 0x14, 0x20],
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
    fn test_cc() {
        test_decoder!(
            &[0xCC, 0x27, 0x88, 0xE5, 0x3D, 0xAE],
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
    fn test_cd() {
        test_decoder!(
            &[0xCD, 0xBA, 0x2A, 0xED, 0x9E, 0x85],
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
    fn test_ce() {
        test_decoder!(
            &[0xCE, 0x5F, 0xB9, 0xF8, 0x38, 0x8C],
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
    fn test_cf() {
        test_decoder!(
            &[0xCF, 0x31, 0x89, 0xDA, 0x29, 0xBC],
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
    fn test_d0() {
        test_decoder!(
            &[0xD0, 0xB6, 0x44, 0x8A, 0x0F, 0x70],
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
    fn test_d1() {
        test_decoder!(
            &[0xD1, 0x00, 0x8D, 0x13, 0x97, 0x5E],
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
    fn test_d2() {
        test_decoder!(
            &[0xD2, 0x8E, 0xF0, 0xAB, 0x5B, 0xD5],
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
    fn test_d3() {
        test_decoder!(
            &[0xD3, 0x60, 0x21, 0x55, 0xF8, 0xFC],
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
    fn test_d4() {
        test_decoder!(
            &[0xD4, 0x63, 0x73, 0xBE, 0xCF, 0x6A],
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
    fn test_d5() {
        test_decoder!(
            &[0xD5, 0x65, 0xC5, 0xCF, 0x52, 0x78],
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
    fn test_d6() {
        test_decoder!(
            &[0xD6, 0x22, 0x03, 0xE1, 0xAD, 0x5C],
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
    fn test_d7() {
        test_decoder!(
            &[0xD7, 0x01, 0xF2, 0xDA, 0x33, 0xA0],
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
    fn test_d8() {
        test_decoder!(
            &[0xD8, 0x1B, 0x4D, 0x1C, 0x13, 0x5B],
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
    fn test_d9() {
        test_decoder!(
            &[0xD9, 0xA1, 0x35, 0xE8, 0x46, 0x10],
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
    fn test_da() {
        test_decoder!(
            &[0xDA, 0x6F, 0x2D, 0x3A, 0x0B, 0x06],
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
    fn test_db() {
        test_decoder!(
            &[0xDB, 0xB0, 0xDA, 0x49, 0xD1, 0x89],
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
    fn test_dc() {
        test_decoder!(
            &[0xDC, 0x0B, 0xD9, 0xC2, 0x3F, 0x84],
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
    fn test_dd() {
        test_decoder!(
            &[0xDD, 0xCB, 0x51, 0x61, 0x17, 0x3F],
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
    fn test_de() {
        test_decoder!(
            &[0xDE, 0x7F, 0x78, 0xC9, 0xEB, 0x5E],
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
    fn test_df() {
        test_decoder!(
            &[0xDF, 0xBA, 0x46, 0x18, 0x67, 0xE6],
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
    fn test_e0() {
        test_decoder!(
            &[0xE0, 0x30, 0x0B, 0x13, 0x59, 0x89],
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
    fn test_e1() {
        test_decoder!(
            &[0xE1, 0x70, 0x7F, 0x82, 0x32, 0x05],
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
    fn test_e2() {
        test_decoder!(
            &[0xE2, 0x82, 0xEA, 0x87, 0xA5, 0x0D],
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
    fn test_e3() {
        test_decoder!(
            &[0xE3, 0x26, 0x43, 0x1F, 0x31, 0xAC],
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
    fn test_e4() {
        test_decoder!(
            &[0xE4, 0xF6, 0x79, 0x84, 0x52, 0x0D],
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
    fn test_e5() {
        test_decoder!(
            &[0xE5, 0x43, 0x51, 0xFB, 0xE1, 0x6D],
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
    fn test_e6() {
        test_decoder!(
            &[0xE6, 0x78, 0xEB, 0x22, 0x1A, 0x7D],
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
    fn test_e7() {
        test_decoder!(
            &[0xE7, 0x5B, 0x6F, 0x21, 0x86, 0x92],
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
    fn test_e8() {
        test_decoder!(
            &[0xE8, 0x48, 0xDE, 0x73, 0x50, 0xA1],
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
    fn test_e9() {
        test_decoder!(
            &[0xE9, 0x7E, 0x5F, 0xA3, 0x0A, 0x1C],
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
    fn test_ea() {
        test_decoder!(
            &[0xEA, 0xF7, 0xFE, 0xEB, 0x15, 0x0D],
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
    fn test_eb() {
        test_decoder!(
            &[0xEB, 0xA7, 0xBA, 0xDB, 0xF1, 0x44],
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
    fn test_ec() {
        test_decoder!(
            &[0xEC, 0x5A, 0x56, 0x20, 0x2E, 0xDD],
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
    fn test_ed() {
        test_decoder!(
            &[0xED, 0xFF, 0x3E, 0xCF, 0xDE, 0x32],
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
    fn test_ee() {
        test_decoder!(
            &[0xEE, 0xF7, 0x98, 0xFF, 0xF8, 0x84],
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
    fn test_ef() {
        test_decoder!(
            &[0xEF, 0x64, 0x85, 0x96, 0xD4, 0xFA],
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
    fn test_f0() {
        test_decoder!(
            &[0xF0, 0x1A, 0xD5, 0x4C, 0xE3, 0x8E],
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
    fn test_f1() {
        test_decoder!(
            &[0xF1, 0x0D, 0x85, 0x19, 0x65, 0x6B],
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
    fn test_f2() {
        test_decoder!(
            &[0xF2, 0xD0, 0x15, 0x36, 0x19, 0xAC],
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
    fn test_f3() {
        test_decoder!(
            &[0xF3, 0x32, 0xF2, 0x81, 0xD5, 0x6C],
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
    fn test_f4() {
        test_decoder!(
            &[0xF4, 0x1D, 0xE6, 0x81, 0x6F, 0xC5],
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
    fn test_f5() {
        test_decoder!(
            &[0xF5, 0x83, 0x46, 0x2E, 0x06, 0x92],
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
    fn test_f6() {
        test_decoder!(
            &[0xF6, 0x59, 0x38, 0x70, 0xC5, 0xAF],
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
    fn test_f7() {
        test_decoder!(
            &[0xF7, 0xC0, 0xDE, 0x80, 0x87, 0x33],
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
    fn test_f8() {
        test_decoder!(
            &[0xF8, 0xC7, 0x90, 0x30, 0x15, 0x1F],
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
    fn test_f9() {
        test_decoder!(
            &[0xF9, 0x76, 0x1D, 0xDE, 0x72, 0xAD],
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
    fn test_fa() {
        test_decoder!(
            &[0xFA, 0x9C, 0x0A, 0x3B, 0x0C, 0xA1],
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
    fn test_fb() {
        test_decoder!(
            &[0xFB, 0xC3, 0x19, 0xB8, 0x3F, 0x23],
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
    fn test_fc() {
        test_decoder!(
            &[0xFC, 0xF5, 0x3D, 0x7B, 0x87, 0xF8],
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
    fn test_fd() {
        test_decoder!(
            &[0xFD, 0x99, 0x57, 0x2F, 0x0B, 0xF9],
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
    fn test_fe() {
        test_decoder!(
            &[0xFE, 0x47, 0x2C, 0x5E, 0xBE, 0xA1],
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
    fn test_ff() {
        test_decoder!(
            &[0xFF, 0xA9, 0x02, 0xF9, 0x86, 0x20],
            Instruction::new(
                Operation::Add,
                OperandSet::DestinationAndSource(
                    Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                    Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                )
            )
        );
    }
}
