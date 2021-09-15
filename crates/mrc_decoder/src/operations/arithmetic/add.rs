use crate::decode::ByteAndExtra;
use crate::errors::Result;
use crate::{ByteReader, ByteSize, DecodeResult, LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation};

// Register/Memory with register to either
// 0 0 0 0 0 0 d w | mod reg r/m
pub fn register_memory_with_register_to_either(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let (modrm_byte, bytes) = bytes.byte_and_extra();
    let (modrm, _, modrm_extra_bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;

    Ok(DecodeResult {
        bytes_read: 2 + modrm_extra_bytes_read,
        instruction: Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(modrm.register), operand_size),
                Operand(modrm.register_or_memory.into(), operand_size),
            ),
        ),
    })
}

// 1 0 0 0 0 0 s w | mod 0 0 0 r/m | data | data if sw = 01
pub fn immediate_to_register_memory(op_code: u8, bytes: &[u8]) -> Result<DecodeResult> {
    let mut bytes_read: usize = 1; // op_code

    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    bytes_read += operand_size.byte_size(); // we are going to read an immediate

    let (modrm_byte, bytes) = bytes.byte_and_extra();

    let (modrm, bytes, modrm_bytes_read) = Modrm::try_from_byte(modrm_byte, bytes)?;
    bytes_read += modrm_bytes_read;

    let operation = match (modrm_byte >> 3) & 0b111 {
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

    // Add the amount of bytes we will read for the immediate value.
    bytes_read += operand_size.byte_size();

    Ok(DecodeResult {
        bytes_read,
        instruction: Instruction::new(
            operation,
            OperandSet::DestinationAndSource(
                Operand(modrm.register_or_memory.into(), operand_size),
                Operand(
                    OperandType::Immediate(match operand_size {
                        OperandSize::Byte => bytes.read_u8()?.into(),
                        OperandSize::Word => bytes.read_u16()?,
                    }),
                    operand_size,
                ),
            ),
        ),
    })
}
