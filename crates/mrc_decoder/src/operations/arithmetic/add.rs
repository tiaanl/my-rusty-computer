use crate::decode::DataIterator;
use crate::errors::Result;
use crate::{it_read_u16, it_read_u8, LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation};

// Register/Memory with register to either
// 0 0 0 0 0 0 d w | mod reg r/m
pub fn register_memory_with_register_to_either<It: DataIterator>(
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let modrm_byte = it.consume();
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    Ok(Instruction::new(
        Operation::Add,
        OperandSet::DestinationAndSource(
            Operand(OperandType::Register(modrm.register), operand_size),
            Operand(modrm.register_or_memory.into(), operand_size),
        ),
    ))
}

// 1 0 0 0 0 0 s w | mod 0 0 0 r/m | data | data if sw = 01
pub fn immediate_to_register_memory<It: DataIterator>(
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    let modrm_byte = it.consume();

    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

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

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand(modrm.register_or_memory.into(), operand_size),
            Operand(
                OperandType::Immediate(match operand_size {
                    OperandSize::Byte => it_read_u8(it).into(),
                    OperandSize::Word => it_read_u16(it),
                }),
                operand_size,
            ),
        ),
    ))
}