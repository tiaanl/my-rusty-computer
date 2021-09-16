use crate::decode::DataIterator;
use crate::errors::Result;
use crate::{LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation};

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
