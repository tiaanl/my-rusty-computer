use crate::errors::Result;
use crate::{LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation};

// 0 0 0 0 1 0 d w | mod reg r/m
pub fn register_memory_and_register_to_either<It: Iterator<Item = u8>>(
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let modrm = Modrm::try_from_iter(it)?;

    Ok(Instruction::new(
        Operation::Or,
        OperandSet::DestinationAndSource(
            Operand(OperandType::Register(modrm.register), operand_size),
            Operand(modrm.register_or_memory.into(), operand_size),
        ),
    ))
}
