use crate::errors::Result;
use crate::{Error, LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation};

// 0 0 0 0 0 0 d w | mod reg r/m
pub(crate) fn register_memory_with_register_to_either<It: Iterator<Item = u8>>(
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let direction = op_code >> 1 & 0b1;

    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    let destination = Operand(OperandType::Register(modrm.register), operand_size);
    let source = Operand(modrm.register_or_memory.into(), operand_size);
    Ok(Instruction::new(
        Operation::Add,
        match direction {
            0 => OperandSet::DestinationAndSource(source, destination),
            _ => OperandSet::DestinationAndSource(destination, source),
        },
    ))
}
