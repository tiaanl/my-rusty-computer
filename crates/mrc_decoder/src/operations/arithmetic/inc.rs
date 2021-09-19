use crate::errors::Result;
use crate::{Error, LowBitsDecoder, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register};

// 1 1 1 1 1 1 1 w | mod 0 0 0 r/m
pub fn register_memory<It: Iterator<Item = u8>>(op_code: u8, it: &mut It) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;
    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    Ok(Instruction::new(
        Operation::Inc,
        OperandSet::Destination(Operand(modrm.register_or_memory.into(), operand_size)),
    ))
}

// 0 1 0 0 0 reg
pub fn register<It: Iterator<Item = u8>>(op_code: u8, _: &mut It) -> Result<Instruction> {
    let register = Register::try_from_low_bits(op_code & 0b111)?;

    Ok(Instruction::new(
        Operation::Inc,
        OperandSet::Destination(Operand(OperandType::Register(register), OperandSize::Word)),
    ))
}
