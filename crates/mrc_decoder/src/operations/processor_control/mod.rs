use crate::errors::Result;
use crate::DataIterator;
use mrc_x86::{Instruction, OperandSet, Operation};

// 1 1 1 1 1 0 0 0
pub fn clear_carry<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Clc, OperandSet::None))
}

// 1 1 1 1 0 1 0 1
pub fn complimentary_carry<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Cmc, OperandSet::None))
}

// 1 1 1 1 1 0 0 1
pub fn set_carry<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Stc, OperandSet::None))
}

// 1 1 1 1 1 1 0 0
pub fn clear_direction<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Cld, OperandSet::None))
}

// 1 1 1 1 1 1 0 1
pub fn set_direction<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Std, OperandSet::None))
}

// 1 1 1 1 1 0 1 0
pub fn clear_interrupt<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Cli, OperandSet::None))
}

// 1 1 1 1 1 0 1 1
pub fn set_interrupt<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Sti, OperandSet::None))
}

// 1 1 1 1 0 1 0 0
pub fn halt<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Hlt, OperandSet::None))
}

// 1 0 0 1 1 0 1 1
pub fn wait<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Wait, OperandSet::None))
}

// 1 1 0 1 1 x x x
pub fn escape_to_external_device<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    todo!()
    // it.consume();
    // Ok(Instruction::new(Operation::Esc, OperandSet::None))
}

// 1 1 1 1 0 0 0 0
pub fn bus_lock_prefix<It: DataIterator>(_: u8, _: &mut It) -> Result<Instruction> {
    Ok(Instruction::new(Operation::Lock, OperandSet::None))
}
