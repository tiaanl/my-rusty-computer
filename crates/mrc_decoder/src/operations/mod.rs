use crate::errors::Result;
use crate::{Error, Modrm, TryFromByte};
use mrc_instruction::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register, Segment,
};

// x x x x x x d w | mod reg r/m
pub(crate) fn register_memory_and_register_to_either<It: Iterator<Item = u8>>(
    operation: Operation,
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let direction = (op_code >> 1) & 1;
    let operand_size = OperandSize::try_from_byte(op_code & 0b1)?;

    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    let destination = Operand(OperandType::Register(modrm.register), operand_size);
    let source = Operand(modrm.register_or_memory.into(), operand_size);

    Ok(Instruction::new(
        operation,
        match direction {
            0 => OperandSet::DestinationAndSource(source, destination),
            _ => OperandSet::DestinationAndSource(destination, source),
        },
    ))
}

// x x x x x x x w | data | data if w = 1
pub(crate) fn immediate_to_accumulator<It: Iterator<Item = u8>>(
    operation: Operation,
    op_code: u8,
    it: &mut It,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_byte(op_code & 0b1)?;

    let mut immediate = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    } as u16;

    if operand_size == OperandSize::Word {
        let next_byte = match it.next() {
            Some(byte) => byte,
            None => return Err(Error::CouldNotReadExtraBytes),
        } as u16;
        immediate |= next_byte << 8;
    }

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand(OperandType::Register(Register::AlAx), operand_size),
            Operand(OperandType::Immediate(immediate), operand_size),
        ),
    ))
}

pub(crate) fn push_pop_segment<It: Iterator<Item = u8>>(
    op_code: u8,
    _: &mut It,
) -> Result<Instruction> {
    Ok(Instruction::new(
        match op_code & 1 {
            0 => Operation::Push,
            _ => Operation::Pop,
        },
        OperandSet::Destination(Operand(
            OperandType::Segment(Segment::try_from_byte(op_code >> 3 & 0b111)?),
            OperandSize::Word,
        )),
    ))
}
