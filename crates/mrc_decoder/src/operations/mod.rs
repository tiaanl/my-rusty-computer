use crate::decode::immediate_operand_from_it;
use crate::errors::Result;
use crate::{Error, Modrm, TryFromByte};
use mrc_instruction::{
    Instruction, Operand, OperandKind, OperandSet, OperandSize, Operation, Register, Segment,
};

// x x x x x x d w | mod reg r/m
pub(crate) fn register_memory_and_register_to_either(
    operation: Operation,
    op_code: u8,
    it: &mut impl Iterator<Item = u8>,
) -> Result<Instruction> {
    let direction = (op_code >> 1) & 1;
    let operand_size = OperandSize::try_from_byte(op_code & 0b1)?;

    let modrm_byte = match it.next() {
        Some(byte) => byte,
        None => return Err(Error::CouldNotReadExtraBytes),
    };
    let modrm = Modrm::try_from_byte(modrm_byte, it)?;

    let destination = Operand(OperandKind::Register(modrm.register), operand_size);
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
pub(crate) fn immediate_to_accumulator(
    operation: Operation,
    op_code: u8,
    it: &mut impl Iterator<Item = u8>,
) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_byte(op_code & 0b1)?;

    let immediate = immediate_operand_from_it(it, operand_size)?;

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand(OperandKind::Register(Register::AlAx), operand_size),
            immediate,
        ),
    ))
}

pub(crate) fn push_pop_segment(
    op_code: u8,
    _: &mut impl Iterator<Item = u8>,
) -> Result<Instruction> {
    Ok(Instruction::new(
        match op_code & 1 {
            0 => Operation::PUSH,
            _ => Operation::POP,
        },
        OperandSet::Destination(Operand(
            OperandKind::Segment(Segment::try_from_byte(op_code >> 3 & 0b111)?),
            OperandSize::Word,
        )),
    ))
}
