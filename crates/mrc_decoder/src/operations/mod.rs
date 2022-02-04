use crate::decode::{immediate_operand_from_it, modrm_and_byte};
use crate::errors::Result;
use crate::TryFromByte;
use mrc_instruction::{
    Instruction, Operand, OperandKind, OperandSet, OperandSize, Operation, Register, Segment,
};

#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) enum Direction {
    Detect,
    RegFirst,
    RegMemFirst,
}

// x x x x x x d w | mod reg r/m
pub(crate) fn register_or_memory_and_register(
    operation: Operation,
    direction: Direction,
    force_operand_size: Option<OperandSize>,
    op_code: u8,
    it: &mut impl Iterator<Item = u8>,
) -> Result<Instruction> {
    let reg_first = match direction {
        Direction::Detect => (op_code >> 1) & 1 == 1,
        Direction::RegFirst => true,
        Direction::RegMemFirst => false,
    };
    let operand_size = match force_operand_size {
        Some(operand_size) => operand_size,
        None => {
            if op_code & 0b1 == 1 {
                OperandSize::Word
            } else {
                OperandSize::Byte
            }
        }
    };

    let (modrm, _) = modrm_and_byte(it)?;

    let reg_mem = Operand(modrm.register_or_memory.into(), operand_size);
    let reg = Operand(OperandKind::Register(modrm.register), operand_size);

    Ok(Instruction::new(
        operation,
        if reg_first {
            OperandSet::DestinationAndSource(reg, reg_mem)
        } else {
            OperandSet::DestinationAndSource(reg_mem, reg)
        },
    ))
}

// x x x x x x d w | mod reg r/m
pub(crate) fn register_or_memory_and_segment(
    operation: Operation,
    op_code: u8,
    it: &mut impl Iterator<Item = u8>,
) -> Result<Instruction> {
    let direction = (op_code >> 1) & 1;

    let (modrm, modrm_byte) = modrm_and_byte(it)?;

    let destination = Operand(modrm.register_or_memory.into(), OperandSize::Word);
    let source = Operand(
        OperandKind::Segment(Segment::try_from_byte((modrm_byte >> 3) & 0b111)?),
        OperandSize::Word,
    );

    Ok(Instruction::new(
        operation,
        match direction {
            0 => OperandSet::DestinationAndSource(destination, source),
            _ => OperandSet::DestinationAndSource(source, destination),
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
