use crate::errors::Result;
use crate::reader::ReadExt;
use crate::TryFromByte;
use mrc_instruction::{
    Instruction, Operand, OperandSet, OperandSize, Operation, Register, Segment, SizedRegister,
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

    let (mrrm, _) = it.read_mrrm_and_byte()?;

    let reg_mem = mrrm.register_or_memory.into_operand_kind(operand_size);
    let reg = Operand::Register(SizedRegister(mrrm.register, operand_size));

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

    let (mrrm, mrrm_byte) = it.read_mrrm_and_byte()?;

    let destination = mrrm.register_or_memory.into_operand_kind(OperandSize::Word);
    let source = Operand::Segment(Segment::try_from_byte((mrrm_byte >> 3) & 0b111)?);

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

    Ok(Instruction::new(
        operation,
        OperandSet::DestinationAndSource(
            Operand::Register(SizedRegister(Register::AlAx, operand_size)),
            it.read_immediate(operand_size)?.into(),
        ),
    ))
}
