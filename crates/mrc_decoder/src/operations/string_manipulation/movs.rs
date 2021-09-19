use crate::errors::Result;
use crate::LowBitsDecoder;
use mrc_x86::{Instruction, OperandSet, OperandSize, Operation};

// 1 0 1 0 0 1 0 w
pub fn move_byte_word<It: Iterator>(op_code: u8, _: &mut It) -> Result<Instruction> {
    let operand_size = OperandSize::try_from_low_bits(op_code & 0b1)?;

    Ok(Instruction::new(
        match operand_size {
            OperandSize::Byte => Operation::Movsb,
            OperandSize::Word => Operation::Movsw,
        },
        OperandSet::None,
    ))
}
