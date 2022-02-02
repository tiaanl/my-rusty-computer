#![allow(unused)]

use mrc_instruction::{Instruction, OperandSize};

pub fn encode_instruction(instruction: &Instruction) -> Result<Vec<u8>, bool> {
    todo!()
}

fn encode_immediate_byte(bytes: &mut Vec<u8>, value: u8) {
    bytes.push(value);
}

fn encode_immediate_word(bytes: &mut Vec<u8>, value: u16) {
    for byte in value.to_le_bytes() {
        bytes.push(byte)
    }
}

fn set_op_code_size_bit(op_code: u8, size: OperandSize) -> u8 {
    match size {
        OperandSize::Byte => op_code,
        OperandSize::Word => op_code | 0b00000001,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use mrc_instruction::{Immediate, Operand, OperandKind, OperandSet, Operation, Register};

    // #[test]
    fn basic() {
        let result = encode_instruction(&Instruction::new(
            Operation::ADD,
            OperandSet::DestinationAndSource(
                Operand(OperandKind::Register(Register::AlAx), OperandSize::Byte),
                Operand(
                    OperandKind::Immediate(Immediate::Byte(0x10)),
                    OperandSize::Byte,
                ),
            ),
        ));
        assert!(result.is_ok());
        assert_eq!(vec![0x04, 0x10], result.unwrap());

        let result = encode_instruction(&Instruction::new(
            Operation::ADD,
            OperandSet::DestinationAndSource(
                Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
                Operand(
                    OperandKind::Immediate(Immediate::Word(0x1020)),
                    OperandSize::Word,
                ),
            ),
        ));
        assert!(result.is_ok());
        assert_eq!(vec![0x05, 0x20, 0x10], result.unwrap());
    }
}
