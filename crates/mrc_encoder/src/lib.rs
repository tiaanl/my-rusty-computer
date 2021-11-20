use mrc_instruction::{
    Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register,
};

pub fn encode_instruction(instruction: &Instruction) -> Result<Vec<u8>, bool> {
    match instruction.operation {
        Operation::Add => match instruction.operands {
            // Immediate to Accumulator 0000010w data data if w e 1
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), destination_operand_size),
                Operand(OperandType::Immediate(value), source_operand_size),
            ) if destination_operand_size == source_operand_size => {
                let op_code = set_op_code_size_bit(0b00000100, destination_operand_size);
                let mut bytes = vec![op_code];
                match destination_operand_size {
                    OperandSize::Byte => encode_immediate_byte(&mut bytes, value as u8),
                    OperandSize::Word => encode_immediate_word(&mut bytes, value),
                }
                Ok(bytes)
            }

            // Reg./Memory with Register to Either 0 0 0 0 0 0 d w mod reg r/m
            // Immediate to Register/Memory 1 0 0 0 0 0 s w mod 0 0 0 r/m data data if s: w e 01
            OperandSet::DestinationAndSource(
                Operand(_, destination_operand_size),
                Operand(OperandType::Immediate(value), source_operand_size),
            ) if destination_operand_size == source_operand_size => {
                let op_code = set_op_code_size_bit(0b10000000, destination_operand_size);

                let mut bytes = vec![op_code];
                match destination_operand_size {
                    OperandSize::Byte => encode_immediate_byte(&mut bytes, value as u8),
                    OperandSize::Word => encode_immediate_word(&mut bytes, value),
                }
                Ok(bytes)
            }

            _ => unreachable!(),
        },
        _ => todo!(),
    }
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

    #[test]
    fn basic() {
        let result = encode_instruction(&Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                Operand(OperandType::Immediate(0x10), OperandSize::Byte),
            ),
        ));
        assert!(result.is_ok());
        assert_eq!(vec![0x04, 0x10], result.unwrap());

        let result = encode_instruction(&Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                Operand(OperandType::Immediate(0x1020), OperandSize::Word),
            ),
        ));
        assert!(result.is_ok());
        assert_eq!(vec![0x05, 0x20, 0x10], result.unwrap());
    }
}
