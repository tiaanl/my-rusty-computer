use crate::modrm::RegisterOrMemory;
use crate::{decode_instruction, DataIterator, Modrm};
use mrc_x86::{Instruction, Operand, OperandSet, OperandSize, OperandType, Operation, Register};

struct TestIterator {
    data: Vec<u8>,
    position: u16,
}

impl TestIterator {
    fn from_bytes(bytes: &[u8]) -> Self {
        Self {
            data: Vec::from(bytes),
            position: 0,
        }
    }
}

impl DataIterator for TestIterator {
    fn peek(&self) -> u8 {
        self.data[self.position as usize]
    }

    fn consume(&mut self) -> u8 {
        let r = self.peek();
        self.advance();
        r
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[test]
fn test_00() {
    let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

    let mut it = TestIterator::from_bytes(&[0x00, modrm.into(), 0xFE]);
    let instruction = decode_instruction(&mut it);
    assert_eq!(
        Ok(Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
                Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
            )
        )),
        instruction
    );
}

#[test]
fn test_01() {
    let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

    let mut it = TestIterator::from_bytes(&[0x01, modrm.into(), 0xFE]);
    let instruction = decode_instruction(&mut it);
    assert_eq!(
        Ok(Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
                Operand(OperandType::Register(Register::BlBx), OperandSize::Word),
            )
        )),
        instruction
    );
}

#[test]
fn test_02() {
    let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

    let mut it = TestIterator::from_bytes(&[0x02, modrm.into(), 0xFE]);
    let instruction = decode_instruction(&mut it);
    assert_eq!(
        Ok(Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::BlBx), OperandSize::Byte),
                Operand(OperandType::Register(Register::AlAx), OperandSize::Byte),
            )
        )),
        instruction
    );
}

#[test]
fn test_03() {
    let modrm = Modrm::new(Register::AlAx, RegisterOrMemory::Register(Register::BlBx));

    let mut it = TestIterator::from_bytes(&[0x03, modrm.into(), 0xFE]);
    let instruction = decode_instruction(&mut it);
    assert_eq!(
        Ok(Instruction::new(
            Operation::Add,
            OperandSet::DestinationAndSource(
                Operand(OperandType::Register(Register::BlBx), OperandSize::Word),
                Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
            )
        )),
        instruction
    );
}
