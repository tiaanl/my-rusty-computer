#[derive(Debug)]
pub enum Operation {
    Add,
}

#[derive(PartialEq, Debug)]
pub enum RegisterEncoding {
    AlAx = 0b000,
    ClCx = 0b001,
    DlDx = 0b010,
    BlBx = 0b011,
    AhSp = 0b100,
    ChBp = 0b101,
    DhSi = 0b110,
    BhDi = 0b111,
}

#[derive(Debug)]
pub enum DataSize {
    Byte,
    Word,
}

#[derive(Debug, PartialEq)]
pub enum IndirectMemoryEncoding {
    BxSi = 0b000,
    BxDi = 0b001,
    BpSi = 0b010,
    BpDi = 0b011,
    Si = 0b100,
    Di = 0b101,
    Bp = 0b110,
    Bx = 0b111,
}

#[derive(Debug)]
pub enum Operand {
    Indirect(IndirectMemoryEncoding, u16),
    Register(RegisterEncoding),
    Immediate(u16),
}

#[derive(Debug)]
pub struct Instruction {
    pub operation: Operation,
    pub data_size: DataSize,
    pub destination: Operand,
    pub source: Operand,
}

impl Instruction {
    pub fn new(
        operation: Operation,
        data_size: DataSize,
        destination: Operand,
        source: Operand,
    ) -> Self {
        Self {
            operation,
            data_size,
            destination,
            source,
        }
    }
}
