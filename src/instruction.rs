#[derive(Debug)]
pub enum Operation {
    Add,
}

#[derive(PartialEq, Debug)]
pub enum RegisterEncoding {
    AlAx,
    ClCx,
    DlDx,
    BlBx,
    AhSp,
    ChBp,
    DhSi,
    BhDi,
}

#[derive(Debug)]
pub enum DataSize {
    Byte,
    Word,
}

#[derive(Debug, PartialEq)]
pub enum IndirectMemoryEncoding {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx,
}

#[derive(Debug)]
pub enum Operand {
    None,

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
