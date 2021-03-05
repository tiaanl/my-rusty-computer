#[derive(Debug)]
pub enum Operation {
    Add,
}

#[derive(Debug)]
pub enum RegisterEncoding {
    AlAx = 0,
    ClCx = 1,
    DlDx = 2,
    BlBx = 3,
    AhSp = 4,
    ChBp = 5,
    DhSi = 6,
    BhDi = 7,
}

#[derive(Debug)]
pub enum DataSize {
    Byte,
    Word,
}

#[derive(Debug)]
pub enum Operand {
    None,

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
