#[derive(Debug)]
pub enum Operation {
    Add,
    Call,
    Cli,
    Jmp,
    Mov,
    Sti,
}

#[derive(PartialEq, Debug)]
pub enum Register {
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
pub enum SegmentEncoding {
    Es,
    Cs,
    Ss,
    Ds,
}

#[derive(Debug)]
pub enum DataSize {
    Byte,
    Word,
}

#[derive(Debug, PartialEq)]
pub enum AddressingMode {
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
    Direct(u16),
    Indirect(AddressingMode, u16),
    Register(Register),
    Segment(SegmentEncoding),
    Immediate(u16),
}

#[derive(Debug)]
pub enum OperandSet {
    None,
    DestinationAndSource(Operand, Operand, DataSize),
    SegmentAndOffset(u16, u16),
    Offset(u16),
}

#[derive(Debug)]
pub struct Instruction {
    pub operation: Operation,
    pub segment_override: Option<SegmentEncoding>,
    pub operands: OperandSet,
}

impl Instruction {
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            segment_override: None,
            operands,
        }
    }

    pub fn with_segment_override(
        operation: Operation,
        segment_override: SegmentEncoding,
        operands: OperandSet,
    ) -> Self {
        Self {
            operation,
            segment_override: Some(segment_override),
            operands,
        }
    }
}
