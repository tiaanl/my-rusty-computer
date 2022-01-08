use mrc_instruction::{AddressingMode, OperandSize, Operation, Segment, SizedRegister};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SourceValueOrLabel {
    Value(i32),
    Label(String),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SourceOperand {
    Direct(SourceValueOrLabel, Option<OperandSize>),
    Indirect(AddressingMode, Option<OperandSize>),
    Register(SizedRegister),
    Segment(Segment),
    Immediate(SourceValueOrLabel),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum SourceOperandSet {
    DestinationAndSource(SourceOperand, SourceOperand),
    Destination(SourceOperand),
    None,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct SourceInstruction {
    operation: Operation,
    operand_set: SourceOperandSet,
}

impl SourceInstruction {
    pub(crate) fn new(operation: Operation, operand_set: SourceOperandSet) -> Self {
        Self {
            operation,
            operand_set,
        }
    }
}
