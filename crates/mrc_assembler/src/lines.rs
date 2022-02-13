#![allow(dead_code)]
// TODO: Needs updating
//! ```text
//! Grammar:
//!
//!   ::=   defined as
//!   <>    defined grammar
//!   []    optional
//!   {}    repeats zero or more times
//!   |     if left is not valid, use right
//!   CAP   type of token
//!
//! <program>       ::= <instruction> {, <instruction>}
//! <instruction>   ::= <operation>, {<operand_set>}
//! <operand_set>   ::= <operand>, {<operand>}
//! <operand>       ::= <immediate> | <register> | <segment> | <direct> | <indirect>
//! <immediate>     ::= NUM
//! <register>      ::= al, bl, cl, dl, ah, bh, ch, dh, ax, bx, cx, dx, bp, sp, si, di
//! <segment>       ::= es, cs, ss, ds
//! <direct>        ::= "[" IDENT | NUM "]"
//! <indirect>      ::= "[" <addressing-mode> "]"
//! ```

use mrc_instruction::{
    data, AddressingMode, OperandSize, Operation, Register, Segment, SizedRegister,
};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, PartialEq)]
pub enum ValueOrLabel<'s> {
    Value(i32),
    Label(&'s str),
}

impl<'s> Display for ValueOrLabel<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueOrLabel::Value(value) => write!(f, "{:#6x}", value),
            ValueOrLabel::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand<'s> {
    Indirect(AddressingMode, Option<OperandSize>, Option<Segment>),
    Direct(ValueOrLabel<'s>, Option<OperandSize>, Option<Segment>),
    Register(SizedRegister),
    Segment(Segment),
    Immediate(ValueOrLabel<'s>),
}

impl<'s> Display for Operand<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Indirect(addressing_mode, operand_size, segment_override) => {
                if let Some(operand_size) = operand_size {
                    match operand_size {
                        OperandSize::Byte => write!(f, "byte ")?,
                        OperandSize::Word => write!(f, "word ")?,
                    }
                }

                if let Some(segment_override) = segment_override {
                    write!(f, "{}:", segment_override)?;
                }

                write!(f, "[{}]", addressing_mode)
            }
            Operand::Direct(value, operand_size, segment_override) => {
                if let Some(operand_size) = operand_size {
                    match operand_size {
                        OperandSize::Byte => write!(f, "byte ")?,
                        OperandSize::Word => write!(f, "word ")?,
                    }
                }

                if let Some(segment_override) = segment_override {
                    write!(f, "{}:", segment_override)?;
                }

                write!(f, "[{}]", value)
            }
            Operand::Register(sized_register) => write!(f, "{}", sized_register),
            Operand::Segment(segment) => write!(f, "{}", segment),
            Operand::Immediate(immediate) => write!(f, "{}", immediate),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum OperandSet<'s> {
    DestinationAndSource(Operand<'s>, Operand<'s>),
    Destination(Operand<'s>),
    None,
}

impl<'s> Display for OperandSet<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandSet::DestinationAndSource(destination, source) => {
                write!(f, "{}, {}", destination, source)
            }
            OperandSet::Destination(destination) => write!(f, "{}", destination),
            OperandSet::None => Ok(()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction<'s> {
    operation: Operation,
    operand_set: OperandSet<'s>,
}

impl<'s> Display for Instruction<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.operation, self.operand_set)
    }
}

impl PartialEq<data::InstructionData> for Instruction<'_> {
    fn eq(&self, other: &data::InstructionData) -> bool {
        if other.operation != self.operation {
            return false;
        }

        if matches!(self.operand_set, OperandSet::None)
            && other.destination == data::OperandType::None
            && other.source == data::OperandType::None
        {
            return true;
        }

        match &self.operand_set {
            OperandSet::DestinationAndSource(destination, _source) => {
                use data::OperandType::*;

                match other.destination {
                    None => false,
                    OpCodeReg => false,
                    AL => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte))
                    ),
                    CL => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::ClCx, OperandSize::Byte))
                    ),
                    DL => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::DlDx, OperandSize::Byte))
                    ),
                    AX => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::AlAx, OperandSize::Word))
                    ),
                    CX => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::ClCx, OperandSize::Word))
                    ),
                    DX => matches!(
                        destination,
                        Operand::Register(SizedRegister(Register::DlDx, OperandSize::Word))
                    ),
                    Reg8 => matches!(
                        destination,
                        Operand::Register(SizedRegister(_, OperandSize::Byte))
                    ),
                    Reg16 => matches!(
                        destination,
                        Operand::Register(SizedRegister(_, OperandSize::Word))
                    ),
                    Mem => false,
                    MemFar => false,
                    Imm8 => false,
                    Imm16 => false,
                    Imm1 => false,
                    SignedImm8 => false,
                    SignedImm16 => false,
                    SegReg => false,
                    Displacement8 => false,
                    Displacement16 => false,
                }
            }
            _ => false,
        }
    }
}

impl<'s> Instruction<'s> {
    pub fn new(operation: Operation, operand_set: OperandSet<'s>) -> Self {
        Self {
            operation,
            operand_set,
        }
    }

    pub fn data(&self) -> &'static data::InstructionData {
        if let Some(data) = data::INSTRUCTION_DATA.iter().find(|d| self == *d) {
            data
        } else {
            unreachable!()
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Const<'s> {
    pub name: &'s str,
    // TODO: This should have more variants.
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub enum Line<'s> {
    Label(&'s str),
    Instruction(Instruction<'s>),
    Const(Const<'s>),
    Comment(&'s str),
}

impl<'s> Display for Line<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "    {}", instruction),
            Line::Const(c) => write!(f, "{} equ {}", c.name, c.value),
            Line::Comment(comment) => write!(f, "{}", comment),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_with_no_operands() {
        let ins = Instruction::new(Operation::DAA, OperandSet::None);
        let data = ins.data();
        assert_eq!(data::OperationMap::Single(Operation::DAA), data.operation);
        assert_eq!(data::OperandType::None, data.destination);
        assert_eq!(data::OperandType::None, data.source);
    }

    #[test]
    fn test_destination_and_source_register_immediate() {
        let ins = Instruction::new(
            Operation::ADD,
            OperandSet::DestinationAndSource(
                Operand::Register(SizedRegister(Register::AlAx, OperandSize::Byte)),
                Operand::Immediate(ValueOrLabel::Value(0x10)),
            ),
        );

        let data = ins.data();
        assert_eq!(data::OperationMap::Single(Operation::ADD), data.operation);
        assert_eq!(data::OperandType::AL, data.destination);
        assert_eq!(data::OperandType::Imm8, data.source);
    }
}
