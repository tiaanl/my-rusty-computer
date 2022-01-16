//! Grammar:
//!
//!   ::=   defined as
//!   <>    defined grammar
//!   []    optional
//!   {}    repeats zero or more times
//!   |     if left is not valid, use right
//!   CAP   type of token
//!
//! <program>       := <instruction> {, <instruction>}
//! <instruction>   ::= <operation>, <operand_set>
//! <operand_set>   ::= {<operand>, {<operand>}}
//! <operand>       ::= <immediate> | <register> | <segment> | <direct> | <indirect>
//! <immediate>     ::= NUM
//! <register>      ::= al, bl, cl, dl, ah, bh, ch, dh, ax, bx, cx, dx, bp, sp, si, di
//! <segment>       ::= es, cs, ss, ds
//! <direct>        ::= IDENT | NUM
//! <indirect>      ::= "[" <addressing-mode> "]"

use mrc_instruction::{AddressingMode, OperandSize, Operation, Segment, SizedRegister};
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq)]
pub enum ValueOrLabel {
    Value(i32),
    Label(String),
}

impl std::fmt::Display for ValueOrLabel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueOrLabel::Value(value) => write!(f, "{:#6x}", value),
            ValueOrLabel::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Indirect(AddressingMode, Option<OperandSize>, Option<Segment>),
    Direct(ValueOrLabel, Option<OperandSize>, Option<Segment>),
    Register(SizedRegister),
    Segment(Segment),
    Immediate(ValueOrLabel),
}

impl std::fmt::Display for Operand {
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
pub enum OperandSet {
    DestinationAndSource(Operand, Operand),
    Destination(Operand),
    None,
}

impl std::fmt::Display for OperandSet {
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
pub struct Instruction {
    operation: Operation,
    operand_set: OperandSet,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.operation, self.operand_set)
    }
}

impl Instruction {
    pub fn new(operation: Operation, operand_set: OperandSet) -> Self {
        Self {
            operation,
            operand_set,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Line {
    Label(String),
    Instruction(Instruction),
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "    {}", instruction),
        }
    }
}
