use crate::display::At;
use crate::{Operand, Operation};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSet {
    None,
    Destination(Operand),
    DestinationAndSource(Operand, Operand),
}

impl Display for OperandSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        At {
            item: self,
            addr: None,
        }
        .fmt(f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Repeat {
    Equal,
    NotEqual,
}

/// Representation of a 8086 (and friends) instruction.
///
/// ```rust
/// use mrc_instruction::*;
///
/// // mov ax, [es:bx+si+8]
/// let i = Instruction::new(
///     Operation::MOV,
///     OperandSet::DestinationAndSource(
///         Operand::Register(
///             SizedRegisterEncoding(
///                 RegisterEncoding::AlAx,
///                 OperandSize::Word,
///             )
///         ),
///         Operand::Indirect(
///             Segment::ES,
///             AddressingMode::BxSi,
///             Displacement::Byte(8),
///             OperandSize::Word,
///         ),
///     ),
/// );
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub operation: Operation,
    pub operands: OperandSet,
    pub repeat: Option<Repeat>,
}

impl Instruction {
    /// Create a new instruction with the given [Operation] and [OperandSet].
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: None,
        }
    }

    /// Create a new instruction with the given [Operation] and [OperandSet], as well as a [Repeat]
    /// flag.
    pub fn with_repeat(repeat: Repeat, operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: Some(repeat),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        At {
            item: self,
            addr: None,
        }
        .fmt(f)
    }
}
