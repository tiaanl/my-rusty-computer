use crate::display::DisAsmOptions;
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
        DisAsmOptions {
            item: self,
            addr: None,
            segment_override: None,
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
    pub size_in_bytes: u8,
}

impl Instruction {
    /// Create a new instruction with the given [Operation] and [OperandSet].
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: None,
            size_in_bytes: 0,
        }
    }

    /// Create a new instruction with the given [Operation] and [OperandSet], as well as a [Repeat]
    /// flag.
    pub fn with_repeat(repeat: Repeat, operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: Some(repeat),
            size_in_bytes: 0,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        DisAsmOptions {
            item: self,
            addr: None,
            segment_override: None,
        }
        .fmt(f)
    }
}
