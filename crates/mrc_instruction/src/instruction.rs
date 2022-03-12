use crate::{Address, Displacement, Operand, Operation};
use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSet {
    None,
    Destination(Operand),
    DestinationAndSource(Operand, Operand),
    Displacement(Displacement),
    SegmentAndOffset(Address),
}

impl Display for OperandSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandSet::None => Ok(()),
            OperandSet::Destination(destination) => write!(f, "{}", destination),
            OperandSet::DestinationAndSource(destination, source) => {
                write!(f, "{}, {}", destination, source)
            }
            OperandSet::SegmentAndOffset(address) => {
                write!(f, "{}", address)
            }
            OperandSet::Displacement(displacement) => write!(f, "{}", displacement),
        }
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.repeat {
            None => match self.operands {
                OperandSet::None => write!(f, "{:<10}", self.operation),
                _ => write!(f, "{:<10} {}", self.operation, &self.operands),
            },
            Some(rep) => {
                match rep {
                    Repeat::Equal => write!(f, "rep "),
                    Repeat::NotEqual => write!(f, "repz "),
                }?;
                write!(f, "{}", self.operation)
            }
        }
    }
}
