use crate::{Operation, Register, Segment, SizedRegister};
use std::fmt::Formatter;
use std::str::FromStr;

pub trait SizedData {
    fn size() -> Option<OperandSize>;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSize {
    Byte,
    Word,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

impl std::fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AddressingMode::*;

        match self {
            BxSi => write!(f, "bx+si"),
            BxDi => write!(f, "bx+di"),
            BpSi => write!(f, "bp+si"),
            BpDi => write!(f, "bp+di"),
            Si => write!(f, "si"),
            Di => write!(f, "di"),
            Bp => write!(f, "bp"),
            Bx => write!(f, "bx"),
        }
    }
}

impl FromStr for AddressingMode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use AddressingMode::*;

        match s.to_lowercase().replace(' ', "").as_str() {
            "bx+si" => Ok(BxSi),
            "bx+di" => Ok(BxDi),
            "bp+si" => Ok(BpSi),
            "bp+di" => Ok(BpDi),
            "si" => Ok(Si),
            "di" => Ok(Di),
            "bp" => Ok(Bp),
            "bx" => Ok(Bx),
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Displacement {
    None,
    Byte(i8),
    Word(i16),
}

impl std::fmt::Display for Displacement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Displacement::None => Ok(()),
            Displacement::Byte(offset) => {
                write!(f, "{:+}", offset)
            }
            Displacement::Word(offset) => {
                write!(f, "{:+}", offset)
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Immediate {
    Byte(u8),
    Word(u16),
}

impl std::fmt::Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Immediate::Byte(value) => write!(f, "{:#04X}", value),
            Immediate::Word(value) => write!(f, "{:#06X}", value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandKind {
    Direct(Segment, u16),
    Indirect(Segment, AddressingMode, Displacement),
    Register(Register),
    Segment(Segment),
    Immediate(Immediate),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operand(pub OperandKind, pub OperandSize);

impl From<SizedRegister> for Operand {
    fn from(sized_register: SizedRegister) -> Self {
        Self(OperandKind::Register(sized_register.0), sized_register.1)
    }
}

impl From<Segment> for Operand {
    fn from(segment: Segment) -> Self {
        Self(OperandKind::Segment(segment), OperandSize::Word)
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! print_segment_prefix {
            ($segment:expr) => {
                match $segment {
                    Segment::ES => write!(f, "es:")?,
                    Segment::CS => write!(f, "cs:")?,
                    Segment::SS => write!(f, "ss:")?,
                    Segment::DS => {}
                }
            };
        }

        match &self.0 {
            OperandKind::Direct(segment, displacement) => {
                match self.1 {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                print_segment_prefix!(segment);
                write!(f, "[{:#06x}]", displacement)?;
            }
            OperandKind::Indirect(segment, encoding, displacement) => {
                match self.1 {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                print_segment_prefix!(segment);
                write!(f, "[{}{}]", encoding, displacement)?;
            }
            OperandKind::Register(encoding) => write!(f, "{}", SizedRegister(*encoding, self.1))?,
            OperandKind::Segment(encoding) => write!(f, "{}", encoding)?,
            OperandKind::Immediate(value) => write!(f, "{}", value)?,
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSet {
    None,
    Destination(Operand),
    DestinationAndSource(Operand, Operand),
    Displacement(Displacement),
    SegmentAndOffset(u16, u16),
}

impl std::fmt::Display for OperandSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandSet::None => Ok(()),
            OperandSet::Destination(destination) => write!(f, "{}", destination),
            OperandSet::DestinationAndSource(destination, source) => {
                write!(f, "{}, {}", destination, source)
            }
            OperandSet::SegmentAndOffset(segment, offset) => {
                write!(f, "{:#06X}:{:#06X}", segment, offset)
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
///         Operand(OperandKind::Register(Register::AlAx), OperandSize::Word),
///         Operand(
///             OperandKind::Indirect(Segment::ES, AddressingMode::BxSi, Displacement::Byte(8)),
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

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.operands {
            OperandSet::None => write!(f, "{:<10}", self.operation),
            _ => write!(f, "{:<10} {}", self.operation, &self.operands),
        }
    }
}
