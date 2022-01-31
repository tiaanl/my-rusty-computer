use crate::Operation;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OperandSize {
    Byte,
    Word,
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SizedRegister(pub Register, pub OperandSize);

impl std::fmt::Display for SizedRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Register::*;

        match self.1 {
            OperandSize::Byte => match self.0 {
                AlAx => write!(f, "al"),
                ClCx => write!(f, "cl"),
                DlDx => write!(f, "dl"),
                BlBx => write!(f, "bl"),
                AhSp => write!(f, "ah"),
                ChBp => write!(f, "ch"),
                DhSi => write!(f, "dh"),
                BhDi => write!(f, "bh"),
            },

            OperandSize::Word => match self.0 {
                AlAx => write!(f, "ax"),
                ClCx => write!(f, "cx"),
                DlDx => write!(f, "dx"),
                BlBx => write!(f, "bx"),
                AhSp => write!(f, "sp"),
                ChBp => write!(f, "bp"),
                DhSi => write!(f, "si"),
                BhDi => write!(f, "di"),
            },
        }
    }
}

impl FromStr for SizedRegister {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use OperandSize::*;
        use Register::*;

        match s.to_lowercase().as_str() {
            "al" => Ok(Self(AlAx, Byte)),
            "cl" => Ok(Self(ClCx, Byte)),
            "dl" => Ok(Self(DlDx, Byte)),
            "bl" => Ok(Self(BlBx, Byte)),
            "ah" => Ok(Self(AhSp, Byte)),
            "ch" => Ok(Self(ChBp, Byte)),
            "dh" => Ok(Self(DhSi, Byte)),
            "bh" => Ok(Self(BhDi, Byte)),

            "ax" => Ok(Self(AlAx, Word)),
            "cx" => Ok(Self(ClCx, Word)),
            "dx" => Ok(Self(DlDx, Word)),
            "bx" => Ok(Self(BlBx, Word)),
            "sp" => Ok(Self(AhSp, Word)),
            "bp" => Ok(Self(ChBp, Word)),
            "si" => Ok(Self(DhSi, Word)),
            "di" => Ok(Self(BhDi, Word)),

            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Segment {
    ES,
    CS,
    SS,
    DS,
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Segment::*;

        match self {
            ES => write!(f, "es"),
            CS => write!(f, "cs"),
            SS => write!(f, "ss"),
            DS => write!(f, "ds"),
        }
    }
}

impl FromStr for Segment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "es" => Ok(Segment::ES),
            "cs" => Ok(Segment::CS),
            "ss" => Ok(Segment::SS),
            "ds" => Ok(Segment::DS),
            _ => Err(s.to_string()),
        }
    }
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
pub enum OperandType {
    Direct(Segment, u16),
    Indirect(Segment, AddressingMode, Displacement),
    Register(Register),
    Segment(Segment),
    Immediate(u16),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Operand(pub OperandType, pub OperandSize);

impl From<SizedRegister> for Operand {
    fn from(sized_register: SizedRegister) -> Self {
        Self(OperandType::Register(sized_register.0), sized_register.1)
    }
}

impl From<Segment> for Operand {
    fn from(segment: Segment) -> Self {
        Self(OperandType::Segment(segment), OperandSize::Word)
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
            OperandType::Direct(segment, displacement) => {
                match self.1 {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                print_segment_prefix!(segment);
                write!(f, "[{:#06x}]", displacement)?;
            }
            OperandType::Indirect(segment, encoding, displacement) => {
                match self.1 {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                print_segment_prefix!(segment);
                write!(f, "[{}{}]", encoding, displacement)?;
            }
            OperandType::Register(encoding) => write!(f, "{}", SizedRegister(*encoding, self.1))?,
            OperandType::Segment(encoding) => write!(f, "{}", encoding)?,
            OperandType::Immediate(value) => match &self.1 {
                OperandSize::Byte => write!(f, "{:#04X}", value)?,
                OperandSize::Word => write!(f, "{:#06X}", value)?,
            },
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
///         Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
///         Operand(
///             OperandType::Indirect(Segment::ES, AddressingMode::BxSi, Displacement::Byte(8)),
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
        write!(f, "{:<10} {}", self.operation, &self.operands)
    }
}
