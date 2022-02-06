use crate::{Register, Segment, SizedRegister};
use std::fmt::{Display, Formatter};
use std::str::FromStr;

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

impl Display for AddressingMode {
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

impl Display for Displacement {
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

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Immediate::Byte(value) => write!(f, "{:#04X}", value),
            Immediate::Word(value) => write!(f, "{:#06X}", value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operand {
    Direct(Segment, u16, OperandSize),
    Indirect(Segment, AddressingMode, Displacement, OperandSize),
    Register(Register, OperandSize),
    Segment(Segment),
    Immediate(Immediate),
}

impl Operand {
    pub fn operand_size(&self) -> OperandSize {
        match self {
            Operand::Direct(_, _, operand_size)
            | Operand::Indirect(_, _, _, operand_size)
            | Operand::Register(_, operand_size) => *operand_size,
            Operand::Segment(_) => OperandSize::Word,
            Operand::Immediate(immediate) => match immediate {
                Immediate::Byte(_) => OperandSize::Byte,
                Immediate::Word(_) => OperandSize::Word,
            },
        }
    }
}

#[inline]
fn write_segment_prefix(f: &mut std::fmt::Formatter<'_>, segment: Segment) -> std::fmt::Result {
    match segment {
        Segment::ES => write!(f, "es:"),
        Segment::CS => write!(f, "cs:"),
        Segment::SS => write!(f, "ss:"),
        Segment::DS => Ok(()),
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Direct(segment, displacement, operand_size) => {
                match operand_size {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                write_segment_prefix(f, *segment)?;
                write!(f, "[{:#06x}]", displacement)?;
            }
            Operand::Indirect(segment, encoding, displacement, operand_size) => {
                match operand_size {
                    OperandSize::Byte => write!(f, "byte ")?,
                    OperandSize::Word => write!(f, "word ")?,
                }
                write_segment_prefix(f, *segment)?;
                write!(f, "[{}{}]", encoding, displacement)?;
            }
            Operand::Register(register, operand_size) => {
                write!(f, "{}", SizedRegister(*register, *operand_size))?
            }
            Operand::Segment(encoding) => write!(f, "{}", encoding)?,
            Operand::Immediate(value) => write!(f, "{}", value)?,
        }
        Ok(())
    }
}
