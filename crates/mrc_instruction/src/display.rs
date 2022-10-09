use crate::{
    Address, Displacement, Instruction, Operand, OperandSet, OperandSize, Repeat, Segment,
    SizedRegisterEncoding,
};
use std::fmt::{Display, Formatter};

pub struct DisAsmOptions<'a, I> {
    pub item: &'a I,
    pub addr: Option<Address>,
    pub segment_override: Option<Segment>,
}

impl Display for DisAsmOptions<'_, Instruction> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.item.repeat {
            None => match self.item.operands {
                OperandSet::None => write!(f, "{:<10}", self.item.operation),
                _ => write!(
                    f,
                    "{:<10} {}",
                    self.item.operation,
                    DisAsmOptions {
                        item: &self.item.operands,
                        addr: self.addr,
                        segment_override: self.segment_override,
                    }
                ),
            },
            Some(rep) => {
                match rep {
                    Repeat::Equal => write!(f, "rep "),
                    Repeat::NotEqual => write!(f, "repz "),
                }?;
                self.item.operation.fmt(f)
            }
        }
    }
}

impl Display for DisAsmOptions<'_, OperandSet> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.item {
            OperandSet::None => Ok(()),
            OperandSet::Destination(destination) => DisAsmOptions {
                item: destination,
                addr: self.addr,
                segment_override: self.segment_override,
            }
            .fmt(f),
            OperandSet::DestinationAndSource(destination, source) => {
                write!(
                    f,
                    "{}, {}",
                    DisAsmOptions {
                        item: destination,
                        addr: self.addr,
                        segment_override: self.segment_override,
                    },
                    DisAsmOptions {
                        item: source,
                        addr: self.addr,
                        segment_override: self.segment_override,
                    }
                )
            }
        }
    }
}

#[inline]
fn write_segment_prefix(f: &mut Formatter<'_>, segment: Segment) -> std::fmt::Result {
    match segment {
        Segment::ES => write!(f, "es:"),
        Segment::CS => write!(f, "cs:"),
        Segment::SS => write!(f, "ss:"),
        Segment::DS => Ok(()),
    }
}

impl Display for DisAsmOptions<'_, Operand> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.item {
            Operand::Direct(segment, displacement, operand_size) => {
                match operand_size {
                    OperandSize::Byte => "byte ".fmt(f)?,
                    OperandSize::Word => "word ".fmt(f)?,
                }
                "[".fmt(f)?;
                write_segment_prefix(f, *segment)?;
                write!(f, "{:#06x}]", displacement)
            }

            Operand::Indirect(segment, encoding, displacement, operand_size) => {
                match operand_size {
                    OperandSize::Byte => "byte ".fmt(f)?,
                    OperandSize::Word => "word ".fmt(f)?,
                }
                "[".fmt(f)?;
                write_segment_prefix(f, *segment)?;
                write!(f, "{}{}]", encoding, displacement)
            }

            Operand::Register(SizedRegisterEncoding(register, operand_size)) => {
                write!(f, "{}", SizedRegisterEncoding(*register, *operand_size))
            }

            Operand::Segment(encoding) => write!(f, "{}", encoding),

            Operand::Immediate(value) => write!(f, "{}", value),

            Operand::SegmentAndOffset(address) => address.fmt(f),

            Operand::Displacement(displacement) => DisAsmOptions {
                item: displacement,
                addr: self.addr,
                segment_override: self.segment_override,
            }
            .fmt(f),
        }
    }
}

impl Display for DisAsmOptions<'_, Displacement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(addr) = self.addr {
            match self.item {
                Displacement::None => Ok(()),
                Displacement::Byte(offset) => {
                    write!(
                        f,
                        "{:04X}:{:04X}",
                        addr.segment,
                        addr.offset.wrapping_add(*offset as u16)
                    )
                }
                Displacement::Word(offset) => {
                    write!(
                        f,
                        "{:04X}:{:04X}",
                        addr.segment,
                        addr.offset.wrapping_add(*offset as u16)
                    )
                }
            }
        } else {
            match self.item {
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
}
