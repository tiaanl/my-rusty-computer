use crate::{
    Address, Displacement, Instruction, Operand, OperandSet, OperandSize, Repeat, Segment,
    SizedRegisterEncoding,
};
use std::fmt::{Display, Formatter};

pub struct At<'a, I> {
    pub item: &'a I,
    pub addr: Option<Address>,
}

impl Display for At<'_, Instruction> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.item.repeat {
            None => match self.item.operands {
                OperandSet::None => write!(f, "{:<10}", self.item.operation),
                _ => write!(
                    f,
                    "{:<10} {}",
                    self.item.operation,
                    At {
                        item: &self.item.operands,
                        addr: self.addr,
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

impl Display for At<'_, OperandSet> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.item {
            OperandSet::None => Ok(()),
            OperandSet::Destination(destination) => At {
                item: destination,
                addr: self.addr,
            }
            .fmt(f),
            OperandSet::DestinationAndSource(destination, source) => {
                write!(
                    f,
                    "{}, {}",
                    At {
                        item: destination,
                        addr: self.addr,
                    },
                    At {
                        item: source,
                        addr: self.addr,
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

impl Display for At<'_, Operand> {
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

            Operand::Displacement(displacement) => At {
                item: displacement,
                addr: self.addr,
            }
            .fmt(f),
        }
    }
}

impl Display for At<'_, Displacement> {
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
