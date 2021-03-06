use crate::instructions::*;
use std::fmt;

trait Keyword {
    fn keyword(&self) -> &'static str;
}

impl Keyword for Operation {
    fn keyword(&self) -> &'static str {
        match self {
            Operation::Add => "add",
            Operation::Cli => "cli",
            Operation::Jmp => "jmp",
            Operation::Mov => "mov",
        }
    }
}

impl Keyword for (&RegisterEncoding, &DataSize) {
    fn keyword(&self) -> &'static str {
        match self.1 {
            DataSize::Byte => match self.0 {
                RegisterEncoding::AlAx => "al",
                RegisterEncoding::ClCx => "cl",
                RegisterEncoding::DlDx => "dl",
                RegisterEncoding::BlBx => "bl",
                RegisterEncoding::AhSp => "ah",
                RegisterEncoding::ChBp => "ch",
                RegisterEncoding::DhSi => "dh",
                RegisterEncoding::BhDi => "bh",
            },
            DataSize::Word => match self.0 {
                RegisterEncoding::AlAx => "ax",
                RegisterEncoding::ClCx => "cx",
                RegisterEncoding::DlDx => "dx",
                RegisterEncoding::BlBx => "bx",
                RegisterEncoding::AhSp => "sp",
                RegisterEncoding::ChBp => "bp",
                RegisterEncoding::DhSi => "si",
                RegisterEncoding::BhDi => "di",
            },
        }
    }
}

impl Keyword for SegmentEncoding {
    fn keyword(&self) -> &'static str {
        use SegmentEncoding::*;

        match self {
            Es => "es",
            Cs => "cs",
            Ss => "ss",
            Ds => "ds",
        }
    }
}

impl Keyword for AddressingMode {
    fn keyword(&self) -> &'static str {
        match self {
            AddressingMode::BxSi => "bx+si",
            AddressingMode::BxDi => "bx+di",
            AddressingMode::BpSi => "bp+si",
            AddressingMode::BpDi => "bp+di",
            AddressingMode::Si => "si",
            AddressingMode::Di => "di",
            AddressingMode::Bp => "bp",
            AddressingMode::Bx => "bx",
        }
    }
}

fn fmt_operand(f: &mut fmt::Formatter<'_>, operand: &Operand, data_size: &DataSize) -> fmt::Result {
    match operand {
        Operand::Direct(displacement) => {
            write!(f, "[{:#06x}]", displacement)?;
        }
        Operand::Indirect(encoding, displacement) => {
            write!(f, "[{}", encoding.keyword())?;
            if *displacement > 0 {
                write!(f, "{}]", displacement)?;
            } else {
                write!(f, "]")?;
            }
        }
        Operand::Register(encoding) => write!(f, "{}", (encoding, data_size).keyword())?,
        Operand::Segment(encoding) => write!(f, "{}", encoding.keyword())?,
        Operand::Immediate(value) => match data_size {
            DataSize::Byte => write!(f, "{:#04X}", value)?,
            DataSize::Word => write!(f, "{:#06X}", value)?,
        },
        // _ => write!(f, "unknown operand")?,
    }
    Ok(())
}

fn fmt_operand_set(f: &mut fmt::Formatter<'_>, operand_set: &OperandSet) -> fmt::Result {
    match operand_set {
        OperandSet::None => {}
        OperandSet::SegmentAndOffset(segment, offset) => {
            write!(f, "{:#06X}:{:#06X}", segment, offset)?
        }
        OperandSet::DestinationAndSource(destination, source, data_size) => {
            fmt_operand(f, destination, data_size)?;
            write!(f, ", ")?;
            fmt_operand(f, source, data_size)?;
        }
    }
    Ok(())
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:<10}", self.operation.keyword())?;
        fmt_operand_set(f, &self.operands)?;

        Ok(())
    }
}
