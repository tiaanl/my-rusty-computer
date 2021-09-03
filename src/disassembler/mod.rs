use crate::instructions::*;
use std::fmt;

trait Keyword {
    fn keyword(&self) -> &'static str;
}

impl Keyword for Operation {
    fn keyword(&self) -> &'static str {
        match self {
            Operation::Add => "add",
            Operation::Call => "call",
            Operation::Cli => "cli",
            Operation::Jmp => "jmp",
            Operation::Mov => "mov",
            Operation::Sti => "sti",
        }
    }
}

impl Keyword for (&Register, &DataSize) {
    fn keyword(&self) -> &'static str {
        match self.1 {
            DataSize::Byte => match self.0 {
                Register::AlAx => "al",
                Register::ClCx => "cl",
                Register::DlDx => "dl",
                Register::BlBx => "bl",
                Register::AhSp => "ah",
                Register::ChBp => "ch",
                Register::DhSi => "dh",
                Register::BhDi => "bh",
            },
            DataSize::Word => match self.0 {
                Register::AlAx => "ax",
                Register::ClCx => "cx",
                Register::DlDx => "dx",
                Register::BlBx => "bx",
                Register::AhSp => "sp",
                Register::ChBp => "bp",
                Register::DhSi => "si",
                Register::BhDi => "di",
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
        OperandSet::DestinationAndSource(destination, source, data_size) => {
            fmt_operand(f, destination, data_size)?;
            write!(f, ", ")?;
            fmt_operand(f, source, data_size)?;
        }
        OperandSet::SegmentAndOffset(segment, offset) => {
            write!(f, "{:#06X}:{:#06X}", segment, offset)?
        }
        OperandSet::Offset(offset) => write!(f, "{:#06X}", offset)?,
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
