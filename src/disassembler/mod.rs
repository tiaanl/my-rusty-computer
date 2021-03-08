use std::fmt;

use crate::instruction::{
    DataSize, IndirectMemoryEncoding, Instruction, Operand, Operation, RegisterEncoding,
};

trait Keyword {
    fn keyword(&self) -> &'static str;
}

impl Keyword for Operation {
    fn keyword(&self) -> &'static str {
        match self {
            Operation::Add => "add",
            // _ => "unknown",
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

impl Keyword for IndirectMemoryEncoding {
    fn keyword(&self) -> &'static str {
        match self {
            IndirectMemoryEncoding::BxSi => "bx+si",
            IndirectMemoryEncoding::BxDi => "bx+di",
            IndirectMemoryEncoding::BpSi => "bp+si",
            IndirectMemoryEncoding::BpDi => "bp+di",
            IndirectMemoryEncoding::Si => "si",
            IndirectMemoryEncoding::Di => "di",
            IndirectMemoryEncoding::Bp => "bp",
            IndirectMemoryEncoding::Bx => "bx",
        }
    }
}

fn fmt_operand(f: &mut fmt::Formatter<'_>, operand: &Operand, data_size: &DataSize) -> fmt::Result {
    match operand {
        Operand::Indirect(encoding, displacement) => {
            write!(f, "[{}", encoding.keyword())?;
            if *displacement > 0 {
                write!(f, "{}]", displacement)?;
            } else {
                write!(f, "]")?;
            }
        }
        Operand::Register(encoding) => write!(f, "{}", (encoding, data_size).keyword())?,
        Operand::Immediate(value) => match data_size {
            DataSize::Byte => write!(f, "0x{:#2X}", value)?,
            DataSize::Word => write!(f, "0x{:#4X}", value)?,
        },
        _ => write!(f, "unknown operand")?,
    }
    Ok(())
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:<10}", self.operation.keyword())?;
        fmt_operand(f, &self.destination, &self.data_size)?;
        write!(f, ", ")?;
        fmt_operand(f, &self.source, &self.data_size)?;

        Ok(())
    }
}
