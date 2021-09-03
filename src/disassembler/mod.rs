use crate::instructions::*;
use std::fmt;

trait Keyword {
    fn keyword(&self) -> &'static str;
}

impl Keyword for Operation {
    fn keyword(&self) -> &'static str {
        match self {
            Operation::Mov => "mov",
            Operation::Push => "push",
            Operation::Pop => "pop",
            Operation::Xchg => "xchg",
            Operation::In => "in",
            Operation::Out => "out",
            Operation::Xlat => "xlat",
            Operation::Lea => "lea",
            Operation::Lds => "lds",
            Operation::Les => "les",
            Operation::Lahf => "lahf",
            Operation::Sahf => "sahf",
            Operation::Pushf => "pushf",
            Operation::Popf => "popf",
            Operation::Add => "add",
            Operation::Adc => "adc",
            Operation::Inc => "inc",
            Operation::Aaa => "aaa",
            Operation::Baa => "baa",
            Operation::Sub => "sub",
            Operation::Ssb => "ssb",
            Operation::Dec => "dec",
            Operation::Neg => "neg",
            Operation::Cmp => "cmp",
            Operation::Aas => "aas",
            Operation::Das => "das",
            Operation::Mul => "mul",
            Operation::Imul => "imul",
            Operation::Aam => "aam",
            Operation::Div => "div",
            Operation::Idiv => "idiv",
            Operation::Aad => "aad",
            Operation::Cbw => "cbw",
            Operation::Cwd => "cwd",
            Operation::Not => "not",
            Operation::Shl => "shl",
            Operation::Shr => "shr",
            Operation::Sar => "sar",
            Operation::Rol => "rol",
            Operation::Ror => "ror",
            Operation::Rcl => "rcl",
            Operation::Rcr => "rcr",
            Operation::And => "and",
            Operation::Test => "test",
            Operation::Or => "or",
            Operation::Xor => "xor",
            Operation::Rep => "rep",
            Operation::Movs => "movs",
            Operation::Cmps => "cmps",
            Operation::Scas => "scas",
            Operation::Lods => "lods",
            Operation::Stos => "stos",
            Operation::Call => "call",
            Operation::Jmp => "jmp",
            Operation::Ret => "ret",
            Operation::Je => "je",
            Operation::Jl => "jl",
            Operation::Jle => "jle",
            Operation::Jb => "jb",
            Operation::Jbe => "jbe",
            Operation::Jp => "jp",
            Operation::Jo => "jo",
            Operation::Js => "js",
            Operation::Jne => "jne",
            Operation::Jnl => "jnl",
            Operation::Jnle => "jnle",
            Operation::Jnb => "jnb",
            Operation::Jnbe => "jnbe",
            Operation::Jnp => "jnp",
            Operation::Jno => "jno",
            Operation::Jns => "jns",
            Operation::Loop => "loop",
            Operation::Loopz => "loopz",
            Operation::Loopnz => "loopnz",
            Operation::Jcxz => "jcxz",
            Operation::Int => "int",
            Operation::Into => "into",
            Operation::IRet => "iret",
            Operation::Clc => "clc",
            Operation::Cmc => "cmc",
            Operation::Stc => "stc",
            Operation::Cld => "cld",
            Operation::Std => "std",
            Operation::Cli => "cli",
            Operation::Sti => "sti",
            Operation::Hlt => "hlt",
            Operation::Wait => "wait",
            Operation::Esc => "esc",
            Operation::Lock => "lock",
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

impl Keyword for Segment {
    fn keyword(&self) -> &'static str {
        use Segment::*;

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
