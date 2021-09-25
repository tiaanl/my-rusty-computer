use crate::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Operation, Register, Segment,
};
use std::fmt;
use std::fmt::Formatter;

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operation::Aaa => write!(f, "aaa"),
            Operation::Aad => write!(f, "aad"),
            Operation::Aam => write!(f, "aam"),
            Operation::Aas => write!(f, "aas"),
            Operation::Adc => write!(f, "adc"),
            Operation::Add => write!(f, "add"),
            Operation::And => write!(f, "and"),
            Operation::Baa => write!(f, "baa"),
            Operation::Call => write!(f, "call"),
            Operation::Cbw => write!(f, "cbw"),
            Operation::Clc => write!(f, "clc"),
            Operation::Cld => write!(f, "cld"),
            Operation::Cli => write!(f, "cli"),
            Operation::Cmc => write!(f, "cmc"),
            Operation::Cmp => write!(f, "cmp"),
            Operation::Cmpsb => write!(f, "cmpsb"),
            Operation::Cmpsw => write!(f, "cmpsw"),
            Operation::Cwd => write!(f, "cwd"),
            Operation::Daa => write!(f, "daa"),
            Operation::Das => write!(f, "das"),
            Operation::Dec => write!(f, "dec"),
            Operation::Div => write!(f, "div"),
            Operation::Esc => write!(f, "esc"),
            Operation::Hlt => write!(f, "hlt"),
            Operation::IRet => write!(f, "iret"),
            Operation::Idiv => write!(f, "idiv"),
            Operation::Imul => write!(f, "imul"),
            Operation::In => write!(f, "in"),
            Operation::Inc => write!(f, "inc"),
            Operation::Int => write!(f, "int"),
            Operation::Into => write!(f, "into"),
            Operation::Jb => write!(f, "jb"),
            Operation::Jbe => write!(f, "jbe"),
            Operation::Jcxz => write!(f, "jcxz"),
            Operation::Je => write!(f, "je"),
            Operation::Jl => write!(f, "jl"),
            Operation::Jle => write!(f, "jle"),
            Operation::Jmp => write!(f, "jmp"),
            Operation::Jnb => write!(f, "jnb"),
            Operation::Jnbe => write!(f, "jnbe"),
            Operation::Jne => write!(f, "jne"),
            Operation::Jnl => write!(f, "jnl"),
            Operation::Jnle => write!(f, "jnle"),
            Operation::Jno => write!(f, "jno"),
            Operation::Jnp => write!(f, "jnp"),
            Operation::Jns => write!(f, "jns"),
            Operation::Jo => write!(f, "jo"),
            Operation::Jp => write!(f, "jp"),
            Operation::Js => write!(f, "js"),
            Operation::Lahf => write!(f, "lahf"),
            Operation::Lds => write!(f, "lds"),
            Operation::Lea => write!(f, "lea"),
            Operation::Les => write!(f, "les"),
            Operation::Lock => write!(f, "lock"),
            Operation::Lodsb => write!(f, "lodsb"),
            Operation::Lodsw => write!(f, "lodsw"),
            Operation::Loop => write!(f, "loop"),
            Operation::Loopnz => write!(f, "loopnz"),
            Operation::Loopz => write!(f, "loopz"),
            Operation::Mov => write!(f, "mov"),
            Operation::Movsb => write!(f, "movsb"),
            Operation::Movsw => write!(f, "movsw"),
            Operation::Mul => write!(f, "mul"),
            Operation::Neg => write!(f, "neg"),
            Operation::Nop => write!(f, "nop"),
            Operation::Not => write!(f, "not"),
            Operation::Or => write!(f, "or"),
            Operation::Out => write!(f, "out"),
            Operation::Pop => write!(f, "pop"),
            Operation::Popf => write!(f, "popf"),
            Operation::Push => write!(f, "push"),
            Operation::Pushf => write!(f, "pushf"),
            Operation::Rcl => write!(f, "rcl"),
            Operation::Rcr => write!(f, "rcr"),
            Operation::Ret => write!(f, "ret"),
            Operation::Retf => write!(f, "retf"),
            Operation::Rol => write!(f, "rol"),
            Operation::Ror => write!(f, "ror"),
            Operation::Sahf => write!(f, "sahf"),
            Operation::Sar => write!(f, "sar"),
            Operation::Sbb => write!(f, "sbb"),
            Operation::Scasb => write!(f, "scasb"),
            Operation::Scasw => write!(f, "scasw"),
            Operation::Shl => write!(f, "shl"),
            Operation::Shr => write!(f, "shr"),
            Operation::Stc => write!(f, "stc"),
            Operation::Std => write!(f, "std"),
            Operation::Sti => write!(f, "sti"),
            Operation::Stosb => write!(f, "stosb"),
            Operation::Stosw => write!(f, "stosw"),
            Operation::Sub => write!(f, "sub"),
            Operation::Test => write!(f, "test"),
            Operation::Wait => write!(f, "wait"),
            Operation::Xchg => write!(f, "xchg"),
            Operation::Xlat => write!(f, "xlat"),
            Operation::Xor => write!(f, "xor"),
        }
    }
}

struct RegisterDisplay<'a> {
    register: &'a Register,
    operand_size: &'a OperandSize,
}

impl<'a> RegisterDisplay<'a> {
    fn new(register: &'a Register, operand_size: &'a OperandSize) -> Self {
        Self {
            register,
            operand_size,
        }
    }
}

impl fmt::Display for RegisterDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Register::*;

        match self.operand_size {
            OperandSize::Byte => match self.register {
                AlAx => write!(f, "al"),
                ClCx => write!(f, "cl"),
                DlDx => write!(f, "dl"),
                BlBx => write!(f, "bl"),
                AhSp => write!(f, "ah"),
                ChBp => write!(f, "ch"),
                DhSi => write!(f, "dh"),
                BhDi => write!(f, "bh"),
            },
            OperandSize::Word => match self.register {
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

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Segment::*;

        match self {
            Es => write!(f, "es"),
            Cs => write!(f, "cs"),
            Ss => write!(f, "ss"),
            Ds => write!(f, "ds"),
        }
    }
}

impl fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for Displacement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.0 {
            OperandType::Direct(displacement) => {
                write!(f, "[{:#06x}]", displacement)?;
            }
            OperandType::Indirect(encoding, displacement) => {
                write!(f, "[{}{}]", encoding, displacement)?;
            }
            OperandType::Register(encoding) => {
                write!(f, "{}", RegisterDisplay::new(encoding, &self.1))?
            }
            OperandType::Segment(encoding) => write!(f, "{}", encoding)?,
            OperandType::Immediate(value) => match &self.1 {
                OperandSize::Byte => write!(f, "{:#04X}", value)?,
                OperandSize::Word => write!(f, "{:#06X}", value)?,
            },
        }
        Ok(())
    }
}

impl fmt::Display for OperandSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OperandSet::None => {}
            OperandSet::Destination(destination) => write!(f, "{}", destination)?,
            OperandSet::DestinationAndSource(destination, source) => {
                write!(f, "{}, {}", destination, source)?;
            }
            OperandSet::SegmentAndOffset(segment, offset) => {
                write!(f, "{:#06X}:{:#06X}", segment, offset)?
            }
            OperandSet::Displacement(displacement) => write!(f, "{}", displacement)?,
        }
        Ok(())
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:<10} {}", self.operation, &self.operands)?;
        Ok(())
    }
}
