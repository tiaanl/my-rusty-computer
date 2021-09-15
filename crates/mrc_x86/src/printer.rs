use crate::{
    AddressingMode, Instruction, Operand, OperandSet, OperandSize, OperandType, Operation,
    Register, Segment,
};
use std::fmt;
use std::fmt::Formatter;

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Operation::*;

        match self {
            Mov => write!(f, "mov"),
            Push => write!(f, "push"),
            Pop => write!(f, "pop"),
            Xchg => write!(f, "xchg"),
            In => write!(f, "in"),
            Out => write!(f, "out"),
            Xlat => write!(f, "xlat"),
            Lea => write!(f, "lea"),
            Lds => write!(f, "lds"),
            Les => write!(f, "les"),
            Lahf => write!(f, "lahf"),
            Sahf => write!(f, "sahf"),
            Pushf => write!(f, "pushf"),
            Popf => write!(f, "popf"),
            Add => write!(f, "add"),
            Adc => write!(f, "adc"),
            Inc => write!(f, "inc"),
            Aaa => write!(f, "aaa"),
            Baa => write!(f, "baa"),
            Sub => write!(f, "sub"),
            Sbb => write!(f, "sbb"),
            Dec => write!(f, "dec"),
            Neg => write!(f, "neg"),
            Cmp => write!(f, "cmp"),
            Aas => write!(f, "aas"),
            Das => write!(f, "das"),
            Mul => write!(f, "mul"),
            Imul => write!(f, "imul"),
            Aam => write!(f, "aam"),
            Div => write!(f, "div"),
            Idiv => write!(f, "idiv"),
            Aad => write!(f, "aad"),
            Cbw => write!(f, "cbw"),
            Cwd => write!(f, "cwd"),
            Not => write!(f, "not"),
            Shl => write!(f, "shl"),
            Shr => write!(f, "shr"),
            Sar => write!(f, "sar"),
            Rol => write!(f, "rol"),
            Ror => write!(f, "ror"),
            Rcl => write!(f, "rcl"),
            Rcr => write!(f, "rcr"),
            And => write!(f, "and"),
            Test => write!(f, "test"),
            Or => write!(f, "or"),
            Xor => write!(f, "xor"),
            Rep => write!(f, "rep"),
            Movs => write!(f, "movs"),
            Cmps => write!(f, "cmps"),
            Scas => write!(f, "scas"),
            Lods => write!(f, "lods"),
            Stos => write!(f, "stos"),
            Call => write!(f, "call"),
            Jmp => write!(f, "jmp"),
            Ret => write!(f, "ret"),
            Je => write!(f, "je"),
            Jl => write!(f, "jl"),
            Jle => write!(f, "jle"),
            Jb => write!(f, "jb"),
            Jbe => write!(f, "jbe"),
            Jp => write!(f, "jp"),
            Jo => write!(f, "jo"),
            Js => write!(f, "js"),
            Jne => write!(f, "jne"),
            Jnl => write!(f, "jnl"),
            Jnle => write!(f, "jnle"),
            Jnb => write!(f, "jnb"),
            Jnbe => write!(f, "jnbe"),
            Jnp => write!(f, "jnp"),
            Jno => write!(f, "jno"),
            Jns => write!(f, "jns"),
            Loop => write!(f, "loop"),
            Loopz => write!(f, "loopz"),
            Loopnz => write!(f, "loopnz"),
            Jcxz => write!(f, "jcxz"),
            Int => write!(f, "int"),
            Into => write!(f, "into"),
            IRet => write!(f, "iret"),
            Clc => write!(f, "clc"),
            Cmc => write!(f, "cmc"),
            Stc => write!(f, "stc"),
            Cld => write!(f, "cld"),
            Std => write!(f, "std"),
            Cli => write!(f, "cli"),
            Sti => write!(f, "sti"),
            Hlt => write!(f, "hlt"),
            Wait => write!(f, "wait"),
            Esc => write!(f, "esc"),
            Lock => write!(f, "lock"),
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

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.0 {
            OperandType::Direct(displacement) => {
                write!(f, "[{:#06x}]", displacement)?;
            }
            OperandType::Indirect(encoding, displacement) => {
                write!(f, "[{}", encoding)?;
                if *displacement > 0 {
                    write!(f, "{}]", displacement)?;
                } else {
                    write!(f, "]")?;
                }
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
            OperandSet::Offset(offset) => write!(f, "{:#06X}", offset)?,
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
