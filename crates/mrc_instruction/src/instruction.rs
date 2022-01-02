use crate::Address;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operation {
    AAA,
    AAD,
    AAM,
    AAS,
    ADC,
    ADD,
    AND,
    BAA,
    CALL,
    CBW,
    CLC,
    CLD,
    CLI,
    CMC,
    CMP,
    CMPSB,
    CMPSW,
    CWD,
    DAA,
    DAS,
    DEC,
    DIV,
    ESC,
    HLT,
    IDIV,
    IMUL,
    IN,
    INC,
    INT,
    INTO,
    IRET,
    JB,
    JBE,
    JCXZ,
    JE,
    JL,
    JLE,
    JMP,
    JNB,
    JNBE,
    JNE,
    JNL,
    JNLE,
    JNO,
    JNP,
    JNS,
    JO,
    JP,
    JS,
    LAHF,
    LDS,
    LEA,
    LES,
    LOCK,
    LODSB,
    LODSW,
    LOOP,
    LOOPNZ,
    LOOPZ,
    MOV,
    MOVSB,
    MOVSW,
    MUL,
    NEG,
    NOP,
    NOT,
    OR,
    OUT,
    POP,
    POPF,
    PUSH,
    PUSHF,
    RCL,
    RCR,
    RET,
    ROL,
    ROR,
    SAHF,
    SALC, // Undocumented
    SAR,
    SBB,
    SCASB,
    SCASW,
    SHL,
    SHR,
    STC,
    STD,
    STI,
    STOSB,
    STOSW,
    SUB,
    TEST,
    WAIT,
    XCHG,
    XLAT,
    XOR,
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::AAA => write!(f, "aaa"),
            Operation::AAD => write!(f, "aad"),
            Operation::AAM => write!(f, "aam"),
            Operation::AAS => write!(f, "aas"),
            Operation::ADC => write!(f, "adc"),
            Operation::ADD => write!(f, "add"),
            Operation::AND => write!(f, "and"),
            Operation::BAA => write!(f, "baa"),
            Operation::CALL => write!(f, "call"),
            Operation::CBW => write!(f, "cbw"),
            Operation::CLC => write!(f, "clc"),
            Operation::CLD => write!(f, "cld"),
            Operation::CLI => write!(f, "cli"),
            Operation::CMC => write!(f, "cmc"),
            Operation::CMP => write!(f, "cmp"),
            Operation::CMPSB => write!(f, "cmpsb"),
            Operation::CMPSW => write!(f, "cmpsw"),
            Operation::CWD => write!(f, "cwd"),
            Operation::DAA => write!(f, "daa"),
            Operation::DAS => write!(f, "das"),
            Operation::DEC => write!(f, "dec"),
            Operation::DIV => write!(f, "div"),
            Operation::ESC => write!(f, "esc"),
            Operation::HLT => write!(f, "hlt"),
            Operation::IDIV => write!(f, "idiv"),
            Operation::IMUL => write!(f, "imul"),
            Operation::IN => write!(f, "in"),
            Operation::INC => write!(f, "inc"),
            Operation::INT => write!(f, "int"),
            Operation::INTO => write!(f, "into"),
            Operation::IRET => write!(f, "iret"),
            Operation::JB => write!(f, "jb"),
            Operation::JBE => write!(f, "jbe"),
            Operation::JCXZ => write!(f, "jcxz"),
            Operation::JE => write!(f, "je"),
            Operation::JL => write!(f, "jl"),
            Operation::JLE => write!(f, "jle"),
            Operation::JMP => write!(f, "jmp"),
            Operation::JNB => write!(f, "jnb"),
            Operation::JNBE => write!(f, "jnbe"),
            Operation::JNE => write!(f, "jne"),
            Operation::JNL => write!(f, "jnl"),
            Operation::JNLE => write!(f, "jnle"),
            Operation::JNO => write!(f, "jno"),
            Operation::JNP => write!(f, "jnp"),
            Operation::JNS => write!(f, "jns"),
            Operation::JO => write!(f, "jo"),
            Operation::JP => write!(f, "jp"),
            Operation::JS => write!(f, "js"),
            Operation::LAHF => write!(f, "lahf"),
            Operation::LDS => write!(f, "lds"),
            Operation::LEA => write!(f, "lea"),
            Operation::LES => write!(f, "les"),
            Operation::LOCK => write!(f, "lock"),
            Operation::LODSB => write!(f, "lodsb"),
            Operation::LODSW => write!(f, "lodsw"),
            Operation::LOOP => write!(f, "loop"),
            Operation::LOOPNZ => write!(f, "loopnz"),
            Operation::LOOPZ => write!(f, "loopz"),
            Operation::MOV => write!(f, "mov"),
            Operation::MOVSB => write!(f, "movsb"),
            Operation::MOVSW => write!(f, "movsw"),
            Operation::MUL => write!(f, "mul"),
            Operation::NEG => write!(f, "neg"),
            Operation::NOP => write!(f, "nop"),
            Operation::NOT => write!(f, "not"),
            Operation::OR => write!(f, "or"),
            Operation::OUT => write!(f, "out"),
            Operation::POP => write!(f, "pop"),
            Operation::POPF => write!(f, "popf"),
            Operation::PUSH => write!(f, "push"),
            Operation::PUSHF => write!(f, "pushf"),
            Operation::RCL => write!(f, "rcl"),
            Operation::RCR => write!(f, "rcr"),
            Operation::RET => write!(f, "ret"),
            Operation::ROL => write!(f, "rol"),
            Operation::ROR => write!(f, "ror"),
            Operation::SAHF => write!(f, "sahf"),
            Operation::SALC => write!(f, "salc"),
            Operation::SAR => write!(f, "sar"),
            Operation::SBB => write!(f, "sbb"),
            Operation::SCASB => write!(f, "scasb"),
            Operation::SCASW => write!(f, "scasw"),
            Operation::SHL => write!(f, "shl"),
            Operation::SHR => write!(f, "shr"),
            Operation::STC => write!(f, "stc"),
            Operation::STD => write!(f, "std"),
            Operation::STI => write!(f, "sti"),
            Operation::STOSB => write!(f, "stosb"),
            Operation::STOSW => write!(f, "stosw"),
            Operation::SUB => write!(f, "sub"),
            Operation::TEST => write!(f, "test"),
            Operation::WAIT => write!(f, "wait"),
            Operation::XCHG => write!(f, "xchg"),
            Operation::XLAT => write!(f, "xlat"),
            Operation::XOR => write!(f, "xor"),
        }
    }
}

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

struct RegisterDisplay<'a>(&'a Register, &'a OperandSize);

impl std::fmt::Display for RegisterDisplay<'_> {
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Segment {
    Es,
    Cs,
    Ss,
    Ds,
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Segment::*;

        match self {
            Es => write!(f, "es"),
            Cs => write!(f, "cs"),
            Ss => write!(f, "ss"),
            Ds => write!(f, "ds"),
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

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! print_segment_prefix {
            ($segment:expr) => {
                match $segment {
                    Segment::Es => write!(f, "es:")?,
                    Segment::Cs => write!(f, "cs:")?,
                    Segment::Ss => write!(f, "ss:")?,
                    Segment::Ds => {}
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
            OperandType::Register(encoding) => write!(f, "{}", RegisterDisplay(encoding, &self.1))?,
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

/// Representation of a 8086 instruction.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub operation: Operation,
    pub operands: OperandSet,
    pub repeat: Option<Repeat>,
    pub lock: bool,
    pub address: Option<Address>,
}

impl Instruction {
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: None,
            lock: false,
            address: None,
        }
    }

    pub fn with_repeat(repeat: Repeat, operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: Some(repeat),
            lock: false,
            address: None,
        }
    }

    pub fn with_address(self, address: Address) -> Self {
        Self {
            address: Some(address),
            ..self
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:<10} {}", self.operation, &self.operands)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn with_address() {
        let i = Instruction::new(Operation::HLT, OperandSet::None);
        assert!(i.address.is_none());

        let i = i.with_address(Address::new(0x1000, 0x0010));
        assert_eq!(i.operation, Operation::HLT);
        assert_eq!(i.operands, OperandSet::None);
        assert_eq!(i.address, Some(Address::new(0x1000, 0x0010)));
    }
}
