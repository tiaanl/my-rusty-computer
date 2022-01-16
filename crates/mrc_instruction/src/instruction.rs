use crate::Address;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Operation {
    // Data transfer
    MOV,   // Move
    PUSH,  // Push
    POP,   // Pop
    XCHG,  // Exchange
    IN,    // Input from
    OUT,   // Output to
    XLAT,  // Translate byte to AL
    LEA,   // Load effective address to register
    LDS,   // Load pointer to DS
    LES,   // Load pointer to ES
    LAHF,  // Load AH with flags
    SAHF,  // Store AH into flags
    PUSHF, // Push flags
    POPF,  // Pop flags

    // Arithmetic
    ADD,  // Add
    ADC,  // Add with carry
    INC,  // Increment
    AAA,  // ASCII adjust for add
    DAA,  // Decimal adjust for add
    SUB,  // Subtract
    SBB,  // Subtract with borrow
    DEC,  // Decrement
    NEG,  // Change sign
    CMP,  // Compare
    AAS,  // ASCII adjust for subtract
    DAS,  // Decimal adjust for subtract
    MUL,  // Multiply (unsigned)
    IMUL, // Integer multiply (signed)
    AAM,  // ASCII adjust for multiply
    DIV,  // Divide (unsigned)
    IDIV, // Integer divide (signed)
    AAD,  // ASCII adjust for divide
    CBW,  // Convert byte to word
    CWD,  // Convert word to double word

    // Logic
    NOT,  // Invert
    SHL,  // Shift logical left (alias: SAL)
    SHR,  // Shift logical right
    SAR,  // Shift arithmetic right
    ROL,  // Rotate left
    ROR,  // Rotate right
    RCL,  // Rotate through carry flag left
    RCR,  // Rotate through carry flag right
    AND,  // And
    TEST, // And function to flags, no result
    OR,   // Or
    XOR,  // Exclusive or

    // String manipulation
    // TODO: Should these be without size and have the size as another operand type?
    REP,   // Repeat
    MOVSB, // Move byte
    MOVSW, // Move word
    CMPSB, // Compare byte
    CMPSW, // Compare word
    SCASB, // Scan byte
    SCASW, // Scan word
    LODSB, // Load byte to AL
    LODSW, // Load word to AX
    STOSB, // Store byte to AL
    STOSW, // Store word to AX

    // Control transfer
    CALL,   // Call
    JMP,    // Unconditional jump
    RET,    // Return from CALL
    JE,     // Jump on equal/zero (alias JZ)
    JL,     // Jump on less/not greater or equal (alias JNGE)
    JLE,    // Jump on less or equal/not greater (alias JNG)
    JB,     // Jump on below/not above or equal (alias JNAE)
    JBE,    // Jump on below or equal/not above (alias JNA)
    JP,     // Jump on parity/parity even (alias JPE)
    JO,     // Jump on overflow
    JS,     // Jump on sign
    JNE,    // Jump on not equal/not zero (alias JNZ)
    JNL,    // Jump on not less/greater or equal (alias JGE)
    JNLE,   // Jump on not less or equal/greater (alias JG)
    JNB,    // Jump on not below/above or equal (alias JAE)
    JNBE,   // Jump on not below or equal/above (alias JA)
    JNP,    // Jump on not parity/parity odd (alias JPO)
    JNO,    // Jump on not overflow
    JNS,    // Jump on not sign
    LOOP,   // Loop CX times
    LOOPZ,  // Loop while zero/equal (alias LOOPE)
    LOOPNZ, // Loop while not zero/equal (alias LOOPNE)
    JCXZ,   // Jump on CX zero
    INT,    // Interrupt
    INTO,   // Interrupt on overflow
    IRET,   // Interrupt return

    // Processor control
    CLC,  // Clear carry
    CMC,  // Complement carry
    STC,  // Set carry
    CLD,  // Clear direction
    STD,  // Set direction
    CLI,  // Clear interrupt
    STI,  // Set interrupt
    HLT,  // Halt
    WAIT, // Wait
    ESC,  // Escape (to external device)
    LOCK, // Bus lock prefix

    NOP, // No operation

    // Undocumented
    SALC, // Set AL on carry
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operation::*;
        write!(
            f,
            "{}",
            match self {
                MOV => "mov",
                PUSH => "push",
                POP => "pop",
                XCHG => "xchg",
                IN => "in",
                OUT => "out",
                XLAT => "xlat",
                LEA => "lea",
                LDS => "lds",
                LES => "les",
                LAHF => "lahf",
                SAHF => "sahf",
                PUSHF => "pushf",
                POPF => "popf",
                ADD => "add",
                ADC => "adc",
                INC => "inc",
                AAA => "aaa",
                DAA => "daa",
                SUB => "sub",
                SBB => "sbb",
                DEC => "dec",
                NEG => "neg",
                CMP => "cmp",
                AAS => "aas",
                DAS => "das",
                MUL => "mul",
                IMUL => "imul",
                AAM => "aam",
                DIV => "div",
                IDIV => "idiv",
                AAD => "aad",
                CBW => "cbw",
                CWD => "cwd",
                NOT => "not",
                SHL => "shl",
                SHR => "shr",
                SAR => "sar",
                ROL => "rol",
                ROR => "ror",
                RCL => "rcl",
                RCR => "rcr",
                AND => "and",
                TEST => "test",
                OR => "or",
                XOR => "xor",
                REP => "rep",
                MOVSB => "movsb",
                MOVSW => "movsw",
                CMPSB => "cmpsb",
                CMPSW => "cmpsw",
                SCASB => "scasb",
                SCASW => "scasw",
                LODSB => "lodsb",
                LODSW => "lodsw",
                STOSB => "stosb",
                STOSW => "stosw",
                CALL => "call",
                JMP => "jmp",
                RET => "ret",
                JE => "je",
                JL => "jl",
                JLE => "jle",
                JB => "jb",
                JBE => "jbe",
                JP => "jp",
                JO => "jo",
                JS => "js",
                JNE => "jne",
                JNL => "jnl",
                JNLE => "jnle",
                JNB => "jnb",
                JNBE => "jnbe",
                JNP => "jnp",
                JNO => "jno",
                JNS => "jns",
                LOOP => "loop",
                LOOPZ => "loopz",
                LOOPNZ => "loopnz",
                JCXZ => "jcxz",
                INT => "int",
                INTO => "into",
                IRET => "iret",
                CLC => "clc",
                CMC => "cmc",
                STC => "stc",
                CLD => "cld",
                STD => "std",
                CLI => "cli",
                STI => "sti",
                HLT => "hlt",
                WAIT => "wait",
                ESC => "esc",
                LOCK => "lock",
                NOP => "nop",
                SALC => "salc",
            }
        )
    }
}

impl std::str::FromStr for Operation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operation::*;

        Ok(match s.to_string().to_lowercase().as_str() {
            "mov" => MOV,
            "push" => PUSH,
            "pop" => POP,
            "xchg" => XCHG,
            "in" => IN,
            "out" => OUT,
            "xlat" => XLAT,
            "lea" => LEA,
            "lds" => LDS,
            "les" => LES,
            "lahf" => LAHF,
            "sahf" => SAHF,
            "pushf" => PUSHF,
            "popf" => POPF,
            "add" => ADD,
            "adc" => ADC,
            "inc" => INC,
            "aaa" => AAA,
            "daa" => DAA,
            "sub" => SUB,
            "sbb" => SBB,
            "dec" => DEC,
            "neg" => NEG,
            "cmp" => CMP,
            "aas" => AAS,
            "das" => DAS,
            "mul" => MUL,
            "imul" => IMUL,
            "aam" => AAM,
            "div" => DIV,
            "idiv" => IDIV,
            "aad" => AAD,
            "cbw" => CBW,
            "cwd" => CWD,
            "not" => NOT,
            "shl" => SHL,
            "shr" => SHR,
            "sar" => SAR,
            "rol" => ROL,
            "ror" => ROR,
            "rcl" => RCL,
            "rcr" => RCR,
            "and" => AND,
            "test" => TEST,
            "or" => OR,
            "xor" => XOR,
            "rep" => REP,
            "movsb" => MOVSB,
            "movsw" => MOVSW,
            "cmpsb" => CMPSB,
            "cmpsw" => CMPSW,
            "scasb" => SCASB,
            "scasw" => SCASW,
            "lodsb" => LODSB,
            "lodsw" => LODSW,
            "stosb" => STOSB,
            "stosw" => STOSW,
            "call" => CALL,
            "jmp" => JMP,
            "ret" => RET,
            "je" => JE,
            "jl" => JL,
            "jle" => JLE,
            "jb" => JB,
            "jbe" => JBE,
            "jp" => JP,
            "jo" => JO,
            "js" => JS,
            "jne" => JNE,
            "jnl" => JNL,
            "jnle" => JNLE,
            "jnb" => JNB,
            "jnbe" => JNBE,
            "jnp" => JNP,
            "jno" => JNO,
            "jns" => JNS,
            "loop" => LOOP,
            "loopz" => LOOPZ,
            "loopnz" => LOOPNZ,
            "jcxz" => JCXZ,
            "int" => INT,
            "into" => INTO,
            "iret" => IRET,
            "clc" => CLC,
            "cmc" => CMC,
            "stc" => STC,
            "cld" => CLD,
            "std" => STD,
            "cli" => CLI,
            "sti" => STI,
            "hlt" => HLT,
            "wait" => WAIT,
            "esc" => ESC,
            "lock" => LOCK,
            "nop" => NOP,
            "salc" => SALC,
            _ => return Err(s.to_string()),
        })
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SizedRegister(pub Register, pub OperandSize);

impl std::fmt::Display for SizedRegister {
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

impl FromStr for SizedRegister {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use OperandSize::*;
        use Register::*;

        match s.to_lowercase().as_str() {
            "al" => Ok(Self(AlAx, Byte)),
            "cl" => Ok(Self(ClCx, Byte)),
            "dl" => Ok(Self(DlDx, Byte)),
            "bl" => Ok(Self(BlBx, Byte)),
            "ah" => Ok(Self(AhSp, Byte)),
            "ch" => Ok(Self(ChBp, Byte)),
            "dh" => Ok(Self(DhSi, Byte)),
            "bh" => Ok(Self(BhDi, Byte)),

            "ax" => Ok(Self(AlAx, Word)),
            "cx" => Ok(Self(ClCx, Word)),
            "dx" => Ok(Self(DlDx, Word)),
            "bx" => Ok(Self(BlBx, Word)),
            "sp" => Ok(Self(AhSp, Word)),
            "bp" => Ok(Self(ChBp, Word)),
            "si" => Ok(Self(DhSi, Word)),
            "di" => Ok(Self(BhDi, Word)),

            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Segment {
    ES,
    CS,
    SS,
    DS,
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Segment::*;

        match self {
            ES => write!(f, "es"),
            CS => write!(f, "cs"),
            SS => write!(f, "ss"),
            DS => write!(f, "ds"),
        }
    }
}

impl FromStr for Segment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "es" => Ok(Segment::ES),
            "cs" => Ok(Segment::CS),
            "ss" => Ok(Segment::SS),
            "ds" => Ok(Segment::DS),
            _ => Err(s.to_string()),
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

impl From<SizedRegister> for Operand {
    fn from(sized_register: SizedRegister) -> Self {
        Self(OperandType::Register(sized_register.0), sized_register.1)
    }
}

impl From<Segment> for Operand {
    fn from(segment: Segment) -> Self {
        Self(OperandType::Segment(segment), OperandSize::Word)
    }
}

impl std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! print_segment_prefix {
            ($segment:expr) => {
                match $segment {
                    Segment::ES => write!(f, "es:")?,
                    Segment::CS => write!(f, "cs:")?,
                    Segment::SS => write!(f, "ss:")?,
                    Segment::DS => {}
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
            OperandType::Register(encoding) => write!(f, "{}", SizedRegister(*encoding, self.1))?,
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

/// Representation of a 8086 (and friends) instruction.
///
/// ```rust
/// use mrc_instruction::*;
///
/// // mov ax, [es:bx+si+8]
/// let i = Instruction::new(
///     Operation::MOV,
///     OperandSet::DestinationAndSource(
///         Operand(OperandType::Register(Register::AlAx), OperandSize::Word),
///         Operand(
///             OperandType::Indirect(Segment::ES, AddressingMode::BxSi, Displacement::Byte(8)),
///             OperandSize::Word,
///         ),
///     ),
/// );
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Instruction {
    pub operation: Operation,
    pub operands: OperandSet,
    pub repeat: Option<Repeat>,
    pub lock: bool,
    pub address: Option<Address>,
}

impl Instruction {
    /// Create a new instruction with the given [Operation] and [OperandSet].
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: None,
            lock: false,
            address: None,
        }
    }

    /// Create a new instruction with the given [Operation] and [OperandSet], as well as a [Repeat]
    /// flag.
    pub fn with_repeat(repeat: Repeat, operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            repeat: Some(repeat),
            lock: false,
            address: None,
        }
    }

    /// Consumes this [Instruction] and returns a duplicate with the `address` set the the given
    /// [Address] value.
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
