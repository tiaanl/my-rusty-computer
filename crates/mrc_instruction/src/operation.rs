#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    LOOPE,  // Loop while zero/equal (alias LOOPZ)
    LOOPNE, // Loop while not zero/equal (alias LOOPNZ)
    JCXZ,   // Jump on CX zero
    INT,    // Interrupt
    INT1,   // Interrupt 1
    INT3,   // Interrupt 3
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
                LOOPE => "loopz",
                LOOPNE => "loopnz",
                JCXZ => "jcxz",
                INT => "int",
                INT1 => "int1",
                INT3 => "int3",
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operation::*;

        Ok(match s.to_string().to_lowercase().as_str() {
            "aaa" => AAA,
            "aad" => AAD,
            "aam" => AAM,
            "aas" => AAS,
            "adc" => ADC,
            "add" => ADD,
            "and" => AND,
            "call" => CALL,
            "cbw" => CBW,
            "clc" => CLC,
            "cld" => CLD,
            "cli" => CLI,
            "cmc" => CMC,
            "cmp" => CMP,
            "cmpsb" => CMPSB,
            "cmpsw" => CMPSW,
            "cwd" => CWD,
            "daa" => DAA,
            "das" => DAS,
            "dec" => DEC,
            "div" => DIV,
            "esc" => ESC,
            "hlt" => HLT,
            "idiv" => IDIV,
            "imul" => IMUL,
            "in" => IN,
            "inc" => INC,
            "int" => INT,
            "int1" => INT1,
            "int3" => INT3,
            "into" => INTO,
            "iret" => IRET,
            "ja" => JNBE,
            "jae" => JNB,
            "jb" => JB,
            "jbe" => JBE,
            "jcxz" => JCXZ,
            "je" => JE,
            "jg" => JNLE,
            "jge" => JNL,
            "jl" => JL,
            "jle" => JLE,
            "jmp" => JMP,
            "jna" => JBE,
            "jnae" => JB,
            "jnb" => JNB,
            "jnbe" => JNBE,
            "jne" => JNE,
            "jng" => JLE,
            "jnge" => JL,
            "jnl" => JNL,
            "jnle" => JNLE,
            "jno" => JNO,
            "jnp" => JNP,
            "jns" => JNS,
            "jnz" => JNE,
            "jo" => JO,
            "jp" => JP,
            "jpe" => JP,
            "js" => JS,
            "jz" => JE,
            "lahf" => LAHF,
            "lds" => LDS,
            "lea" => LEA,
            "les" => LES,
            "lock" => LOCK,
            "lodsb" => LODSB,
            "lodsw" => LODSW,
            "loop" => LOOP,
            "loope" => LOOPE,
            "loopne" => LOOPNE,
            "loopnz" => LOOPNE,
            "loopz" => LOOPE,
            "mov" => MOV,
            "movsb" => MOVSB,
            "movsw" => MOVSW,
            "mul" => MUL,
            "neg" => NEG,
            "nop" => NOP,
            "not" => NOT,
            "or" => OR,
            "out" => OUT,
            "pop" => POP,
            "popf" => POPF,
            "push" => PUSH,
            "pushf" => PUSHF,
            "rcl" => RCL,
            "rcr" => RCR,
            "rep" => REP,
            "ret" => RET,
            "rol" => ROL,
            "ror" => ROR,
            "sahf" => SAHF,
            "salc" => SALC,
            "sar" => SAR,
            "sbb" => SBB,
            "scasb" => SCASB,
            "scasw" => SCASW,
            "shl" => SHL,
            "shr" => SHR,
            "stc" => STC,
            "std" => STD,
            "sti" => STI,
            "stosb" => STOSB,
            "stosw" => STOSW,
            "sub" => SUB,
            "test" => TEST,
            "wait" => WAIT,
            "xchg" => XCHG,
            "xlat" => XLAT,
            "xor" => XOR,
            _ => return Err(()),
        })
    }
}
