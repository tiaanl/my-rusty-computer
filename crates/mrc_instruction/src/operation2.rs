#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operation {
    // DATA TRANSFER
    IN,    // Input from
    LAHF,  // Load AH with Flags
    LDS,   // Load Pointer to DS
    LEA,   // Load EA to Register
    LES,   // Load Pointer to ES
    MOV,   // Move
    OUT,   // Output to
    POP,   // Pop
    POPF,  // Pop Flags
    PUSH,  // Push
    PUSHF, // Push Flags
    SAHF,  // Store AH into Flags
    XCHG,  // Exchange
    XLAT,  // Translate Byte to AL

    // ARITHMETIC
    AAA,  // ASCII Adjust for Add
    AAD,  // ASCII Adjust for Divide
    AAM,  // ASCII Adjust for Multiply
    AAS,  // ASCII Adjust for Subtract
    ADC,  // Add with Carry
    ADD,  // Add
    CBW,  // Convert Byte to Word
    CMP,  // Compare
    CWD,  // Convert Word to Double Word
    DAA,  // Decimal Adjust for Add
    DAS,  // Decimal Adjust for Subtract
    DEC,  // Decrement
    DIV,  // Divide (Unsigned)
    IDIV, // Integer Divide (Signed)
    IMUL, // Integer Multiply (Signed)
    INC,  // Increment
    MUL,  // Multiply (Unsigned)
    NEG,  // Change sign
    SBB,  // Subtract with Borrow
    SUB,  // Subtract

    // LOGIC
    AND,  // And
    NOT,  // Invert
    OR,   // Or
    RCL,  // Rotate Through Carry Flag Left
    RCR,  // Rotate Through Carry Right
    ROL,  // Rotate Left
    ROR,  // Rotate Right
    SAR,  // Shift Arithmetic Right
    SHL,  // Shift Logical/Arithmetic Left
    SHR,  // Shift Logical Right
    TEST, // And Function to Flags. No Result
    XOR,  // Exclusive or

    // STRING MANIPULATION
    CMPS, // Compare Byte/Word
    LODS, // Load Byte/Wd to AL/AX
    MOVS, // Move Byte/Word
    REP,  // Repeat
    SCAS, // Scan Byte/Word
    STOS, // Stor Byte/Wd from AL/A

    // CONTROL TRANSFER
    CALL,   // Call
    INT,    // Interrupt
    INTO,   // Interrupt on Overflow
    IRET,   // Interrupt Return
    JB,     // Jump on Below/Not Above or Equal
    JBE,    // Jump on Below or Equal/Not Above
    JCXZ,   // Jump on CX Zero
    JE,     // Jump on Equal/Zero
    JL,     // Jump on Less/Not Greater or Equal
    JLE,    // Jump on Less or Equal/Not Greater
    JMP,    // Unconditional Jump
    JNB,    // Jump on Not Below/Above or Equal
    JNBE,   // Jump on Not Below or Equal/Above
    JNE,    // Jump on Not Equal/Not Zero
    JNL,    // Jump on Not Less/Greater or Equal
    JNLE,   // Jump on Not Less or Equal/Greater
    JNO,    // Jump on Not Overflow
    JNP,    // Jump on Not Par/Par Odd
    JNS,    // Jump on Not Sign
    JO,     // Jump on Overflow
    JP,     // Jump on Parity/Parity Even
    JS,     // Jump on Sign
    LOOP,   // Loop CX Times
    LOOPNZ, // Loop While Not Zero/Equal
    LOOPZ,  // Loop While Zero/Equal
    RET,    // Return from CALL

    // PROCESSOR CONTROL
    CLC,  // Clear Carry
    CLD,  // Clear Direction
    CLI,  // Clear Interrupt
    CMC,  // Complement Carry
    ESC,  // Escape (to External Device)
    HLT,  // Halt
    LOCK, // Bus Lock Prefix
    NOP,  // Cycle the CPU without permorning an action.
    STC,  // Set Carry
    STD,  // Set Direction
    STI,  // Set Interrupt
    WAIT, // Wait
}

const MNEMONICS_STR: &[&str] = &[
    "IN", "LAHF", "LDS", "LEA", "LES", "MOV", "OUT", "POP", "POPF", "PUSH", "PUSHF", "SAHF",
    "XCHG", "XLAT", "AAA", "AAD", "AAM", "AAS", "ADC", "ADD", "CBW", "CMP", "CWD", "DAA", "DAS",
    "DEC", "DIV", "IDIV", "IMUL", "INC", "MUL", "NEG", "SBB", "SUB", "AND", "NOT", "OR", "RCL",
    "RCR", "ROL", "ROR", "SAR", "SHL", "SHR", "TEST", "XOR", "CMPS", "LODS", "MOVS", "REP", "SCAS",
    "STOS", "CALL", "INT", "INTO", "IRET", "JB", "JBE", "JCXZ", "JE", "JL", "JLE", "JMP", "JNB",
    "JNBE", "JNE", "JNL", "JNLE", "JNO", "JNP", "JNS", "JO", "JP", "JS", "LOOP", "LOOPNZ", "LOOPZ",
    "RET", "CLC", "CLD", "CLI", "CMC", "ESC", "HLT", "LOCK", "NOP", "STC", "STD", "STI", "WAIT",
];

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", MNEMONICS_STR[*self as usize])
    }
}

impl std::str::FromStr for Operation {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operation::*;

        Ok(match s.to_string().to_lowercase().as_str() {
            "in" => IN,
            "lahf" => LAHF,
            "lds" => LDS,
            "lea" => LEA,
            "les" => LES,
            "mov" => MOV,
            "out" => OUT,
            "pop" => POP,
            "popf" => POPF,
            "push" => PUSH,
            "pushf" => PUSHF,
            "sahf" => SAHF,
            "xchg" => XCHG,
            "xlat" => XLAT,
            "aaa" => AAA,
            "aad" => AAD,
            "aam" => AAM,
            "aas" => AAS,
            "adc" => ADC,
            "add" => ADD,
            "cbw" => CBW,
            "cmp" => CMP,
            "cwd" => CWD,
            "daa" => DAA,
            "das" => DAS,
            "dec" => DEC,
            "div" => DIV,
            "idiv" => IDIV,
            "imul" => IMUL,
            "inc" => INC,
            "mul" => MUL,
            "neg" => NEG,
            "sbb" => SBB,
            "sub" => SUB,
            "and" => AND,
            "not" => NOT,
            "or" => OR,
            "rcl" => RCL,
            "rcr" => RCR,
            "rol" => ROL,
            "ror" => ROR,
            "sar" => SAR,
            "shl" => SHL,
            "sal" => SHL,
            "shr" => SHR,
            "test" => TEST,
            "xor" => XOR,
            "cmps" => CMPS,
            "lods" => LODS,
            "movs" => MOVS,
            "rep" => REP,
            "scas" => SCAS,
            "stos" => STOS,
            "call" => CALL,
            "int" => INT,
            "into" => INTO,
            "iret" => IRET,
            "jb" => JB,
            "jnae" => JB,
            "jbe" => JBE,
            "jna" => JBE,
            "jcxz" => JCXZ,
            "je" => JE,
            "jz" => JE,
            "jl" => JL,
            "jnge" => JL,
            "jle" => JLE,
            "jng" => JLE,
            "jmp" => JMP,
            "jnb" => JNB,
            "jae" => JNB,
            "jnbe" => JNBE,
            "ja" => JNBE,
            "jne" => JNE,
            "jnz" => JNE,
            "jnl" => JNL,
            "jge" => JNL,
            "jnle" => JNLE,
            "jg" => JNLE,
            "jno" => JNO,
            "jnp" => JNP,
            "jpo" => JNP,
            "jns" => JNS,
            "jo" => JO,
            "jp" => JP,
            "jpe" => JP,
            "js" => JS,
            "loop" => LOOP,
            "loopnz" => LOOPNZ,
            "loopne" => LOOPNZ,
            "loopz" => LOOPZ,
            "loope" => LOOPZ,
            "ret" => RET,
            "clc" => CLC,
            "cld" => CLD,
            "cli" => CLI,
            "cmc" => CMC,
            "esc" => ESC,
            "hlt" => HLT,
            "lock" => LOCK,
            "nop" => NOP,
            "stc" => STC,
            "std" => STD,
            "sti" => STI,
            "wait" => WAIT,
            _ => return Err(()),
        })
    }
}
