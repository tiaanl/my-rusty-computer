use crate::Operation;

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum OperandType {
    None,

    OpCodeReg,

    // Specific Register
    AL,
    CL,
    DL,

    AX,
    CX,
    DX,

    // Register
    Reg8,
    Reg16,

    // Memory address, e.g. [0xABCD] or [bx + 4]
    Mem,

    MemFar, // ...with segment prefix, e.g. 0FFF:9BCD

    // Immediate
    Imm8,
    Imm16,
    Imm1, // The immediate value of 1

    // Sign extended immediate
    SignedImm8,
    SignedImm16,

    // Segment Register
    SegReg,

    // Jump Displacement
    Displacement8,
    Displacement16,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OperationMap {
    Single(Operation),
    ModrmReg(&'static [Operation]),
}

impl PartialEq<Operation> for OperationMap {
    fn eq(&self, other: &Operation) -> bool {
        match self {
            Single(operation) => operation == other,
            ModrmReg(group) => group.iter().any(|o| o == other),
        }
    }
}

#[derive(Debug)]
pub struct InstructionData {
    pub op_code: u8,
    pub operation: OperationMap,
    pub destination: OperandType,
    pub source: OperandType,
}

macro_rules! op {
    ($op_code:expr,$operation_map:expr,$destination:ident,$source:ident) => {{
        InstructionData {
            op_code: $op_code,
            operation: $operation_map,
            destination: OperandType::$destination,
            source: OperandType::$source,
        }
    }};
}

use crate::Operation::*;
use OperationMap::*;

pub const INSTRUCTION_DATA: &[InstructionData] = &[
    op!(0x04, Single(ADD), AL, Imm8),
    op!(0x05, Single(ADD), AX, Imm16),
    op!(0x00, Single(ADD), Reg8, Mem),
    op!(0x01, Single(ADD), Reg16, Mem),
    op!(0x02, Single(ADD), Mem, Reg8),
    op!(0x03, Single(ADD), Mem, Reg16),
    op!(0x06, Single(PUSH), SegReg, None),
    op!(0x07, Single(POP), SegReg, None),
    op!(0x0C, Single(OR), AL, Imm8),
    op!(0x0D, Single(OR), AX, Imm16),
    op!(0x08, Single(OR), Reg8, Mem),
    op!(0x09, Single(OR), Reg16, Mem),
    op!(0x0A, Single(OR), Mem, Reg8),
    op!(0x0B, Single(OR), Mem, Reg16),
    op!(0x0E, Single(PUSH), SegReg, None),
    op!(0x14, Single(ADC), AL, Imm8),
    op!(0x15, Single(ADC), AX, Imm16),
    op!(0x10, Single(ADC), Reg8, Mem),
    op!(0x11, Single(ADC), Reg16, Mem),
    op!(0x12, Single(ADC), Mem, Reg8),
    op!(0x13, Single(ADC), Mem, Reg16),
    op!(0x16, Single(PUSH), SegReg, None),
    op!(0x17, Single(POP), SegReg, None),
    op!(0x1C, Single(SBB), AL, Imm8),
    op!(0x1D, Single(SBB), AX, Imm16),
    op!(0x18, Single(SBB), Reg8, Mem),
    op!(0x19, Single(SBB), Reg16, Mem),
    op!(0x1A, Single(SBB), Mem, Reg8),
    op!(0x1B, Single(SBB), Mem, Reg16),
    op!(0x1E, Single(PUSH), SegReg, None),
    op!(0x1F, Single(POP), SegReg, None),
    op!(0x24, Single(AND), AL, Imm8),
    op!(0x25, Single(AND), AX, Imm16),
    op!(0x20, Single(AND), Reg8, Mem),
    op!(0x21, Single(AND), Reg16, Mem),
    op!(0x22, Single(AND), Mem, Reg8),
    op!(0x23, Single(AND), Mem, Reg16),
    op!(0x27, Single(DAA), None, None),
    op!(0x2C, Single(SUB), AL, Imm8),
    op!(0x2D, Single(SUB), AX, Imm16),
    op!(0x28, Single(SUB), Reg8, Mem),
    op!(0x29, Single(SUB), Reg16, Mem),
    op!(0x2A, Single(SUB), Mem, Reg8),
    op!(0x2B, Single(SUB), Mem, Reg16),
    op!(0x2F, Single(DAS), None, None),
    op!(0x34, Single(XOR), AL, Imm8),
    op!(0x35, Single(XOR), AX, Imm16),
    op!(0x30, Single(XOR), Reg8, Mem),
    op!(0x31, Single(XOR), Reg16, Mem),
    op!(0x32, Single(XOR), Mem, Reg8),
    op!(0x33, Single(XOR), Mem, Reg16),
    op!(0x37, Single(AAA), None, None),
    op!(0x3C, Single(CMP), AL, Imm8),
    op!(0x3D, Single(CMP), AX, Imm16),
    op!(0x38, Single(CMP), Reg8, Mem),
    op!(0x39, Single(CMP), Reg16, Mem),
    op!(0x3A, Single(CMP), Mem, Reg8),
    op!(0x3B, Single(CMP), Mem, Reg16),
    op!(0x3F, Single(AAS), None, None),
    op!(0x40, Single(INC), OpCodeReg, None),
    op!(0x41, Single(INC), OpCodeReg, None),
    op!(0x42, Single(INC), OpCodeReg, None),
    op!(0x43, Single(INC), OpCodeReg, None),
    op!(0x44, Single(INC), OpCodeReg, None),
    op!(0x45, Single(INC), OpCodeReg, None),
    op!(0x46, Single(INC), OpCodeReg, None),
    op!(0x47, Single(INC), OpCodeReg, None),
    op!(0x48, Single(DEC), OpCodeReg, None),
    op!(0x49, Single(DEC), OpCodeReg, None),
    op!(0x4A, Single(DEC), OpCodeReg, None),
    op!(0x4B, Single(DEC), OpCodeReg, None),
    op!(0x4C, Single(DEC), OpCodeReg, None),
    op!(0x4D, Single(DEC), OpCodeReg, None),
    op!(0x4E, Single(DEC), OpCodeReg, None),
    op!(0x4F, Single(DEC), OpCodeReg, None),
    op!(0x50, Single(PUSH), OpCodeReg, None),
    op!(0x51, Single(PUSH), OpCodeReg, None),
    op!(0x52, Single(PUSH), OpCodeReg, None),
    op!(0x53, Single(PUSH), OpCodeReg, None),
    op!(0x54, Single(PUSH), OpCodeReg, None),
    op!(0x55, Single(PUSH), OpCodeReg, None),
    op!(0x56, Single(PUSH), OpCodeReg, None),
    op!(0x57, Single(PUSH), OpCodeReg, None),
    op!(0x58, Single(POP), OpCodeReg, None),
    op!(0x59, Single(POP), OpCodeReg, None),
    op!(0x5A, Single(POP), OpCodeReg, None),
    op!(0x5B, Single(POP), OpCodeReg, None),
    op!(0x5C, Single(POP), OpCodeReg, None),
    op!(0x5D, Single(POP), OpCodeReg, None),
    op!(0x5E, Single(POP), OpCodeReg, None),
    op!(0x5F, Single(POP), OpCodeReg, None),
    op!(0x68, Single(PUSH), Imm16, None),
    op!(0x69, Single(IMUL), SignedImm16, Reg16),
    op!(0x6A, Single(PUSH), Imm8, None),
    op!(0x6B, Single(IMUL), SignedImm8, Mem),
    // op!(0x6C, INSB, DX, Yb),
    // op!(0x6D, INSW, DX, Yw),
    // op!(0x6E, OUTSB, Xb, DX),
    // op!(0x6F, OUTSW, Xw, DX),
    op!(0x70, Single(JO), Displacement8, None),
    op!(0x71, Single(JNO), Displacement8, None),
    op!(0x72, Single(JB), Displacement8, None),
    op!(0x73, Single(JNB), Displacement8, None),
    op!(0x74, Single(JE), Displacement8, None),
    op!(0x75, Single(JNE), Displacement8, None),
    op!(0x76, Single(JBE), Displacement8, None),
    op!(0x77, Single(JNBE), Displacement8, None),
    op!(0x78, Single(JS), Displacement8, None),
    op!(0x79, Single(JNS), Displacement8, None),
    op!(0x7A, Single(JP), Displacement8, None),
    op!(0x7B, Single(JNP), Displacement8, None),
    op!(0x7C, Single(JL), Displacement8, None),
    op!(0x7D, Single(JNL), Displacement8, None),
    op!(0x7E, Single(JLE), Displacement8, None),
    op!(0x7F, Single(JNLE), Displacement8, None),
    op!(
        0x80,
        ModrmReg(&[ADD, OR, ADC, SBB, AND, SUB, XOR, CMP]),
        Mem,
        Imm8
    ),
    op!(
        0x81,
        ModrmReg(&[ADD, OR, ADC, SBB, AND, SUB, XOR, CMP]),
        Mem,
        Imm16
    ),
    // op!(0x82, NOP, None, None),
    op!(
        0x83,
        ModrmReg(&[ADD, OR, ADC, SBB, AND, SUB, XOR, CMP]),
        Reg16,
        SignedImm8
    ),
    op!(0x84, Single(TEST), Reg8, Mem),
    op!(0x85, Single(TEST), Reg16, Mem),
    op!(0x86, Single(XCHG), Reg8, Mem),
    op!(0x87, Single(XCHG), Reg16, Mem),
    op!(0x88, Single(MOV), Reg8, Mem),
    op!(0x89, Single(MOV), Reg16, Mem),
    op!(0x8A, Single(MOV), Mem, Reg8),
    op!(0x8B, Single(MOV), Mem, Reg16),
    op!(0x8C, Single(MOV), Reg16, SegReg),
    op!(0x8D, Single(LEA), Mem, Reg16),
    op!(0x8E, Single(MOV), SegReg, Reg16),
    op!(0x8F, Single(POP), Mem, None),
    op!(0x90, Single(NOP), None, None),
    op!(0x91, Single(XCHG), AX, OpCodeReg),
    op!(0x92, Single(XCHG), AX, OpCodeReg),
    op!(0x93, Single(XCHG), AX, OpCodeReg),
    op!(0x94, Single(XCHG), AX, OpCodeReg),
    op!(0x95, Single(XCHG), AX, OpCodeReg),
    op!(0x96, Single(XCHG), AX, OpCodeReg),
    op!(0x97, Single(XCHG), AX, OpCodeReg),
    op!(0x98, Single(CBW), None, None),
    op!(0x99, Single(CWD), None, None),
    op!(0x9A, Single(CALL), MemFar, None),
    op!(0x9B, Single(WAIT), None, None),
    op!(0x9C, Single(PUSHF), None, None),
    op!(0x9D, Single(POPF), None, None),
    op!(0x9E, Single(SAHF), None, None),
    op!(0x9F, Single(LAHF), None, None),
    op!(0xA0, Single(MOV), AL, Mem),
    op!(0xA1, Single(MOV), AX, Mem),
    op!(0xA2, Single(MOV), Mem, AL),
    op!(0xA3, Single(MOV), Mem, AX),
    op!(0xA4, Single(MOVSB), None, None),
    op!(0xA5, Single(MOVSW), None, None),
    op!(0xA6, Single(CMPSB), None, None),
    op!(0xA7, Single(CMPSW), None, None),
    op!(0xA8, Single(TEST), AL, Imm8),
    op!(0xA9, Single(TEST), AX, Imm16),
    op!(0xAA, Single(STOSB), None, None),
    op!(0xAB, Single(STOSW), None, None),
    op!(0xAC, Single(LODSB), None, None),
    op!(0xAD, Single(LODSW), None, None),
    op!(0xAE, Single(SCASB), None, None),
    op!(0xAF, Single(SCASW), None, None),
    op!(0xB0, Single(MOV), OpCodeReg, Imm8),
    op!(0xB1, Single(MOV), OpCodeReg, Imm8),
    op!(0xB2, Single(MOV), OpCodeReg, Imm8),
    op!(0xB3, Single(MOV), OpCodeReg, Imm8),
    op!(0xB4, Single(MOV), OpCodeReg, Imm8),
    op!(0xB5, Single(MOV), OpCodeReg, Imm8),
    op!(0xB6, Single(MOV), OpCodeReg, Imm8),
    op!(0xB7, Single(MOV), OpCodeReg, Imm8),
    op!(0xB8, Single(MOV), OpCodeReg, Imm16),
    op!(0xB9, Single(MOV), OpCodeReg, Imm16),
    op!(0xBA, Single(MOV), OpCodeReg, Imm16),
    op!(0xBB, Single(MOV), OpCodeReg, Imm16),
    op!(0xBC, Single(MOV), OpCodeReg, Imm16),
    op!(0xBD, Single(MOV), OpCodeReg, Imm16),
    op!(0xBE, Single(MOV), OpCodeReg, Imm16),
    op!(0xBF, Single(MOV), OpCodeReg, Imm16),
    op!(
        0xC0,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg8,
        Imm8
    ),
    op!(
        0xC1,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg16,
        Imm8
    ),
    op!(0xC2, Single(RET), Displacement16, None),
    op!(0xC3, Single(RET), None, None),
    op!(0xC4, Single(LES), Mem, Reg16),
    op!(0xC5, Single(LDS), Mem, Reg16),
    op!(0xC6, Single(MOV), Reg8, Imm8),
    op!(0xC7, Single(MOV), Reg16, Imm16),
    // op!(0xC8, ENTER, Imm8, Imm16),
    // op!(0xC9, LEAVE, None, None),
    op!(0xCA, Single(RET), Imm16, None),
    op!(0xCB, Single(RET), None, None),
    op!(0xCC, Single(INT3), None, None), //  Should we create Int3 as an instruction?
    op!(0xCD, Single(INT), Imm8, None),
    op!(0xCE, Single(INTO), None, None),
    op!(0xCF, Single(IRET), None, None),
    op!(
        0xD0,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg8,
        Imm1
    ),
    op!(
        0xD1,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg16,
        Imm1
    ),
    op!(
        0xD2,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg8,
        CL
    ),
    op!(
        0xD3,
        ModrmReg(&[ROL, ROR, RCL, RCR, SHL, SHR, SHL, SAR]),
        Reg16,
        CX
    ),
    op!(0xD4, Single(AAM), Imm8, None),
    op!(0xD5, Single(AAD), Imm8, None),
    op!(0xD6, Single(SALC), None, None),
    op!(0xD7, Single(XLAT), None, None),
    op!(0xD8, Single(ESC), None, None),
    op!(0xD9, Single(ESC), None, None),
    op!(0xDA, Single(ESC), None, None),
    op!(0xDB, Single(ESC), None, None),
    op!(0xDC, Single(ESC), None, None),
    op!(0xDD, Single(ESC), None, None),
    op!(0xDE, Single(ESC), None, None),
    op!(0xDF, Single(ESC), None, None),
    op!(0xE0, Single(LOOPNZ), Displacement8, None),
    op!(0xE1, Single(LOOPZ), Displacement8, None),
    op!(0xE2, Single(LOOP), Displacement8, None),
    op!(0xE3, Single(JCXZ), Displacement8, None),
    op!(0xE4, Single(IN), AL, Imm8),
    op!(0xE5, Single(IN), AX, Imm8),
    op!(0xE6, Single(OUT), Imm8, AL),
    op!(0xE7, Single(OUT), Imm8, AX),
    op!(0xE8, Single(CALL), Displacement16, None),
    op!(0xE9, Single(JMP), Displacement16, None),
    op!(0xEA, Single(JMP), MemFar, None),
    op!(0xEB, Single(JMP), Displacement8, None),
    op!(0xEC, Single(IN), AL, DX),
    op!(0xED, Single(IN), AX, DX),
    op!(0xEE, Single(OUT), DX, AL),
    op!(0xEF, Single(OUT), DX, AX),
    op!(0xF0, Single(LOCK), None, None),
    op!(0xF1, Single(INT1), None, None),
    op!(0xF4, Single(HLT), None, None),
    op!(0xF5, Single(CMC), None, None),
    op!(0xF6, Single(IDIV), Reg8, AL),  // group_4
    op!(0xF7, Single(IDIV), Reg16, AX), // group_4
    op!(0xF8, Single(CLC), None, None),
    op!(0xF9, Single(STC), None, None),
    op!(0xFA, Single(CLI), None, None),
    op!(0xFB, Single(STI), None, None),
    op!(0xFC, Single(CLD), None, None),
    op!(0xFD, Single(STD), None, None),
    op!(0xFE, ModrmReg(&[INC, DEC]), Reg8, None),
    op!(
        0xFF,
        ModrmReg(&[INC, DEC, CALL, CALL, JMP, JMP, PUSH, PUSH]),
        Reg16,
        None
    ),
];
