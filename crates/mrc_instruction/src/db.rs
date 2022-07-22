use crate::Operation;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum OperandEncoding {
    None,

    Imm,
    Imm8,
    Imm16,
    Sbw,   // Signed byte word
    Sbw8,  // Signed byte word
    Sbw16, // Signed byte word

    RegAl,
    RegAx,
    RegCl,
    RegCx,
    RegDx,

    Reg8,
    Reg16,

    SegEs,
    SegCs,
    SegSs,
    SegDs,
    Seg,

    Mem,
    Mem8,
    Mem16,

    SegOff,

    RegMem8,
    RegMem16,

    Disp8,
    Disp16,
}

#[derive(Debug)]
pub enum Code {
    Byte(u8),
    Imm8,
    SignImm8,
    Imm16,
    Disp8,
    Disp16,
    SegOff,
    PlusReg(u8),
    ModRegRM,
    ModRM(u8),
    Addr,
}

#[derive(Debug)]
pub struct InstructionData {
    pub operation: Operation,
    pub destination: OperandEncoding,
    pub source: OperandEncoding,
    pub codes: &'static [Code],
}

mod private {
    use super::*;
    use Code::*;

    macro_rules! id {
        ($operation:ident, $destination:ident, $source:ident, $codes:expr) => {{
            InstructionData {
                operation: Operation::$operation,
                destination: OperandEncoding::$destination,
                source: OperandEncoding::$source,
                codes: $codes,
            }
        }};
    }

    pub const INSTRUCTIONS: &[InstructionData] = &[
        id!(AAA, None, None, &[Byte(0x37)]),             // 8086, NOLONG
        id!(AAD, None, None, &[Byte(0xd5), Byte(0x0a)]), // 8086, NOLONG
        id!(AAD, Imm, None, &[Byte(0xd5), Imm8]),        // 8086, SB, NOLONG
        id!(AAM, None, None, &[Byte(0xd4), Byte(0x0a)]), // 8086, NOLONG
        id!(AAM, Imm, None, &[Byte(0xd4), Imm8]),        // 8086, SB, NOLONG
        id!(AAS, None, None, &[Byte(0x3f)]),             // 8086, NOLONG
        id!(ADC, Mem, Reg8, &[Byte(0x10), ModRegRM]),    // 8086, SM, LOCK
        id!(ADC, Reg8, Reg8, &[Byte(0x10), ModRegRM]),   // 8086
        id!(ADC, Mem, Reg16, &[Byte(0x11), ModRegRM]),   // 8086, SM, LOCK
        id!(ADC, Reg16, Reg16, &[Byte(0x11), ModRegRM]), // 8086
        id!(ADC, Reg8, Mem, &[Byte(0x12), ModRegRM]),    // 8086, SM
        id!(ADC, Reg8, Reg8, &[Byte(0x12), ModRegRM]),   // 8086
        id!(ADC, Reg16, Mem, &[Byte(0x13), ModRegRM]),   // 8086, SM
        id!(ADC, Reg16, Reg16, &[Byte(0x13), ModRegRM]), // 8086
        id!(ADC, RegMem16, Imm8, &[Byte(0x83), ModRM(2), SignImm8]), // 8086, LOCK
        id!(ADC, RegAl, Imm, &[Byte(0x14), Imm8]),       // 8086, SM
        id!(ADC, RegAx, Sbw, &[Byte(0x83), ModRM(2), SignImm8]), // 8086, SM, ND
        id!(ADC, RegAx, Imm, &[Byte(0x15), Imm16]),      // 8086, SM
        id!(ADC, RegMem8, Imm, &[Byte(0x80), ModRM(2), Imm8]), // 8086, SM, LOCK
        id!(ADC, RegMem16, Sbw, &[Byte(0x83), ModRM(2), SignImm8]), // 8086, SM, LOCK, ND
        id!(ADC, RegMem16, Imm, &[Byte(0x81), ModRM(2), Imm16]), // 8086, SM, LOCK
        id!(ADC, Mem, Imm8, &[Byte(0x80), ModRM(2), Imm8]), // 8086, SM, LOCK, ND
        id!(ADC, Mem, Sbw16, &[Byte(0x83), ModRM(2), SignImm8]), // 8086, SM, LOCK, ND
        id!(ADC, Mem, Imm16, &[Byte(0x81), ModRM(2), Imm16]), // 8086, SM, LOCK
        id!(ADC, RegMem8, Imm, &[Byte(0x82), ModRM(2), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(ADD, Mem, Reg8, &[Byte(0x00), ModRegRM]),    // 8086, SM, LOCK
        id!(ADD, Reg8, Reg8, &[Byte(0x00), ModRegRM]),   // 8086
        id!(ADD, Mem, Reg16, &[Byte(0x01), ModRegRM]),   // 8086, SM, LOCK
        id!(ADD, Reg16, Reg16, &[Byte(0x01), ModRegRM]), // 8086
        id!(ADD, Reg8, Mem, &[Byte(0x02), ModRegRM]),    // 8086, SM
        id!(ADD, Reg8, Reg8, &[Byte(0x02), ModRegRM]),   // 8086
        id!(ADD, Reg16, Mem, &[Byte(0x03), ModRegRM]),   // 8086, SM
        id!(ADD, Reg16, Reg16, &[Byte(0x03), ModRegRM]), // 8086
        id!(ADD, RegMem16, Imm8, &[Byte(0x83), ModRM(0), SignImm8]), // 8086, LOCK
        id!(ADD, RegAl, Imm, &[Byte(0x04), Imm8]),       // 8086, SM
        id!(ADD, RegAx, Sbw, &[Byte(0x83), ModRM(0), SignImm8]), // 8086, SM, ND
        id!(ADD, RegAx, Imm, &[Byte(0x05), Imm16]),      // 8086, SM
        id!(ADD, RegMem8, Imm, &[Byte(0x80), ModRM(0), Imm8]), // 8086, SM, LOCK
        id!(ADD, RegMem16, Sbw, &[Byte(0x83), ModRM(0), SignImm8]), // 8086, SM, LOCK, ND
        id!(ADD, RegMem16, Imm, &[Byte(0x81), ModRM(0), Imm16]), // 8086, SM, LOCK
        id!(ADD, Mem, Imm8, &[Byte(0x80), ModRM(0), Imm8]), // 8086, SM, LOCK
        id!(ADD, Mem, Sbw16, &[Byte(0x83), ModRM(0), SignImm8]), // 8086, SM, LOCK, ND
        id!(ADD, Mem, Imm16, &[Byte(0x81), ModRM(0), Imm16]), // 8086, SM, LOCK
        id!(ADD, RegMem8, Imm, &[Byte(0x82), ModRM(0), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(AND, Mem, Reg8, &[Byte(0x20), ModRegRM]),    // 8086, SM, LOCK
        id!(AND, Reg8, Reg8, &[Byte(0x20), ModRegRM]),   // 8086
        id!(AND, Mem, Reg16, &[Byte(0x21), ModRegRM]),   // 8086, SM, LOCK
        id!(AND, Reg16, Reg16, &[Byte(0x21), ModRegRM]), // 8086
        id!(AND, Reg8, Mem, &[Byte(0x22), ModRegRM]),    // 8086, SM
        id!(AND, Reg8, Reg8, &[Byte(0x22), ModRegRM]),   // 8086
        id!(AND, Reg16, Mem, &[Byte(0x23), ModRegRM]),   // 8086, SM
        id!(AND, Reg16, Reg16, &[Byte(0x23), ModRegRM]), // 8086
        id!(AND, RegMem16, Imm8, &[Byte(0x83), ModRM(4), SignImm8]), // 8086, LOCK
        id!(AND, RegAl, Imm, &[Byte(0x24), Imm8]),       // 8086, SM
        id!(AND, RegAx, Sbw, &[Byte(0x83), ModRM(4), SignImm8]), // 8086, SM, ND
        id!(AND, RegAx, Imm, &[Byte(0x25), Imm16]),      // 8086, SM
        id!(AND, RegMem8, Imm, &[Byte(0x80), ModRM(4), Imm8]), // 8086, SM, LOCK
        id!(AND, RegMem16, Sbw, &[Byte(0x83), ModRM(4), SignImm8]), // 8086, SM, LOCK, ND
        id!(AND, RegMem16, Imm, &[Byte(0x81), ModRM(4), Imm16]), // 8086, SM, LOCK
        id!(AND, Mem, Imm8, &[Byte(0x80), ModRM(4), Imm8]), // 8086, SM, LOCK
        id!(AND, Mem, Sbw16, &[Byte(0x83), ModRM(4), SignImm8]), // 8086, SM, LOCK, ND
        id!(AND, Mem, Imm16, &[Byte(0x81), ModRM(4), Imm16]), // 8086, SM, LOCK
        id!(AND, RegMem8, Imm, &[Byte(0x82), ModRM(4), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(CALL, Imm16, None, &[Byte(0xe8), Disp16]),   // 8086, BND
        id!(CALL, SegOff, None, &[Byte(0x9a), SegOff]),  // 8086, ND, NOLONG  |far
        id!(CALL, Mem, None, &[Byte(0xff), ModRM(3)]),   // 8086, NOLONG |far
        id!(CALL, Mem16, None, &[Byte(0xff), ModRM(3)]), // 8086  |far
        id!(CALL, Mem, None, &[Byte(0xff), ModRM(2)]),   // 8086, ND, BND |near
        id!(CALL, RegMem16, None, &[Byte(0xff), ModRM(2)]), // 8086, NOLONG, ND, BND |near
        id!(CALL, Mem, None, &[Byte(0xff), ModRM(2)]),   // 8086, BND
        id!(CALL, RegMem16, None, &[Byte(0xff), ModRM(2)]), // 8086, NOLONG, BND
        id!(CBW, None, None, &[Byte(0x98)]),             // 8086
        id!(CLC, None, None, &[Byte(0xf8)]),             // 8086
        id!(CLD, None, None, &[Byte(0xfc)]),             // 8086
        id!(CLI, None, None, &[Byte(0xfa)]),             // 8086
        id!(CMC, None, None, &[Byte(0xf5)]),             // 8086
        id!(CMP, Mem, Reg8, &[Byte(0x38), ModRegRM]),    // 8086, SM
        id!(CMP, Reg8, Reg8, &[Byte(0x38), ModRegRM]),   // 8086
        id!(CMP, Mem, Reg16, &[Byte(0x39), ModRegRM]),   // 8086, SM
        id!(CMP, Reg16, Reg16, &[Byte(0x39), ModRegRM]), // 8086
        id!(CMP, Reg8, Mem, &[Byte(0x3a), ModRegRM]),    // 8086, SM
        id!(CMP, Reg8, Reg8, &[Byte(0x3a), ModRegRM]),   // 8086
        id!(CMP, Reg16, Mem, &[Byte(0x3b), ModRegRM]),   // 8086, SM
        id!(CMP, Reg16, Reg16, &[Byte(0x3b), ModRegRM]), // 8086
        id!(CMP, RegMem16, Imm8, &[Byte(0x83), ModRM(7), SignImm8]), // 8086
        id!(CMP, RegAl, Imm, &[Byte(0x3c), Imm8]),       // 8086, SM
        id!(CMP, RegAx, Sbw, &[Byte(0x83), ModRM(7), SignImm8]), // 8086, SM, ND
        id!(CMP, RegAx, Imm, &[Byte(0x3d), Imm16]),      // 8086, SM
        id!(CMP, RegMem8, Imm, &[Byte(0x80), ModRM(7), Imm8]), // 8086, SM
        id!(CMP, RegMem16, Sbw, &[Byte(0x83), ModRM(7), SignImm8]), // 8086, SM, ND
        id!(CMP, RegMem16, Imm, &[Byte(0x81), ModRM(7), Imm16]), // 8086, SM
        id!(CMP, Mem, Imm8, &[Byte(0x80), ModRM(7), Imm8]), // 8086, SM
        id!(CMP, Mem, Sbw16, &[Byte(0x83), ModRM(7), SignImm8]), // 8086, SM, ND
        id!(CMP, Mem, Imm16, &[Byte(0x81), ModRM(7), Imm16]), // 8086, SM
        id!(CMP, RegMem8, Imm, &[Byte(0x82), ModRM(7), Imm8]), // 8086, SM, ND, NOLONG
        id!(CMPSB, None, None, &[Byte(0xa6)]),           // 8086
        id!(CMPSW, None, None, &[Byte(0xa7)]),           // 8086
        id!(CWD, None, None, &[Byte(0x99)]),             // 8086
        id!(DAA, None, None, &[Byte(0x27)]),             // 8086, NOLONG
        id!(DAS, None, None, &[Byte(0x2f)]),             // 8086, NOLONG
        id!(DEC, Reg16, None, &[PlusReg(0x48)]),         // 8086, NOLONG
        id!(DEC, RegMem8, None, &[Byte(0xfe), ModRM(1)]), // 8086, LOCK
        id!(DEC, RegMem16, None, &[Byte(0xff), ModRM(1)]), // 8086, LOCK
        id!(DIV, RegMem8, None, &[Byte(0xf6), ModRM(6)]), // 8086
        id!(DIV, RegMem16, None, &[Byte(0xf7), ModRM(6)]), // 8086
        id!(HLT, None, None, &[Byte(0xf4)]),             // 8086, PRIV
        id!(IDIV, RegMem8, None, &[Byte(0xf6), ModRM(7)]), // 8086
        id!(IDIV, RegMem16, None, &[Byte(0xf7), ModRM(7)]), // 8086
        id!(IMUL, RegMem8, None, &[Byte(0xf6), ModRM(5)]), // 8086
        id!(IMUL, RegMem16, None, &[Byte(0xf7), ModRM(5)]), // 8086
        id!(IN, RegAl, Imm, &[Byte(0xe4), Imm8]),        // 8086, SB
        id!(IN, RegAx, Imm, &[Byte(0xe5), Imm8]),        // 8086, SB
        id!(IN, RegAl, RegDx, &[Byte(0xec)]),            // 8086
        id!(IN, RegAx, RegDx, &[Byte(0xed)]),            // 8086
        id!(INC, Reg16, None, &[PlusReg(0x40)]),         // 8086, NOLONG
        id!(INC, RegMem8, None, &[Byte(0xfe), ModRM(0)]), // 8086, LOCK
        id!(INC, RegMem16, None, &[Byte(0xff), ModRM(0)]), // 8086, LOCK
        id!(INT, Imm8, None, &[Byte(0xcd), Imm8]),       // 8086, SB
        id!(INT3, None, None, &[Byte(0xcc)]),            // 8086
        id!(INTO, None, None, &[Byte(0xce)]),            // 8086, NOLONG
        id!(IRET, None, None, &[Byte(0xcf)]),            // 8086
        // id!(IRETW, None, None, &[Byte(0xcf), ]), // 8086
        id!(JCXZ, Imm, None, &[Byte(0xe3), Disp8]), // 8086, NOLONG
        id!(JMP, Imm8, None, &[Byte(0xeb), Disp8]),
        id!(JMP, Imm16, None, &[Byte(0xe9), Disp16]),
        id!(JMP, SegOff, None, &[Byte(0xea), SegOff]), // 8086, ND, NOLONG |far
        id!(JMP, Mem, None, &[Byte(0xff), ModRM(5)]),  // 8086, NOLONG |far
        id!(JMP, Mem16, None, &[Byte(0xff), ModRM(5)]), // 8086  |far
        id!(JMP, Mem, None, &[Byte(0xff), ModRM(4)]),  // 8086, ND, BND |near
        id!(JMP, RegMem16, None, &[Byte(0xff), ModRM(4)]), // 8086, NOLONG, ND, BND |near
        id!(JMP, Mem, None, &[Byte(0xff), ModRM(4)]),  // 8086, BND
        id!(JMP, RegMem16, None, &[Byte(0xff), ModRM(4)]), // 8086, NOLONG, BND
        id!(LAHF, None, None, &[Byte(0x9f)]),          // 8086
        id!(LDS, Reg16, Mem, &[Byte(0xc5), ModRegRM]), // 8086, NOLONG
        id!(LEA, Reg16, Mem, &[Byte(0x8d), ModRegRM]), // 8086, ANYSIZE
        id!(LEA, Reg16, Imm, &[Byte(0x8d), ModRegRM]), // 8086, ND, ANYSIZE
        id!(LES, Reg16, Mem, &[Byte(0xc4), ModRegRM]), // 8086, NOLONG
        id!(LODSB, None, None, &[Byte(0xac)]),         // 8086
        id!(LODSW, None, None, &[Byte(0xad)]),         // 8086
        id!(LOOP, Disp8, None, &[Byte(0xe2), Disp8]),  // 8086
        id!(LOOP, Disp8, RegCx, &[Byte(0xe2), Disp8]), // 8086, NOLONG
        id!(LOOPZ, Disp8, None, &[Byte(0xe1), Disp8]), // 8086
        id!(LOOPZ, Disp8, RegCx, &[Byte(0xe1), Disp8]), // 8086, NOLONG
        id!(LOOPNZ, Disp8, None, &[Byte(0xe0), Disp8]), // 8086
        id!(LOOPNZ, Disp8, RegCx, &[Byte(0xe0), Disp8]), // 8086, NOLONG
        id!(MOV, Mem, Seg, &[Byte(0x8c), ModRegRM]),   // 8086, SW
        id!(MOV, Reg16, Seg, &[Byte(0x8c), ModRegRM]), // 8086
        id!(MOV, Seg, Mem, &[Byte(0x8e), ModRegRM]),   // 8086, SW
        id!(MOV, Seg, Reg16, &[Byte(0x8e), ModRegRM]), // 8086
        id!(MOV, RegAl, SegOff, &[Byte(0xa0), SegOff]), // 8086, SM
        id!(MOV, RegAx, SegOff, &[Byte(0xa1), SegOff]), // 8086, SM
        id!(MOV, SegOff, RegAl, &[Byte(0xa2), SegOff]), // 8086, SM, NOHLE
        id!(MOV, SegOff, RegAx, &[Byte(0xa3), SegOff]), // 8086, SM, NOHLE
        id!(MOV, Mem, Reg8, &[Byte(0x88), ModRegRM]),  // 8086, SM
        id!(MOV, Reg8, Reg8, &[Byte(0x88), ModRegRM]), // 8086
        id!(MOV, Mem, Reg16, &[Byte(0x89), ModRegRM]), // 8086, SM
        id!(MOV, Reg16, Reg16, &[Byte(0x89), ModRegRM]), // 8086
        id!(MOV, Reg8, Mem, &[Byte(0x8a), ModRegRM]),  // 8086, SM
        id!(MOV, Reg8, Reg8, &[Byte(0x8a), ModRegRM]), // 8086
        id!(MOV, Reg16, Mem, &[Byte(0x8b), ModRegRM]), // 8086, SM
        id!(MOV, Reg16, Reg16, &[Byte(0x8b), ModRegRM]), // 8086
        id!(MOV, Reg8, Imm, &[PlusReg(0xb0), Imm8]),   // 8086, SM
        id!(MOV, Reg16, Imm, &[PlusReg(0xb8), Imm16]), // 8086, SM
        id!(MOV, RegMem8, Imm, &[Byte(0xc6), ModRM(0), Imm8]), // 8086, SM
        id!(MOV, RegMem16, Imm, &[Byte(0xc7), ModRM(0), Imm16]), // 8086, SM
        id!(MOV, Mem, Imm8, &[Byte(0xc6), ModRM(0), Imm8]), // 8086, SM
        id!(MOV, Mem, Imm16, &[Byte(0xc7), ModRM(0), Imm16]), // 8086, SM
        id!(MOVSB, None, None, &[Byte(0xa4)]),         // 8086
        id!(MOVSW, None, None, &[Byte(0xa5)]),         // 8086
        id!(MUL, RegMem8, None, &[Byte(0xf6), ModRM(4)]), // 8086
        id!(MUL, RegMem16, None, &[Byte(0xf7), ModRM(4)]), // 8086
        id!(NEG, RegMem8, None, &[Byte(0xf6), ModRM(3)]), // 8086, LOCK
        id!(NEG, RegMem16, None, &[Byte(0xf7), ModRM(3)]), // 8086, LOCK
        id!(NOP, None, None, &[Byte(0x90)]),           // 8086
        id!(NOT, RegMem8, None, &[Byte(0xf6), ModRM(2)]), // 8086, LOCK
        id!(NOT, RegMem16, None, &[Byte(0xf7), ModRM(2)]), // 8086, LOCK
        id!(OR, Mem, Reg8, &[Byte(0x08), ModRegRM]),   // 8086, SM, LOCK
        id!(OR, Reg8, Reg8, &[Byte(0x08), ModRegRM]),  // 8086
        id!(OR, Mem, Reg16, &[Byte(0x09), ModRegRM]),  // 8086, SM, LOCK
        id!(OR, Reg16, Reg16, &[Byte(0x09), ModRegRM]), // 8086
        id!(OR, Reg8, Mem, &[Byte(0x0a), ModRegRM]),   // 8086, SM
        id!(OR, Reg8, Reg8, &[Byte(0x0a), ModRegRM]),  // 8086
        id!(OR, Reg16, Mem, &[Byte(0x0b), ModRegRM]),  // 8086, SM
        id!(OR, Reg16, Reg16, &[Byte(0x0b), ModRegRM]), // 8086
        id!(OR, RegMem16, Imm8, &[Byte(0x83), ModRM(1), SignImm8]), // 8086, LOCK
        id!(OR, RegAl, Imm, &[Byte(0x0c), Imm8]),      // 8086, SM
        id!(OR, RegAx, Sbw, &[Byte(0x83), ModRM(1), SignImm8]), // 8086, SM, ND
        id!(OR, RegAx, Imm, &[Byte(0x0d), Imm16]),     // 8086, SM
        id!(OR, RegMem8, Imm, &[Byte(0x80), ModRM(1), Imm8]), // 8086, SM, LOCK
        id!(OR, RegMem16, Sbw, &[Byte(0x83), ModRM(1), SignImm8]), // 8086, SM, LOCK, ND
        id!(OR, RegMem16, Imm, &[Byte(0x81), ModRM(1), Imm16]), // 8086, SM, LOCK
        id!(OR, Mem, Imm8, &[Byte(0x80), ModRM(1), Imm8]), // 8086, SM, LOCK
        id!(OR, Mem, Sbw16, &[Byte(0x83), ModRM(1), SignImm8]), // 8086, SM, LOCK, ND
        id!(OR, Mem, Imm16, &[Byte(0x81), ModRM(1), Imm16]), // 8086, SM, LOCK
        id!(OR, RegMem8, Imm, &[Byte(0x82), ModRM(1), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(OUT, Imm, RegAl, &[Byte(0xe6), Imm8]),     // 8086, SB
        id!(OUT, Imm, RegAx, &[Byte(0xe7), Imm8]),     // 8086, SB
        id!(OUT, RegDx, RegAl, &[Byte(0xee)]),         // 8086
        id!(OUT, RegDx, RegAx, &[Byte(0xef)]),         // 8086
        // id!(PAUSE, None, None, &[Byte(0x90), ]), // 8086
        id!(POP, Reg16, None, &[PlusReg(0x58)]), // 8086
        id!(POP, RegMem16, None, &[Byte(0x8f), ModRM(0)]), // 8086
        id!(POP, SegEs, None, &[Byte(0x07)]),    // 8086, NOLONG
        id!(POP, SegCs, None, &[Byte(0x0f)]),    // 8086, UNDOC, ND, OBSOLETE
        id!(POP, SegSs, None, &[Byte(0x17)]),    // 8086, NOLONG
        id!(POP, SegDs, None, &[Byte(0x1f)]),    // 8086, NOLONG
        id!(POPF, None, None, &[Byte(0x9d)]),    // 8086
        // id!(POPFW, None, None, &[9d]), // 8086
        id!(PUSH, Reg16, None, &[PlusReg(0x50)]), // 8086
        id!(PUSH, RegMem16, None, &[Byte(0xff), ModRM(6)]), // 8086
        id!(PUSH, SegEs, None, &[Byte(0x06)]),    // 8086, NOLONG
        id!(PUSH, SegCs, None, &[Byte(0x0e)]),    // 8086, NOLONG
        id!(PUSH, SegSs, None, &[Byte(0x16)]),    // 8086, NOLONG
        id!(PUSH, SegDs, None, &[Byte(0x1e)]),    // 8086, NOLONG
        id!(PUSHF, None, None, &[Byte(0x9c)]),    // 8086
        // id!(PUSHFW, None, None, &[Byte(0x9c), ]), // 8086
        // id!(RCL, RegMem8, unity, &[Byte(0xd0), ModRM(2)]), // 8086
        id!(RCL, RegMem8, RegCl, &[Byte(0xd2), ModRM(2)]), // 8086
        // id!(RCL, RegMem16, unity, &[Byte(0xd1), ModRM(2)]), // 8086
        id!(RCL, RegMem16, RegCl, &[Byte(0xd3), ModRM(2)]), // 8086
        // id!(RCR, RegMem8, unity, &[Byte(0xd0), ModRM(3)]), // 8086
        id!(RCR, RegMem8, RegCl, &[Byte(0xd2), ModRM(3)]), // 8086
        // id!(RCR, RegMem16, unity, &[Byte(0xd1), ModRM(3)]), // 8086
        id!(RCR, RegMem16, RegCl, &[Byte(0xd3), ModRM(3)]), // 8086
        id!(RET, None, None, &[Byte(0xc3)]),                // 8086, BND
        id!(RET, Imm, None, &[Byte(0xc2), Imm16]),          // 8086, SW, BND
        // id!(RETF, None, None, &[Byte(0xcb), ]), // 8086
        // id!(RETF, Imm, None, &[Byte(0xca), ImmediateWord]), // 8086, SW
        // id!(RETN, None, None, &[Byte(0xc3), ]), // 8086, BND
        // id!(RETN, Imm, None, &[Byte(0xc2), ImmediateWord]), // 8086, SW, BND
        // id!(RETW, None, None, &[Byte(0xc3), ]), // 8086, BND
        // id!(RETW, Imm, None, &[Byte(0xc2), ImmediateWord]), // 8086, SW, BND
        // id!(RETFW, None, None, &[Byte(0xcb), ]), // 8086
        // id!(RETFW, Imm, None, &[Byte(0xca), ImmediateWord]), // 8086, SW
        // id!(RETNW, None, None, &[Byte(0xc3), ]), // 8086, BND
        // id!(RETNW, Imm, None, &[Byte(0xc2), ImmediateWord]), // 8086, SW, BND
        // id!(RETD, None, None, &[o32 c3]), // 8086, BND, NOLONG
        // id!(RETD, Imm, None, &[o32 c2 ImmediateWord]), // 8086, SW, BND, NOLONG
        // id!(RETFD, None, None, &[o32 cb]), // 8086
        // id!(RETFD, Imm, None, &[o32 ca ImmediateWord]), // 8086, SW
        // id!(RETND, None, None, &[o32 c3]), // 8086, BND, NOLONG
        // id!(RETND, Imm, None, &[o32 c2 ImmediateWord]), // 8086, SW, BND, NOLONG
        // id!(ROL, RegMem8, unity, &[d0 ModRM(0)]), // 8086
        id!(ROL, RegMem8, RegCl, &[Byte(0xd2), ModRM(0)]), // 8086
        // id!(ROL, RegMem16, unity, &[Byte(0xd1), ModRM(0)]), // 8086
        id!(ROL, RegMem16, RegCl, &[Byte(0xd3), ModRM(0)]), // 8086
        // id!(ROR, RegMem8, unity, &[Byte(0xd0), ModRM(1)]), // 8086
        id!(ROR, RegMem8, RegCl, &[Byte(0xd2), ModRM(1)]), // 8086
        // id!(ROR, RegMem16, unity, &[Byte(0xd1), ModRM(1)]), // 8086
        id!(ROR, RegMem16, RegCl, &[Byte(0xd3), ModRM(1)]), // 8086
        id!(SAHF, None, None, &[Byte(0x9e)]),               // 8086
        // id!(SAL, RegMem8, unity, &[Byte(0xd0), ModRM(4)]), // 8086, ND
        // id!(SAL, RegMem8, RegCl, &[Byte(0xd2), ModRM(4)]), // 8086, ND
        // id!(SAL, RegMem16, unity, &[Byte(0xd1), ModRM(4)]), // 8086, ND
        // id!(SAL, RegMem16, RegCl, &[Byte(0xd3), ModRM(4)]), // 8086, ND
        id!(SALC, None, None, &[Byte(0xd6)]), // 8086, UNDOC
        // id!(SAR, RegMem8, unity, &[Byte(0xd0), ModRM(7)]), // 8086
        id!(SAR, RegMem8, RegCl, &[Byte(0xd2), ModRM(7)]), // 8086
        // id!(SAR, RegMem16, unity, &[Byte(0xd1), ModRM(7)]), // 8086
        id!(SAR, RegMem16, RegCl, &[Byte(0xd3), ModRM(7)]), // 8086
        id!(SBB, Mem, Reg8, &[Byte(0x18), ModRegRM]),       // 8086, SM, LOCK
        id!(SBB, Reg8, Reg8, &[Byte(0x18), ModRegRM]),      // 8086
        id!(SBB, Mem, Reg16, &[Byte(0x19), ModRegRM]),      // 8086, SM, LOCK
        id!(SBB, Reg16, Reg16, &[Byte(0x19), ModRegRM]),    // 8086
        id!(SBB, Reg8, Mem, &[Byte(0x1a), ModRegRM]),       // 8086, SM
        id!(SBB, Reg8, Reg8, &[Byte(0x1a), ModRegRM]),      // 8086
        id!(SBB, Reg16, Mem, &[Byte(0x1b), ModRegRM]),      // 8086, SM
        id!(SBB, Reg16, Reg16, &[Byte(0x1b), ModRegRM]),    // 8086
        id!(SBB, RegMem16, Imm8, &[Byte(0x83), ModRM(3), SignImm8]), // 8086, LOCK
        id!(SBB, RegAl, Imm, &[Byte(0x1c), Imm8]),          // 8086, SM
        id!(SBB, RegAx, Sbw, &[Byte(0x83), ModRM(3), SignImm8]), // 8086, SM, ND
        id!(SBB, RegAx, Imm, &[Byte(0x1d), Imm16]),         // 8086, SM
        id!(SBB, RegMem8, Imm, &[Byte(0x80), ModRM(3), Imm8]), // 8086, SM, LOCK
        id!(SBB, RegMem16, Sbw, &[Byte(0x83), ModRM(3), SignImm8]), // 8086, SM, LOCK, ND
        id!(SBB, RegMem16, Imm, &[Byte(0x81), ModRM(3), Imm16]), // 8086, SM, LOCK
        id!(SBB, Mem, Imm8, &[Byte(0x80), ModRM(3), Imm8]), // 8086, SM, LOCK
        id!(SBB, Mem, Sbw16, &[Byte(0x83), ModRM(3), SignImm8]), // 8086, SM, LOCK, ND
        id!(SBB, Mem, Imm16, &[Byte(0x81), ModRM(3), Imm16]), // 8086, SM, LOCK
        id!(SBB, RegMem8, Imm, &[Byte(0x82), ModRM(3), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(SCASB, None, None, &[Byte(0xae)]),              // 8086
        id!(SCASW, None, None, &[Byte(0xaf)]),              // 8086
        id!(SHL, RegMem8, Imm8, &[Byte(0xd0), ModRM(4)]),   // 8086 | unity?
        id!(SHL, RegMem8, RegCl, &[Byte(0xd2), ModRM(4)]),  // 8086
        id!(SHL, RegMem16, Imm8, &[Byte(0xd1), ModRM(4)]),  // 8086 | unity?
        id!(SHL, RegMem16, RegCl, &[Byte(0xd3), ModRM(4)]), // 8086
        id!(SHR, RegMem8, Imm8, &[Byte(0xd0), ModRM(5)]),   // 8086 | unity?
        id!(SHR, RegMem8, RegCl, &[Byte(0xd2), ModRM(5)]),  // 8086
        id!(SHR, RegMem16, Imm8, &[Byte(0xd1), ModRM(5)]),  // 8086 | unity?
        id!(SHR, RegMem16, RegCl, &[Byte(0xd3), ModRM(5)]), // 8086
        id!(STC, None, None, &[Byte(0xf9)]),                // 8086
        id!(STD, None, None, &[Byte(0xfd)]),                // 8086
        id!(STI, None, None, &[Byte(0xfb)]),                // 8086
        id!(STOSB, None, None, &[Byte(0xaa)]),              // 8086
        id!(STOSW, None, None, &[Byte(0xab)]),              // 8086
        id!(SUB, Mem, Reg8, &[Byte(0x28), ModRegRM]),       // 8086, SM, LOCK
        id!(SUB, Reg8, Reg8, &[Byte(0x28), ModRegRM]),      // 8086
        id!(SUB, Mem, Reg16, &[Byte(0x29), ModRegRM]),      // 8086, SM, LOCK
        id!(SUB, Reg16, Reg16, &[Byte(0x29), ModRegRM]),    // 8086
        id!(SUB, Reg8, Mem, &[Byte(0x2a), ModRegRM]),       // 8086, SM
        id!(SUB, Reg8, Reg8, &[Byte(0x2a), ModRegRM]),      // 8086
        id!(SUB, Reg16, Mem, &[Byte(0x2b), ModRegRM]),      // 8086, SM
        id!(SUB, Reg16, Reg16, &[Byte(0x2b), ModRegRM]),    // 8086
        id!(SUB, RegMem16, Imm8, &[Byte(0x83), ModRM(5), SignImm8]), // 8086, LOCK
        id!(SUB, RegAl, Imm, &[Byte(0x2c), Imm8]),          // 8086, SM
        id!(SUB, RegAx, Sbw, &[Byte(0x83), ModRM(5), SignImm8]), // 8086, SM, ND
        id!(SUB, RegAx, Imm, &[Byte(0x2d), Imm16]),         // 8086, SM
        id!(SUB, RegMem8, Imm, &[Byte(0x80), ModRM(5), Imm8]), // 8086, SM, LOCK
        id!(SUB, RegMem16, Sbw, &[Byte(0x83), ModRM(5), SignImm8]), // 8086, SM, LOCK, ND
        id!(SUB, RegMem16, Imm, &[Byte(0x81), ModRM(5), Imm16]), // 8086, SM, LOCK
        id!(SUB, Mem, Imm8, &[Byte(0x80), ModRM(5), Imm8]), // 8086, SM, LOCK
        id!(SUB, Mem, Sbw16, &[Byte(0x83), ModRM(5), SignImm8]), // 8086, SM, LOCK, ND
        id!(SUB, Mem, Imm16, &[Byte(0x81), ModRM(5), Imm16]), // 8086, SM, LOCK
        id!(SUB, RegMem8, Imm, &[Byte(0x82), ModRM(5), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        id!(TEST, Mem, Reg8, &[Byte(0x84), ModRegRM]),      // 8086, SM
        id!(TEST, Reg8, Reg8, &[Byte(0x84), ModRegRM]),     // 8086
        id!(TEST, Mem, Reg16, &[Byte(0x85), ModRegRM]),     // 8086, SM
        id!(TEST, Reg16, Reg16, &[Byte(0x85), ModRegRM]),   // 8086
        id!(TEST, Reg8, Mem, &[Byte(0x84), ModRegRM]),      // 8086, SM
        id!(TEST, Reg16, Mem, &[Byte(0x85), ModRegRM]),     // 8086, SM
        id!(TEST, RegAl, Imm, &[Byte(0xa8), Imm8]),         // 8086, SM
        id!(TEST, RegAx, Imm, &[Byte(0xa9), Imm16]),        // 8086, SM
        id!(TEST, RegMem8, Imm, &[Byte(0xf6), ModRM(0), Imm8]), // 8086, SM
        id!(TEST, RegMem16, Imm, &[Byte(0xf7), ModRM(0), Imm16]), // 8086, SM
        id!(TEST, Mem, Imm8, &[Byte(0xf6), ModRM(0), Imm8]), // 8086, SM
        id!(TEST, Mem, Imm16, &[Byte(0xf7), ModRM(0), Imm16]), // 8086, SM
        // id!(FWAIT, None, None, &[wait]), // 8086
        id!(XCHG, RegAx, Reg16, &[PlusReg(0x90)]), // 8086
        id!(XCHG, Reg16, RegAx, &[PlusReg(0x90)]), // 8086
        id!(XCHG, Reg8, Mem, &[Byte(0x86), ModRegRM]), // 8086, SM, LOCK
        id!(XCHG, Reg8, Reg8, &[Byte(0x86), ModRegRM]), // 8086
        id!(XCHG, Reg16, Mem, &[Byte(0x87), ModRegRM]), // 8086, SM, LOCK
        id!(XCHG, Reg16, Reg16, &[Byte(0x87), ModRegRM]), // 8086
        id!(XCHG, Mem, Reg8, &[Byte(0x86), ModRegRM]), // 8086, SM, LOCK
        id!(XCHG, Reg8, Reg8, &[Byte(0x86), ModRegRM]), // 8086
        id!(XCHG, Mem, Reg16, &[Byte(0x87), ModRegRM]), // 8086, SM, LOCK
        id!(XCHG, Reg16, Reg16, &[Byte(0x87), ModRegRM]), // 8086
        // id!(XLATB, None, None, &[Byte(0xd7)]), // 8086
        id!(XLAT, None, None, &[Byte(0xd7)]),            // 8086
        id!(XOR, Mem, Reg8, &[Byte(0x30), ModRegRM]),    // 8086, SM, LOCK
        id!(XOR, Reg8, Reg8, &[Byte(0x30), ModRegRM]),   // 8086
        id!(XOR, Mem, Reg16, &[Byte(0x31), ModRegRM]),   // 8086, SM, LOCK
        id!(XOR, Reg16, Reg16, &[Byte(0x31), ModRegRM]), // 8086
        id!(XOR, Reg8, Mem, &[Byte(0x32), ModRegRM]),    // 8086, SM
        id!(XOR, Reg8, Reg8, &[Byte(0x32), ModRegRM]),   // 8086
        id!(XOR, Reg16, Mem, &[Byte(0x33), ModRegRM]),   // 8086, SM
        id!(XOR, Reg16, Reg16, &[Byte(0x33), ModRegRM]), // 8086
        id!(XOR, RegMem16, Imm8, &[Byte(0x83), ModRM(6), SignImm8]), // 8086, LOCK
        id!(XOR, RegAl, Imm, &[Byte(0x34), Imm8]),       // 8086, SM
        id!(XOR, RegAx, Sbw, &[Byte(0x83), ModRM(6), SignImm8]), // 8086, SM, ND
        id!(XOR, RegAx, Imm, &[Byte(0x35), Imm16]),      // 8086, SM
        id!(XOR, RegMem8, Imm, &[Byte(0x80), ModRM(6), Imm8]), // 8086, SM, LOCK
        id!(XOR, RegMem16, Sbw, &[Byte(0x83), ModRM(6), SignImm8]), // 8086, SM, LOCK, ND
        id!(XOR, RegMem16, Imm, &[Byte(0x81), ModRM(6), Imm16]), // 8086, SM, LOCK
        id!(XOR, Mem, Imm8, &[Byte(0x80), ModRM(6), Imm8]), // 8086, SM, LOCK
        id!(XOR, Mem, Sbw16, &[Byte(0x83), ModRM(6), SignImm8]), // 8086, SM, LOCK, ND
        id!(XOR, Mem, Imm16, &[Byte(0x81), ModRM(6), Imm16]), // 8086, SM, LOCK
        id!(XOR, RegMem8, Imm, &[Byte(0x82), ModRM(6), Imm8]), // 8086, SM, LOCK, ND, NOLONG
        // Conditional jumps.
        id!(JCXZ, Disp8, None, &[Byte(0xE3), Disp8]),
        id!(JO, Disp8, None, &[Byte(0x70), Disp8]),
        id!(JNO, Disp8, None, &[Byte(0x71), Disp8]),
        id!(JB, Disp8, None, &[Byte(0x72), Disp8]),
        id!(JNB, Disp8, None, &[Byte(0x73), Disp8]),
        id!(JE, Disp8, None, &[Byte(0x74), Disp8]),
        id!(JNE, Disp8, None, &[Byte(0x75), Disp8]),
        id!(JBE, Disp8, None, &[Byte(0x76), Disp8]),
        id!(JNBE, Disp8, None, &[Byte(0x77), Disp8]),
        id!(JS, Disp8, None, &[Byte(0x78), Disp8]),
        id!(JNS, Disp8, None, &[Byte(0x79), Disp8]),
        id!(JP, Disp8, None, &[Byte(0x7A), Disp8]),
        id!(JNP, Disp8, None, &[Byte(0x7B), Disp8]),
        id!(JL, Disp8, None, &[Byte(0x7C), Disp8]),
        id!(JNL, Disp8, None, &[Byte(0x7D), Disp8]),
        id!(JLE, Disp8, None, &[Byte(0x7E), Disp8]),
        id!(JNLE, Disp8, None, &[Byte(0x7F), Disp8]),
    ];
}

pub use private::INSTRUCTIONS;
