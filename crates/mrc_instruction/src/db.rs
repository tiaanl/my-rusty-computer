#![allow(dead_code)]

use crate::Operation;

#[derive(Debug, Eq, PartialEq)]
pub enum OperandEncoding {
    None,

    Imm,
    Imm8,
    Imm16,
    SignedByteWord8,
    SignedByteWord16,

    RegAl,
    RegAx,

    Reg8,
    Reg16,

    Seg,

    Mem,
    Mem8,
    Mem16,

    RegMem8,
    RegMem16,

    Disp8,
    Disp16,
}

#[derive(Debug)]
pub enum Code {
    Byte(u8),
    ImmediateByte,
    ImmediateByteSigned,
    ImmediateWord,
    DisplacementByte,
    DisplacementWord,
    PlusReg(u8),
    ModRegRM,
    ModRM(u8),
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

    #[rustfmt::skip]
    pub const INSTRUCTIONS: &[InstructionData] = &[
        id!(NOP,               None,     None,             &[Byte(0x90)]),
    
        id!(MOV,               Mem16,    Seg,              &[Byte(0x8C),     ModRegRM]),
        id!(MOV,               Reg16,    Seg,              &[Byte(0x8C),     ModRegRM]),
        id!(MOV,               Seg,      Mem16,            &[Byte(0x8E),     ModRegRM]),
        id!(MOV,               Seg,      Reg16,            &[Byte(0x8E),     ModRegRM]),
        id!(MOV,               RegAl,    Mem,              &[Byte(0xA0),     ImmediateWord]),
        id!(MOV,               RegAx,    Mem,              &[Byte(0xA1),     ImmediateByte]),
        id!(MOV,               Mem,      RegAl,            &[Byte(0xA2),     ImmediateWord]),
        id!(MOV,               Mem,      RegAx,            &[Byte(0xA3),     ImmediateWord]),
        id!(MOV,               Mem,      Reg8,             &[Byte(0x88),     ModRegRM]),
        id!(MOV,               Reg8,     Reg8,             &[Byte(0x88),     ModRegRM]),
        id!(MOV,               Mem,      Reg16,            &[Byte(0x89),     ModRegRM]),
        id!(MOV,               Reg16,    Reg16,            &[Byte(0x89),     ModRegRM]),
        id!(MOV,               Reg8,     Mem,              &[Byte(0x8A),     ModRegRM]),
        id!(MOV,               Reg8,     Reg8,             &[Byte(0x8A),     ModRegRM]),
        id!(MOV,               Reg16,    Mem,              &[Byte(0x8B),     ModRegRM]),
        id!(MOV,               Reg16,    Reg16,            &[Byte(0x8B),     ModRegRM]),
        id!(MOV,               Reg8,     Imm,              &[PlusReg(0xB0),  ImmediateByte]),
        id!(MOV,               Reg16,    Imm,              &[PlusReg(0xB8),  ImmediateWord]),
        id!(MOV,               Mem8,     Imm,              &[Byte(0xC6),     ModRM(0),          ImmediateByte]),
        id!(MOV,               Mem16,    Imm,              &[Byte(0xC7),     ModRM(0),          ImmediateWord]),
        id!(MOV,               Mem8,     Imm,              &[Byte(0xC6),     ModRM(0),          ImmediateByte]),
        id!(MOV,               Mem16,    Imm,              &[Byte(0xC7),     ModRM(0),          ImmediateWord]),
    
        id!(STC,               None,     None,             &[Byte(0xF9)]),
        id!(STD,               None,     None,             &[Byte(0xFD)]),
        id!(STI,               None,     None,             &[Byte(0xFB)]),
        id!(JMP,               Disp8,    None,             &[Byte(0xEB),     DisplacementByte]),
        id!(JMP,               Disp16,   None,             &[Byte(0xE9),     DisplacementWord]),
    
        id!(JB,                Disp8,    None,             &[Byte(0x72),     DisplacementByte]),
    
        id!(PUSH,              Reg16,    None,             &[PlusReg(0x50)]),
        id!(POP,               Reg16,    None,             &[PlusReg(0x58)]),
    
        id!(IN,                RegAl,    Imm8,             &[Byte(0xE4),     ImmediateByte]),
        id!(IN,                RegAx,    Imm8,             &[Byte(0xE5),     ImmediateByte]),
    
        id!(CMP,               Mem,      Reg8,             &[Byte(0x38),     ModRegRM]),
        id!(CMP,               Reg8,     Reg8,             &[Byte(0x38),     ModRegRM]),
        id!(CMP,               Mem,      Reg16,            &[Byte(0x39),     ModRegRM]),
        id!(CMP,               Reg16,    Reg16,            &[Byte(0x39),     ModRegRM]),
        id!(CMP,               Reg8,     Mem,              &[Byte(0x3a),     ModRegRM]),
        id!(CMP,               Reg8,     Reg8,             &[Byte(0x3a),     ModRegRM]),
        id!(CMP,               Reg16,    Mem,              &[Byte(0x3b),     ModRegRM]),
        id!(CMP,               Reg16,    Reg16,            &[Byte(0x3b),     ModRegRM]),
        id!(CMP,               RegMem16, Imm8,             &[Byte(0x83),     ModRM(7),          ImmediateByteSigned]),
        id!(CMP,               RegAl,    Imm,              &[Byte(0x3c),     ImmediateByte]),
        id!(CMP,               RegAx,    SignedByteWord8,  &[Byte(0x83),     ModRM(7),          ImmediateByteSigned]),
        id!(CMP,               RegAx,    Imm,              &[Byte(0x3d),     ImmediateWord]),
        id!(CMP,               RegMem8,  Imm,              &[Byte(0x80),     ModRM(7),          ImmediateByte]),
        id!(CMP,               RegMem16, SignedByteWord8,  &[Byte(0x83),     ModRM(7),          ImmediateByteSigned]),
        id!(CMP,               RegMem16, Imm,              &[Byte(0x81),     ModRM(7),          ImmediateWord]),
        id!(CMP,               Mem,      Imm8,             &[Byte(0x80),     ModRM(7),          ImmediateByte]),
        id!(CMP,               Mem,      SignedByteWord16, &[Byte(0x83),     ModRM(7),          ImmediateByteSigned]),
        id!(CMP,               Mem,      Imm16,            &[Byte(0x81),     ModRM(7),          ImmediateWord]),
        id!(CMP,               RegMem8,  Imm,              &[Byte(0x82),     ModRM(7),          ImmediateByte]),
    
        id!(SUB,               Mem,      Reg8,             &[Byte(0x28),     ModRegRM]),
        id!(SUB,               Reg8,     Reg8,             &[Byte(0x28),     ModRegRM]),
        id!(SUB,               Mem,      Reg16,            &[Byte(0x29),     ModRegRM]),
        id!(SUB,               Reg16,    Reg16,            &[Byte(0x29),     ModRegRM]),
        id!(SUB,               Reg8,     Mem,              &[Byte(0x2A),     ModRegRM]),
        id!(SUB,               Reg8,     Reg8,             &[Byte(0x2A),     ModRegRM]),
        id!(SUB,               Reg16,    Mem,              &[Byte(0x2B),     ModRegRM]),
        id!(SUB,               Reg16,    Reg16,            &[Byte(0x2B),     ModRegRM]),
        id!(SUB,               RegMem16, Imm8,             &[Byte(0x83),     ModRM(5),          ImmediateByteSigned]),
        id!(SUB,               RegAl,    Imm,              &[Byte(0x2C),     ImmediateByte]),
        id!(SUB,               RegAx,    SignedByteWord8,  &[Byte(0x83),     ModRM(5),          ImmediateByteSigned]),
        id!(SUB,               RegAx,    Imm,              &[Byte(0x2D),     ImmediateWord]),
        id!(SUB,               RegMem8,  Imm,              &[Byte(0x80),     ModRM(5),          ImmediateByte]),
        id!(SUB,               RegMem16, SignedByteWord8,  &[Byte(0x83),     ModRM(5),          ImmediateByteSigned]),
        id!(SUB,               RegMem16, Imm,              &[Byte(0x81),     ModRM(5),          ImmediateWord]),
        id!(SUB,               Mem,      Imm8,             &[Byte(0x80),     ModRM(5),          ImmediateByte]),
        id!(SUB,               Mem,      SignedByteWord16, &[Byte(0x83),     ModRM(5),          ImmediateByteSigned]),
        id!(SUB,               Mem,      Imm16,            &[Byte(0x81),     ModRM(5),          ImmediateWord]),
        id!(SUB,               RegMem8,  Imm,              &[Byte(0x82),     ModRM(5),          ImmediateByte]),
    
        // THIS IS FROM MY HEAD
    
        id!(INT,               Imm8,     None,             &[Byte(0xCD),     ImmediateByte]),
        id!(CALL,              Disp8,    None,             &[]),
    ];
}

pub use private::INSTRUCTIONS;
