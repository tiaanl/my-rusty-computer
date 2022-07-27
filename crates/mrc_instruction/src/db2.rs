#![allow(dead_code, unused)]

use crate::{
    Immediate, Instruction, Operand, OperandSet, OperandSize, Operation, RegisterEncoding, Segment,
    SizedRegisterEncoding,
};

pub type TypeFlags = u16;

const fn gen_mask(bits: TypeFlags, shift: TypeFlags) -> TypeFlags {
    ((1 << bits) - 1) << shift
}

const fn gen_bit(bit: TypeFlags, shift: TypeFlags) -> TypeFlags {
    1 << (shift + bit)
}

pub trait HasFlags {
    fn contains(&self, flags: TypeFlags) -> bool;
}

impl HasFlags for TypeFlags {
    #[inline(always)]
    fn contains(&self, flags: TypeFlags) -> bool {
        *self & flags == flags
    }
}

macro_rules! bits_section {
    ($type:ident, $shift:literal, $bits:literal) => {
        #[allow(dead_code)]
        pub mod $type {
            use super::TypeFlags;
            const SHIFT: TypeFlags = $shift;
            const BITS: TypeFlags = $bits;
            const MASK: TypeFlags = super::gen_mask(BITS, SHIFT);
            pub const fn gen_bit(bit: TypeFlags) -> TypeFlags {
                super::gen_bit(bit, SHIFT)
            }
            pub fn only(flags: TypeFlags) -> TypeFlags {
                flags & MASK
            }
        }
    };
}

bits_section!(class, 0, 6);
bits_section!(sub_class, 6, 4);
bits_section!(size, 14, 2);

// class::MASK | sub_class::MASK | size::MASK == TypeFlags::MAX

pub const T_NONE: TypeFlags = 0;

// Classes

pub const T_IMM: TypeFlags = class::gen_bit(0);
pub const T_DISP: TypeFlags = class::gen_bit(1);
pub const T_REG: TypeFlags = class::gen_bit(2);
pub const T_SEG: TypeFlags = class::gen_bit(3);
pub const T_MEM: TypeFlags = class::gen_bit(4);
pub const T_SEG_OFF: TypeFlags = class::gen_bit(5);

// Sub Classes (T_IMM)
pub const T_SIGNED: TypeFlags = sub_class::gen_bit(0);

// Sub Classes (T_REG)

pub const T_REG_AL_AX: TypeFlags = sub_class::gen_bit(0);

// Sub Classes (T_SEG)

pub const T_SEG_ES: TypeFlags = sub_class::gen_bit(0);
pub const T_SEG_CS: TypeFlags = sub_class::gen_bit(1);
pub const T_SEG_SS: TypeFlags = sub_class::gen_bit(2);
pub const T_SEG_DS: TypeFlags = sub_class::gen_bit(3);

// Sizes

pub const T_BITS_8: TypeFlags = size::gen_bit(0);
pub const T_BITS_16: TypeFlags = size::gen_bit(1);

pub type Code = u8;

// Codes

pub const C_END: Code = 0x00;
pub const C_BYTE: Code = 0x01;
pub const C_REG_BASE: Code = 0x02;
pub const C_MOD_RM: Code = 0x03;
pub const C_MOD_REG_RM: Code = 0x04;
pub const C_IMM_BYTE: Code = 0x05;
pub const C_IMM_WORD: Code = 0x06;
pub const C_IMM_BYTE_SIGN: Code = 0x07;
pub const C_IMM_WORD_SIGN: Code = 0x07;
pub const C_DISP_BYTE: Code = 0x08;
pub const C_DISP_WORD: Code = 0x09;
pub const C_SEG_OFF: Code = 0x0A;

pub fn codes_to_string(codes: &[Code]) -> String {
    let mut parts = vec![];

    let mut i = 0;
    loop {
        if i >= codes.len() {
            break;
        }

        match codes[i] {
            C_END => {
                parts.push("C_END".to_owned());
            }

            C_BYTE => {
                parts.push("C_BYTE".to_owned());
                i += 1;
                parts.push(format!("{:#04X}", codes[i]));
            }

            C_REG_BASE => {
                parts.push("C_REG_BASE".to_owned());
                i += 1;
                parts.push(format!("{:#04X}", codes[i]));
            }

            C_MOD_REG_RM => {
                parts.push("C_MOD_REG_RM".to_owned());
            }

            C_MOD_RM => {
                parts.push("C_MOD_RM".to_owned());
                i += 1;
                parts.push(format!("{:#04X}", codes[i]));
            }

            C_IMM_BYTE => {
                parts.push("C_IMM_BYTE".to_owned());
            }

            C_IMM_WORD => {
                parts.push("C_IMM_WORD".to_owned());
            }

            C_IMM_BYTE_SIGN => {
                parts.push("C_IMM_BYTE_SIGN".to_owned());
            }

            C_IMM_WORD_SIGN => {
                parts.push("C_IMM_WORD_SIGN".to_owned());
            }

            C_DISP_BYTE => {
                parts.push("C_DISP_BYTE".to_owned());
            }

            C_DISP_WORD => {
                parts.push("C_DISP_WORD".to_owned());
            }

            C_SEG_OFF => {
                parts.push("C_SEG_OFF".to_owned());
            }

            _ => todo!("{}", codes[i]),
        }

        i += 1;
    }

    parts.push("C_END".to_owned());

    parts.join(", ")
}

pub struct Template {
    pub operation: Operation,
    pub dst: TypeFlags,
    pub src: TypeFlags,
    pub codes: &'static [Code],
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} [{}] [{}]",
            self.operation,
            format_type_flags(self.dst),
            format_type_flags(self.src),
        )
    }
}

macro_rules! t {
        ($operation:ident, [$($more:expr),+]) => {{
            Template {
                operation: Operation::$operation,
                dst: T_NONE,
                src: T_NONE,
                codes: &[$($more),*],
            }
        }};

        ($operation:ident, $dst:expr, [$($more:expr),+]) => {{
            Template {
                operation: Operation::$operation,
                dst: $dst,
                src: T_NONE,
                codes: &[$($more),*],
            }
        }};

        ($operation:ident, $dst:expr, $src:expr, [$($more:expr),+]) => {{
            Template {
                operation: Operation::$operation,
                dst: $dst,
                src: $src,
                codes: &[$($more),*],
            }
        }};
    }

#[rustfmt::skip]
const TEMPLATES: &[Template] = &[
    t!(AAA, [C_BYTE, 0x37, C_END]),
    t!(AAD, [C_BYTE, 0xD5, C_BYTE, 0x0A, C_END]),
    t!(AAM, [C_BYTE, 0xD4, C_BYTE, 0x0A, C_END]),
    t!(AAS, [C_BYTE, 0x3F, C_END]),
    t!(ADC, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x14, C_IMM_WORD, C_END]),
    t!(ADC, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x15, C_IMM_WORD, C_END]),
    t!(ADC, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x12, C_MOD_REG_RM, C_END]),
    t!(ADC, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x13, C_MOD_REG_RM, C_END]),
    t!(ADC, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x10, C_MOD_REG_RM, C_END]),
    t!(ADC, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x11, C_MOD_REG_RM, C_END]),
    t!(ADC, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x02, C_IMM_BYTE, C_END]),
    t!(ADC, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x02, C_IMM_WORD, C_END]),
    t!(ADC, T_REG|T_MEM|T_BITS_16, T_IMM|T_SIGNED|T_BITS_16, [C_BYTE, 0x83, C_MOD_RM, 0x02, C_IMM_BYTE_SIGN, C_END]),
    t!(ADD, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x04, C_IMM_WORD, C_END]),
    t!(ADD, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x05, C_IMM_WORD, C_END]),
    t!(ADD, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x02, C_MOD_REG_RM, C_END]),
    t!(ADD, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x03, C_MOD_REG_RM, C_END]),
    t!(ADD, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x00, C_MOD_REG_RM, C_END]),
    t!(ADD, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x01, C_MOD_REG_RM, C_END]),
    t!(ADD, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x00, C_IMM_BYTE, C_END]),
    t!(ADD, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x00, C_IMM_WORD, C_END]),
    t!(ADD, T_REG|T_MEM|T_BITS_16, T_IMM|T_SIGNED|T_BITS_16, [C_BYTE, 0x83, C_MOD_RM, 0x00, C_IMM_BYTE_SIGN, C_END]),
    t!(AND, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x24, C_IMM_WORD, C_END]),
    t!(AND, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x25, C_IMM_WORD, C_END]),
    t!(AND, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x22, C_MOD_REG_RM, C_END]),
    t!(AND, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x23, C_MOD_REG_RM, C_END]),
    t!(AND, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x20, C_MOD_REG_RM, C_END]),
    t!(AND, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x21, C_MOD_REG_RM, C_END]),
    t!(AND, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x04, C_IMM_WORD, C_END]),
    t!(AND, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x04, C_IMM_WORD, C_END]),
    t!(CALL, T_DISP|T_BITS_16, [C_BYTE, 0xE8, C_DISP_WORD, C_END]),
    t!(CALL, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x02, C_END]),
    t!(CALL, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x03, C_END]),
    t!(CALL, T_SEG_OFF, [C_BYTE, 0x9A, C_SEG_OFF, C_END]),
    t!(CBW, [C_BYTE, 0x98, C_END]),
    t!(CLC, [C_BYTE, 0xF8, C_END]),
    t!(CLD, [C_BYTE, 0xFC, C_END]),
    t!(CLI, [C_BYTE, 0xFA, C_END]),
    t!(CMC, [C_BYTE, 0xF5, C_END]),
    t!(CMP, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x3C, C_IMM_WORD, C_END]),
    t!(CMP, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x3D, C_IMM_WORD, C_END]),
    t!(CMP, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x3A, C_MOD_REG_RM, C_END]),
    t!(CMP, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x3B, C_MOD_REG_RM, C_END]),
    t!(CMP, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x38, C_MOD_REG_RM, C_END]),
    t!(CMP, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x39, C_MOD_REG_RM, C_END]),
    t!(CMP, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x07, C_IMM_BYTE, C_END]),
    t!(CMP, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x07, C_IMM_WORD, C_END]),
    t!(CMP, T_REG|T_MEM|T_BITS_16, T_IMM|T_SIGNED|T_BITS_16, [C_BYTE, 0x83, C_MOD_RM, 0x07, C_IMM_BYTE_SIGN, C_END]),
    t!(CMPSB, [C_BYTE, 0xA6, C_END]),
    t!(CMPSW, [C_BYTE, 0xA7, C_END]),
    t!(CWD, [C_BYTE, 0x99, C_END]),
    t!(DAA, [C_BYTE, 0x27, C_END]),
    t!(DAS, [C_BYTE, 0x2F, C_END]),
    t!(DEC, T_REG|T_BITS_16, [C_REG_BASE, 0x48, C_END]),
    t!(DEC, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0xFE, C_MOD_RM, 0x01, C_END]),
    t!(DEC, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x01, C_END]),
    t!(DIV, [C_BYTE, 0xF6, C_MOD_RM, 0x06, C_END]),
    t!(DIV, [C_BYTE, 0xF7, C_MOD_RM, 0x06, C_END]),
    t!(HLT, [C_BYTE, 0xF4, C_END]),
    t!(IDIV, [C_BYTE, 0xF6, C_MOD_RM, 0x07, C_END]),
    t!(IDIV, [C_BYTE, 0xF7, C_MOD_RM, 0x07, C_END]),
    t!(IMUL, [C_BYTE, 0xF6, C_MOD_RM, 0x05, C_END]),
    t!(IMUL, [C_BYTE, 0xF7, C_MOD_RM, 0x05, C_END]),
    t!(IN, T_IMM|T_BITS_8, [C_BYTE, 0xE4, C_IMM_BYTE, C_END]),
    t!(IN, T_IMM|T_BITS_16, [C_BYTE, 0xE5, C_IMM_BYTE, C_END]),
    t!(IN, T_IMM|T_BITS_8, [C_BYTE, 0xEC, C_END]),
    t!(IN, T_IMM|T_BITS_16, [C_BYTE, 0xED, C_END]),
    t!(INC, T_REG|T_BITS_16, [C_REG_BASE, 0x40, C_END]),
    t!(INC, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0xFE, C_MOD_RM, 0x00, C_END]),
    t!(INC, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x00, C_END]),
    t!(INT, T_IMM|T_BITS_8, [C_BYTE, 0xCD, C_IMM_BYTE, C_END]),
    t!(INT3, [C_BYTE, 0xCC, C_END]),
    t!(INTO, [C_BYTE, 0xCE, C_END]),
    t!(IRET, [C_BYTE, 0xCF, C_END]),
    t!(JB, [C_BYTE, 0x72, C_DISP_BYTE, C_END]),
    t!(JBE, [C_BYTE, 0x76, C_DISP_BYTE, C_END]),
    t!(JCXZ, [C_BYTE, 0xE3, C_DISP_BYTE, C_END]),
    t!(JE, [C_BYTE, 0x74, C_DISP_BYTE, C_END]),
    t!(JL, [C_BYTE, 0x7C, C_DISP_BYTE, C_END]),
    t!(JLE, [C_BYTE, 0x7E, C_DISP_BYTE, C_END]),
    t!(JMP, T_DISP|T_BITS_16, [C_BYTE, 0xE9, C_DISP_WORD, C_END]),
    t!(JMP, T_DISP|T_BITS_8, [C_BYTE, 0xEB, C_DISP_BYTE, C_END]),
    t!(JMP, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x04, C_END]),
    t!(JMP, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x05, C_END]),
    t!(JMP, T_SEG_OFF, [C_BYTE, 0xEA, C_SEG_OFF, C_END]),
    t!(JNB, [C_BYTE, 0x73, C_DISP_BYTE, C_END]),
    t!(JNBE, [C_BYTE, 0x77, C_DISP_BYTE, C_END]),
    t!(JNE, [C_BYTE, 0x75, C_DISP_BYTE, C_END]),
    t!(JNL, [C_BYTE, 0x7D, C_DISP_BYTE, C_END]),
    t!(JNLE, [C_BYTE, 0x7F, C_DISP_BYTE, C_END]),
    t!(JNO, [C_BYTE, 0x71, C_DISP_BYTE, C_END]),
    t!(JNP, [C_BYTE, 0x7B, C_DISP_BYTE, C_END]),
    t!(JNS, [C_BYTE, 0x79, C_DISP_BYTE, C_END]),
    t!(JO, [C_BYTE, 0x70, C_DISP_BYTE, C_END]),
    t!(JP, [C_BYTE, 0x7A, C_DISP_BYTE, C_END]),
    t!(JS, [C_BYTE, 0x78, C_DISP_BYTE, C_END]),
    t!(LAHF, [C_BYTE, 0x9F, C_END]),
    t!(LDS, [C_BYTE, 0xC5, C_MOD_REG_RM, C_END]),
    t!(LEA, [C_BYTE, 0x8D, C_MOD_REG_RM, C_END]),
    t!(LES, [C_BYTE, 0xC4, C_MOD_REG_RM, C_END]),
    t!(LOCK, [C_BYTE, 0xF0, C_END]),
    t!(LODSB, [C_BYTE, 0xAC, C_END]),
    t!(LODSW, [C_BYTE, 0xAD, C_END]),
    t!(LOOP, [C_BYTE, 0xE2, C_DISP_BYTE, C_END]),
    t!(LOOPNZ, [C_BYTE, 0xE0, C_DISP_BYTE, C_END]),
    t!(LOOPZ, [C_BYTE, 0xE1, C_DISP_BYTE, C_END]),
    t!(MOV, T_REG|T_REG_AL_AX|T_BITS_8, T_MEM|T_BITS_8, [C_BYTE, 0xA0, C_IMM_WORD, C_END]),
    t!(MOV, T_REG|T_REG_AL_AX|T_BITS_16, T_MEM|T_BITS_16, [C_BYTE, 0xA1, C_IMM_WORD, C_END]),
    t!(MOV, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x8A, C_MOD_REG_RM, C_END]),
    t!(MOV, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x8B, C_MOD_REG_RM, C_END]),
    t!(MOV, T_REG|T_BITS_8, T_IMM|T_BITS_8, [C_REG_BASE, 0xB0, C_IMM_BYTE, C_END]),
    t!(MOV, T_REG|T_BITS_16, T_IMM|T_BITS_16, [C_REG_BASE, 0xB8, C_IMM_WORD, C_END]),
    t!(MOV, T_SEG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x8E, C_MOD_REG_RM, C_END]),
    t!(MOV, T_MEM|T_BITS_8, T_REG|T_REG_AL_AX|T_BITS_8, [C_BYTE, 0xA2, C_IMM_WORD, C_END]),
    t!(MOV, T_MEM|T_BITS_16, T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE, 0xA3, C_IMM_WORD, C_END]),
    t!(MOV, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x88, C_MOD_REG_RM, C_END]),
    t!(MOV, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x89, C_MOD_REG_RM, C_END]),
    t!(MOV, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0xC6, C_MOD_RM, 0x00, C_IMM_WORD, C_END]),
    t!(MOV, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0xC7, C_MOD_RM, 0x00, C_IMM_WORD, C_END]),
    t!(MOV, T_REG|T_MEM|T_BITS_16, T_SEG|T_BITS_16, [C_BYTE, 0x8C, C_MOD_REG_RM, C_END]),
    t!(MOVSB, [C_BYTE, 0xA4, C_END]),
    t!(MOVSW, [C_BYTE, 0xA5, C_END]),
    t!(MUL, [C_BYTE, 0xF6, C_MOD_RM, 0x04, C_END]),
    t!(MUL, [C_BYTE, 0xF7, C_MOD_RM, 0x04, C_END]),
    t!(NEG, [C_BYTE, 0xF6, C_MOD_RM, 0x03, C_END]),
    t!(NEG, [C_BYTE, 0xF7, C_MOD_RM, 0x03, C_END]),
    t!(NOT, [C_BYTE, 0xF6, C_MOD_RM, 0x02, C_END]),
    t!(NOT, [C_BYTE, 0xF7, C_MOD_RM, 0x02, C_END]),
    t!(OR, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x0C, C_IMM_WORD, C_END]),
    t!(OR, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x0D, C_IMM_WORD, C_END]),
    t!(OR, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x0A, C_MOD_REG_RM, C_END]),
    t!(OR, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x0B, C_MOD_REG_RM, C_END]),
    t!(OR, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x08, C_MOD_REG_RM, C_END]),
    t!(OR, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x09, C_MOD_REG_RM, C_END]),
    t!(OR, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x01, C_IMM_WORD, C_END]),
    t!(OR, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x01, C_IMM_WORD, C_END]),
    t!(OUT, T_IMM|T_BITS_8, [C_BYTE, 0xE6, C_IMM_BYTE, C_END]),
    t!(OUT, T_IMM|T_BITS_16, [C_BYTE, 0xE7, C_IMM_BYTE, C_END]),
    t!(OUT, T_IMM|T_BITS_8, [C_BYTE, 0xEE, C_END]),
    t!(OUT, T_IMM|T_BITS_16, [C_BYTE, 0xEF, C_END]),
    t!(POP, T_REG|T_BITS_16, [C_REG_BASE, 0x58, C_END]),
    t!(POP, T_SEG|T_SEG_DS|T_BITS_16, [C_BYTE, 0x1F, C_END]),
    t!(POP, T_SEG|T_SEG_SS|T_BITS_16, [C_BYTE, 0x17, C_END]),
    t!(POP, T_SEG|T_SEG_CS|T_BITS_16, [C_BYTE, 0x0F, C_END]),
    t!(POP, T_SEG|T_SEG_ES|T_BITS_16, [C_BYTE, 0x07, C_END]),
    t!(POP, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x8F, C_MOD_RM, 0x00, C_END]),
    t!(POPF, [C_BYTE, 0x9D, C_END]),
    t!(PUSH, T_REG|T_BITS_16, [C_REG_BASE, 0x50, C_END]),
    t!(PUSH, T_SEG|T_SEG_DS|T_BITS_16, [C_BYTE, 0x1E, C_END]),
    t!(PUSH, T_SEG|T_SEG_SS|T_BITS_16, [C_BYTE, 0x16, C_END]),
    t!(PUSH, T_SEG|T_SEG_CS|T_BITS_16, [C_BYTE, 0x0E, C_END]),
    t!(PUSH, T_SEG|T_SEG_ES|T_BITS_16, [C_BYTE, 0x06, C_END]),
    t!(PUSH, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0xFF, C_MOD_RM, 0x06, C_END]),
    t!(PUSHF, [C_BYTE, 0x9C, C_END]),
    t!(RCL, [C_BYTE, 0xD0, C_MOD_RM, 0x02, C_END]),
    t!(RCL, [C_BYTE, 0xD1, C_MOD_RM, 0x02, C_END]),
    t!(RCL, [C_BYTE, 0xD2, C_MOD_RM, 0x02, C_END]),
    t!(RCL, [C_BYTE, 0xD3, C_MOD_RM, 0x02, C_END]),
    t!(RCR, [C_BYTE, 0xD0, C_MOD_RM, 0x03, C_END]),
    t!(RCR, [C_BYTE, 0xD1, C_MOD_RM, 0x03, C_END]),
    t!(RCR, [C_BYTE, 0xD2, C_MOD_RM, 0x03, C_END]),
    t!(RCR, [C_BYTE, 0xD3, C_MOD_RM, 0x03, C_END]),
    t!(REP, [C_BYTE, 0xF2, C_END]),
    t!(REPZ, [C_BYTE, 0xF3, C_END]),
    t!(RET, [C_BYTE, 0xC3, C_END]),
    t!(RET, [C_BYTE, 0xCB, C_END]),
    t!(RET, T_DISP|T_BITS_16, [C_BYTE, 0xC2, C_DISP_WORD, C_END]),
    t!(RET, T_DISP|T_BITS_16, [C_BYTE, 0xCA, C_DISP_WORD, C_END]),
    t!(ROL, [C_BYTE, 0xD0, C_MOD_RM, 0x00, C_END]),
    t!(ROL, [C_BYTE, 0xD1, C_MOD_RM, 0x00, C_END]),
    t!(ROL, [C_BYTE, 0xD2, C_MOD_RM, 0x00, C_END]),
    t!(ROL, [C_BYTE, 0xD3, C_MOD_RM, 0x00, C_END]),
    t!(ROR, [C_BYTE, 0xD0, C_MOD_RM, 0x01, C_END]),
    t!(ROR, [C_BYTE, 0xD1, C_MOD_RM, 0x01, C_END]),
    t!(ROR, [C_BYTE, 0xD2, C_MOD_RM, 0x01, C_END]),
    t!(ROR, [C_BYTE, 0xD3, C_MOD_RM, 0x01, C_END]),
    t!(SAHF, [C_BYTE, 0x9E, C_END]),
    t!(SAR, [C_BYTE, 0xD0, C_MOD_RM, 0x07, C_END]),
    t!(SAR, [C_BYTE, 0xD1, C_MOD_RM, 0x07, C_END]),
    t!(SAR, [C_BYTE, 0xD2, C_MOD_RM, 0x07, C_END]),
    t!(SAR, [C_BYTE, 0xD3, C_MOD_RM, 0x07, C_END]),
    t!(SBB, T_IMM|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x03, C_IMM_BYTE, C_END]),
    t!(SBB, T_IMM|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x03, C_IMM_WORD, C_END]),
    t!(SBB, T_IMM|T_BITS_16, T_REG|T_MEM|T_REG_AL_AX|T_BITS_16, [C_BYTE, 0x83, C_MOD_RM, 0x03, C_IMM_BYTE_SIGN, C_END]),
    t!(SBB, T_IMM|T_BITS_8, T_REG|T_REG_AL_AX|T_BITS_8, [C_BYTE, 0x0E, C_IMM_WORD, C_END]),
    t!(SBB, T_IMM|T_BITS_16, T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE, 0x0F, C_IMM_WORD, C_END]),
    t!(SBB, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x1A, C_MOD_REG_RM, C_END]),
    t!(SBB, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x1B, C_MOD_REG_RM, C_END]),
    t!(SBB, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x18, C_MOD_REG_RM, C_END]),
    t!(SBB, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x19, C_MOD_REG_RM, C_END]),
    t!(SCASB, [C_BYTE, 0xAE, C_END]),
    t!(SCASW, [C_BYTE, 0xAF, C_END]),
    t!(SHL, [C_BYTE, 0xD0, C_MOD_RM, 0x04, C_END]),
    t!(SHL, [C_BYTE, 0xD1, C_MOD_RM, 0x04, C_END]),
    t!(SHL, [C_BYTE, 0xD2, C_MOD_RM, 0x04, C_END]),
    t!(SHL, [C_BYTE, 0xD3, C_MOD_RM, 0x04, C_END]),
    t!(SHR, [C_BYTE, 0xD0, C_MOD_RM, 0x05, C_END]),
    t!(SHR, [C_BYTE, 0xD1, C_MOD_RM, 0x05, C_END]),
    t!(SHR, [C_BYTE, 0xD2, C_MOD_RM, 0x05, C_END]),
    t!(SHR, [C_BYTE, 0xD3, C_MOD_RM, 0x05, C_END]),
    t!(STC, [C_BYTE, 0xF9, C_END]),
    t!(STD, [C_BYTE, 0xFD, C_END]),
    t!(STI, [C_BYTE, 0xFB, C_END]),
    t!(STOSB, [C_BYTE, 0xAA, C_END]),
    t!(STOSW, [C_BYTE, 0xAB, C_END]),
    t!(SUB, T_IMM|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x05, C_IMM_BYTE, C_END]),
    t!(SUB, T_IMM|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x05, C_IMM_WORD, C_END]),
    t!(SUB, T_IMM|T_BITS_16, T_REG|T_MEM|T_REG_AL_AX|T_BITS_16, [C_BYTE, 0x83, C_MOD_RM, 0x05, C_IMM_BYTE_SIGN, C_END]),
    t!(SUB, T_IMM|T_BITS_8, T_REG|T_REG_AL_AX|T_BITS_8, [C_BYTE, 0x2C, C_IMM_WORD, C_END]),
    t!(SUB, T_IMM|T_BITS_16, T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE, 0x2D, C_IMM_WORD, C_END]),
    t!(SUB, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x2A, C_MOD_REG_RM, C_END]),
    t!(SUB, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x2B, C_MOD_REG_RM, C_END]),
    t!(SUB, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x28, C_MOD_REG_RM, C_END]),
    t!(SUB, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x29, C_MOD_REG_RM, C_END]),
    t!(TEST, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0xA8, C_IMM_WORD, C_END]),
    t!(TEST, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0xA9, C_IMM_WORD, C_END]),
    t!(TEST, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x84, C_MOD_REG_RM, C_END]),
    t!(TEST, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x85, C_MOD_REG_RM, C_END]),
    t!(TEST, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0xF6, C_MOD_RM, 0x00, C_IMM_WORD, C_END]),
    t!(TEST, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0xF7, C_MOD_RM, 0x00, C_IMM_WORD, C_END]),
    t!(WAIT, [C_BYTE, 0x9B, C_END]),
    t!(XCHG, T_REG|T_REG_AL_AX|T_BITS_16, [C_REG_BASE, 0x90, C_END]),
    t!(XCHG, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x86, C_MOD_REG_RM, C_END]),
    t!(XCHG, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x87, C_MOD_REG_RM, C_END]),
    t!(XLAT, [C_BYTE, 0xD7, C_END]),
    t!(XOR, T_REG|T_REG_AL_AX|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x34, C_IMM_WORD, C_END]),
    t!(XOR, T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x35, C_IMM_WORD, C_END]),
    t!(XOR, T_REG|T_BITS_8, T_REG|T_MEM|T_BITS_8, [C_BYTE, 0x32, C_MOD_REG_RM, C_END]),
    t!(XOR, T_REG|T_BITS_16, T_REG|T_MEM|T_BITS_16, [C_BYTE, 0x33, C_MOD_REG_RM, C_END]),
    t!(XOR, T_REG|T_MEM|T_BITS_8, T_REG|T_BITS_8, [C_BYTE, 0x30, C_MOD_REG_RM, C_END]),
    t!(XOR, T_REG|T_MEM|T_BITS_16, T_REG|T_BITS_16, [C_BYTE, 0x31, C_MOD_REG_RM, C_END]),
    t!(XOR, T_REG|T_MEM|T_BITS_8, T_IMM|T_BITS_8, [C_BYTE, 0x80, C_MOD_RM, 0x06, C_IMM_WORD, C_END]),
    t!(XOR, T_REG|T_MEM|T_BITS_16, T_IMM|T_BITS_16, [C_BYTE, 0x81, C_MOD_RM, 0x06, C_IMM_WORD, C_END]),
];

pub fn find_match(
    operation: Operation,
    dst: TypeFlags,
    src: TypeFlags,
) -> Option<&'static Template> {
    for temp in TEMPLATES {
        macro_rules! check_type_flags {
            ($tf:ident) => {{
                if $tf != T_NONE {
                    if !temp.$tf.contains(class::only($tf)) {
                        continue;
                    }

                    if !$tf.contains(sub_class::only(temp.$tf)) {
                        continue;
                    }

                    if !temp.$tf.contains(size::only($tf)) {
                        continue;
                    }
                }
            }};
        }

        if temp.operation != operation {
            continue;
        }

        // println!(
        //     "{:?} [{}] [{}] ==> [{}] [{}]",
        //     temp.operation,
        //     format_type_flags(temp.dst),
        //     format_type_flags(temp.src),
        //     format_type_flags(dst),
        //     format_type_flags(src),
        // );

        check_type_flags!(dst);
        check_type_flags!(src);

        return Some(temp);
    }
    None
}

fn operand_to_type(operand: &Operand) -> TypeFlags {
    match operand {
        Operand::Register(SizedRegisterEncoding(RegisterEncoding::AlAx, operand_size)) => {
            T_REG
                | T_REG_AL_AX
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Register(SizedRegisterEncoding(_, operand_size)) => {
            T_REG
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Immediate(imm) => {
            T_IMM
                | if let Immediate::Byte(_) = imm {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        Operand::Direct(_, _, operand_size) => {
            T_MEM
                | if let OperandSize::Byte = operand_size {
                    T_BITS_8
                } else {
                    T_BITS_16
                }
        }

        _ => todo!("{:?}", operand),
    }
}

fn find_match_for_instruction(insn: &Instruction) -> Option<&'static Template> {
    let (dst, src) = match &insn.operands {
        OperandSet::DestinationAndSource(dst, src) => (operand_to_type(dst), operand_to_type(src)),
        OperandSet::Destination(dst) => (operand_to_type(dst), T_NONE),
        OperandSet::None => (T_NONE, T_NONE),
    };

    find_match(insn.operation, dst, src)
}

pub fn format_type_flags(flags: TypeFlags) -> String {
    let mut parts = vec![];

    macro_rules! do_flag {
        ($f:expr, $s:literal) => {{
            if flags.contains($f) {
                parts.push($s);
            }
        }};
    }

    // Class

    do_flag!(T_IMM, "T_IMM");
    do_flag!(T_DISP, "T_DISP");
    do_flag!(T_REG, "T_REG");
    do_flag!(T_SEG, "T_SEG");
    do_flag!(T_MEM, "T_MEM");
    do_flag!(T_SEG_OFF, "T_SEG_OFF");

    // Sub Class

    if flags.contains(T_IMM) {
        do_flag!(T_SIGNED, "T_SIGNED");
    }

    if flags.contains(T_REG) {
        do_flag!(T_REG_AL_AX, "T_REG_AL_AX");
    }

    if flags.contains(T_SEG) {
        do_flag!(T_SEG_ES, "T_SEG_ES");
        do_flag!(T_SEG_CS, "T_SEG_CS");
        do_flag!(T_SEG_SS, "T_SEG_SS");
        do_flag!(T_SEG_DS, "T_SEG_DS");
    }

    // Size

    do_flag!(T_BITS_8, "T_BITS_8");
    do_flag!(T_BITS_16, "T_BITS_16");

    parts.to_owned().join("|")
}

macro_rules! imm8 {
    ($value:expr) => {{
        Operand::Immediate(Immediate::Byte($value))
    }};
}

macro_rules! imm16 {
    ($value:expr) => {{
        Operand::Immediate(Immediate::Word($value))
    }};
}

macro_rules! reg_internal {
    ($encoding:ident, $size:ident) => {{
        Operand::Register(SizedRegisterEncoding(
            RegisterEncoding::$encoding,
            OperandSize::$size,
        ))
    }};
}

macro_rules! reg8 {
    (al) => {{
        reg_internal!(AlAx, Byte)
    }};

    (bl) => {{
        reg_internal!(BlBx, Byte)
    }};

    (cl) => {{
        reg_internal!(ClCx, Byte)
    }};

    (dl) => {{
        reg_internal!(DlDx, Byte)
    }};
}

macro_rules! reg16 {
    (ax) => {{
        reg_internal!(AlAx, Word)
    }};

    (bx) => {{
        reg_internal!(BlBx, Word)
    }};

    (cx) => {{
        reg_internal!(ClCx, Word)
    }};

    (dx) => {{
        reg_internal!(DlDx, Word)
    }};
}

macro_rules! segment_internal {
    (es) => {{
        Segment::ES
    }};

    (cs) => {{
        Segment::CS
    }};

    (ss) => {{
        Segment::SS
    }};

    (ds) => {{
        Segment::DS
    }};
}

macro_rules! direct8 {
    ($segment:ident, $addr:literal) => {{
        Operand::Direct(segment_internal!($segment), $addr, OperandSize::Byte)
    }};
}

macro_rules! direct16 {
    ($segment:ident, $addr:literal) => {{
        Operand::Direct(segment_internal!($segment), $addr, OperandSize::Word)
    }};
}

macro_rules! insn {
    ($operation:ident) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::None,
            repeat: None,
        }
    }};

    ($operation:ident, $dst:expr) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::Destination($dst),
            repeat: None,
        }
    }};

    ($operation:ident, $dst:expr, $src:expr) => {{
        Instruction {
            operation: Operation::$operation,
            operands: OperandSet::DestinationAndSource($dst, $src),
            repeat: None,
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        #[rustfmt::skip]
        let tests: &[(Instruction, &[Code])] = &[
            (insn!(AAA), &[C_BYTE, 0x37]),
            (insn!(AAD), &[C_BYTE, 0xD5, C_BYTE, 0x0A]),
            (insn!(AAM), &[C_BYTE, 0xD4, C_BYTE, 0x0A]),
            (insn!(AAS), &[C_BYTE, 0x3F]),
            (insn!(ADC, reg8!(al), imm8!(0x10)), &[C_BYTE, 0x14]),           // acc8,imm8
            (insn!(ADC, reg16!(ax), imm16!(0x100)), &[C_BYTE, 0x15]),        // acc16,imm16
            (insn!(ADC, reg8!(cl), imm8!(0x10)), &[C_BYTE, 0x80]),           // reg8,imm8
            (insn!(ADC, reg16!(cx), imm16!(0x100)), &[C_BYTE, 0x81]),        // reg16,imm16
            (insn!(ADC, reg8!(cl), reg8!(dl)), &[C_BYTE, 0x12]),             // reg8,reg8
            (insn!(ADC, reg16!(cx), reg16!(dx)), &[C_BYTE, 0x13]),           // reg16,reg16
            (insn!(ADC, reg8!(cl), direct8!(ds, 0x100)), &[C_BYTE, 0x12]),   // reg8,mem8
            (insn!(ADC, reg16!(cx), direct16!(ds, 0x100)), &[C_BYTE, 0x13]), // reg16,mem16
        ];

        for test in tests {
            let (instruction, codes) = test;
            let m = find_match_for_instruction(instruction);
            assert!(m.is_some(), "No match for [{}]", instruction);
            if let Some(m) = m {
                assert_eq!(
                    instruction.operation, m.operation,
                    "Incorrect operation for [{}]",
                    instruction
                );

                let max = codes.len();

                assert!(
                    m.codes.len() >= max,
                    "Not enough codes for [{}]",
                    instruction
                );

                assert_eq!(
                    codes[..max],
                    m.codes[..max],
                    "Incorrect codes for [{}] code:[{}] m.codes:[{}]",
                    instruction,
                    codes_to_string(codes),
                    codes_to_string(m.codes),
                );
            }
        }
    }
}
