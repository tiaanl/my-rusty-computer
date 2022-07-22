use crate::{
    Immediate, Instruction, Operand, OperandSet, OperandSize, Operation, SizedRegisterEncoding,
};

type TypeFlags = u16;

const fn gen_mask(bits: TypeFlags, shift: TypeFlags) -> TypeFlags {
    ((1 << bits) - 1) << shift
}

const fn gen_bit(bit: TypeFlags, shift: TypeFlags) -> TypeFlags {
    1 << (shift + bit)
}

macro_rules! bits_section {
    ($type:ident, $shift:literal, $bits:literal) => {
        #[allow(dead_code)]
        mod $type {
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

bits_section!(class, 0, 5);
bits_section!(sub_class, 5, 8);
bits_section!(size, 14, 2);

const T_NONE: TypeFlags = 0;

// Types

const T_IMM: TypeFlags = class::gen_bit(0);
const T_DISP: TypeFlags = class::gen_bit(1);
const T_REG: TypeFlags = class::gen_bit(2);
const T_SEG: TypeFlags = class::gen_bit(3);
const T_MEM: TypeFlags = class::gen_bit(4);

// Sub Types (T_REG)

const T_REG_AL_AX: TypeFlags = sub_class::gen_bit(0);

// Sub Types (T_SEG)

const T_SEG_ES: TypeFlags = sub_class::gen_bit(0);
const T_SEG_CS: TypeFlags = sub_class::gen_bit(1);
const T_SEG_SS: TypeFlags = sub_class::gen_bit(2);
const T_SEG_DS: TypeFlags = sub_class::gen_bit(3);

// Sizes

const T_BITS_8: TypeFlags = size::gen_bit(0);
const T_BITS_16: TypeFlags = size::gen_bit(1);

type Code = u8;

// Codes

const C_END: Code = 0x00;
const C_BYTE: Code = 0x01;
const C_PLUS_REG: Code = 0x02;
const C_MOD_RM: Code = 0x03;
const C_MOD_REG_RM: Code = 0x04;
const C_IMM_BYTE: Code = 0x05;
const C_IMM_WORD: Code = 0x06;

pub struct Template {
    operation: Operation,
    dst: TypeFlags,
    src: TypeFlags,
    codes: &'static [Code],
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
                codes: &[$($more),*, C_END],
            }
        }};

        ($operation:ident, $dst:expr, [$($more:expr),+]) => {{
            Template {
                operation: Operation::$operation,
                dst: $dst,
                src: T_NONE,
                codes: &[$($more),*, C_END],
            }
        }};

        ($operation:ident, $dst:expr, $src:expr, [$($more:expr),+]) => {{
            Template {
                operation: Operation::$operation,
                dst: $dst,
                src: $src,
                codes: &[$($more),*, C_END],
            }
        }};
    }

#[rustfmt::skip]
const TEMPLATES: &[Template] = &[
    t!(AAA,                                                               [C_BYTE,     0x37]),
    t!(AAD,                                                               [C_BYTE,     0xD5, C_BYTE, 0x0A]),
    t!(AAD,     T_IMM|T_BITS_8,                                           [C_BYTE,     0xD5, C_IMM_BYTE]),
    t!(AAM,                                                               [C_BYTE,     0xD4, C_BYTE, 0x0A]),
    t!(AAM,     T_IMM|T_BITS_8,                                           [C_BYTE,     0xD4, C_IMM_BYTE]),
    t!(AAS,                                                               [C_BYTE,     0x3F]),
    t!(ADC,     T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x14]),
    t!(ADC,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(ADC,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x82]),
    t!(ADC,     T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x15]),
    t!(ADC,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(ADC,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x83]),
    t!(ADC,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x10]),
    t!(ADC,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x11]),
    t!(ADC,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x12, C_MOD_REG_RM]),
    t!(ADC,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x13]),
    t!(ADD,     T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x04]),
    t!(ADD,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(ADD,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x82]),
    t!(ADD,     T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x05]),
    t!(ADD,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(ADD,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x83]),
    t!(ADD,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x00]),
    t!(ADD,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x01]),
    t!(ADD,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x02, C_MOD_REG_RM]),
    t!(ADD,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x03]),
    t!(AND,     T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x24]),
    t!(AND,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(AND,     T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x25]),
    t!(AND,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(AND,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x20]),
    t!(AND,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x21]),
    t!(AND,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x22, C_MOD_REG_RM]),
    t!(AND,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x23]),
    // t!(CALL, SegOff,                                                   [C_BYTE,     0x9A]),
    t!(CALL,    T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(CALL,    T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(CALL,    T_DISP|T_BITS_16,                                         [C_BYTE,     0xE8]),
    t!(CBW,                                                               [C_BYTE,     0x98]),
    t!(CLC,                                                               [C_BYTE,     0xF8]),
    t!(CLD,                                                               [C_BYTE,     0xFC]),
    t!(CLI,                                                               [C_BYTE,     0xFA]),
    t!(CMC,                                                               [C_BYTE,     0xF5]),
    t!(CMP,     T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x3C]),
    t!(CMP,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(CMP,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x82]),
    t!(CMP,     T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x3D]),
    t!(CMP,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(CMP,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x83]),
    t!(CMP,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x38]),
    t!(CMP,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x39]),
    t!(CMP,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x3A, C_MOD_REG_RM]),
    t!(CMP,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x3B]),
    t!(CMPSB,                                                             [C_BYTE,     0xA6]),
    t!(CMPSW,                                                             [C_BYTE,     0xA7]),
    t!(CWD,                                                               [C_BYTE,     0x99]),
    t!(DAA,                                                               [C_BYTE,     0x27]),
    t!(DAS,                                                               [C_BYTE,     0x2F]),
    t!(DEC,     T_REG|T_BITS_16,                                          [C_PLUS_REG, 0x48]),
    t!(DEC,     T_REG|T_MEM|T_BITS_8,                                     [C_BYTE,     0xFE]),
    t!(DEC,     T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(DIV,                                                               [C_BYTE,     0xF6]),
    t!(DIV,                                                               [C_BYTE,     0xF7]),
    t!(HLT,                                                               [C_BYTE,     0xF4]),
    t!(IDIV,                                                              [C_BYTE,     0xF6]),
    t!(IDIV,                                                              [C_BYTE,     0xF7]),
    t!(IMUL,                                                              [C_BYTE,     0xF6]),
    t!(IMUL,                                                              [C_BYTE,     0xF7]),
    t!(IN,      T_IMM|T_BITS_8,                                           [C_BYTE,     0xE4]),
    t!(IN,      T_IMM|T_BITS_8,                                           [C_BYTE,     0xEC]),
    t!(IN,      T_IMM|T_BITS_16,                                          [C_BYTE,     0xE5]),
    t!(IN,      T_IMM|T_BITS_16,                                          [C_BYTE,     0xED]),
    t!(INC,     T_REG|T_BITS_16,                                          [C_PLUS_REG, 0x40]),
    t!(INC,     T_REG|T_MEM|T_BITS_8,                                     [C_BYTE,     0xFE]),
    t!(INC,     T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(INT,     T_IMM|T_BITS_8,                                           [C_BYTE,     0xCD]),
    t!(INT3,                                                              [C_BYTE,     0xCC]),
    t!(INTO,                                                              [C_BYTE,     0xCE]),
    t!(IRET,                                                              [C_BYTE,     0xCF]),
    t!(JB,                                                                [C_BYTE,     0x72]),
    t!(JBE,                                                               [C_BYTE,     0x76]),
    t!(JCXZ,                                                              [C_BYTE,     0xE3]),
    t!(JE,                                                                [C_BYTE,     0x74]),
    t!(JL,                                                                [C_BYTE,     0x7C]),
    t!(JLE,                                                               [C_BYTE,     0x7E]),
    // t!(JMP,  SegOff,                                                   [C_BYTE,     0xEA]),
    t!(JMP,     T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(JMP,     T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(JMP,     T_DISP|T_BITS_8,                                          [C_BYTE,     0xEB]),
    t!(JMP,     T_DISP|T_BITS_16,                                         [C_BYTE,     0xE9]),
    t!(JNB,                                                               [C_BYTE,     0x73]),
    t!(JNBE,                                                              [C_BYTE,     0x77]),
    t!(JNE,                                                               [C_BYTE,     0x75]),
    t!(JNL,                                                               [C_BYTE,     0x7D]),
    t!(JNLE,                                                              [C_BYTE,     0x7F]),
    t!(JNO,                                                               [C_BYTE,     0x71]),
    t!(JNP,                                                               [C_BYTE,     0x7B]),
    t!(JNS,                                                               [C_BYTE,     0x79]),
    t!(JO,                                                                [C_BYTE,     0x70]),
    t!(JP,                                                                [C_BYTE,     0x7A]),
    t!(JS,                                                                [C_BYTE,     0x78]),
    t!(LAHF,                                                              [C_BYTE,     0x9F]),
    t!(LDS,                                                               [C_BYTE,     0xC5]),
    t!(LEA,                                                               [C_BYTE,     0x8D]),
    t!(LES,                                                               [C_BYTE,     0xC4]),
    t!(LOCK,                                                              [C_BYTE,     0xF0]),
    t!(LODSB,                                                             [C_BYTE,     0xAC]),
    t!(LODSW,                                                             [C_BYTE,     0xAD]),
    t!(LOOP,                                                              [C_BYTE,     0xE2]),
    t!(LOOPNZ,                                                            [C_BYTE,     0xE0]),
    t!(LOOPZ,                                                             [C_BYTE,     0xE1]),
    t!(MOV,     T_REG|T_BITS_8,              T_IMM|T_BITS_8,              [C_PLUS_REG, 0xB0]),
    t!(MOV,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0xC6]),
    t!(MOV,     T_REG|T_BITS_16,             T_IMM|T_BITS_16,             [C_PLUS_REG, 0xB8]),
    t!(MOV,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0xC7]),
    t!(MOV,     T_MEM|T_BITS_8,              T_REG|T_REG_AL_AX|T_BITS_8,  [C_BYTE,     0xA2]),
    t!(MOV,     T_MEM|T_BITS_16,             T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE,     0xA3]),
    t!(MOV,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x88]),
    t!(MOV,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x89]),
    t!(MOV,     T_REG|T_MEM|T_BITS_16,       T_REG|T_SEG|T_BITS_16,       [C_BYTE,     0x8C]),
    t!(MOV,     T_REG|T_REG_AL_AX|T_BITS_8,  T_MEM|T_BITS_16,             [C_BYTE,     0xA0]),
    t!(MOV,     T_REG|T_REG_AL_AX|T_BITS_16, T_MEM|T_BITS_16,             [C_BYTE,     0xA1]),
    t!(MOV,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x8A, C_MOD_REG_RM]),
    t!(MOV,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x8B]),
    t!(MOV,     T_REG|T_SEG|T_BITS_16,       T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x8E]),
    t!(MOVSB,                                                             [C_BYTE,     0xA4]),
    t!(MOVSW,                                                             [C_BYTE,     0xA5]),
    t!(MUL,                                                               [C_BYTE,     0xF6]),
    t!(MUL,                                                               [C_BYTE,     0xF7]),
    t!(NEG,                                                               [C_BYTE,     0xF6]),
    t!(NEG,                                                               [C_BYTE,     0xF7]),
    t!(NOT,                                                               [C_BYTE,     0xF6]),
    t!(NOT,                                                               [C_BYTE,     0xF7]),
    t!(OR,      T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x0C]),
    t!(OR,      T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(OR,      T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x0D]),
    t!(OR,      T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(OR,      T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x08]),
    t!(OR,      T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x09]),
    t!(OR,      T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x0A, C_MOD_REG_RM]),
    t!(OR,      T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x0B]),
    t!(OUT,     T_IMM|T_BITS_8,                                           [C_BYTE,     0xE6]),
    t!(OUT,     T_IMM|T_BITS_8,                                           [C_BYTE,     0xEE]),
    t!(OUT,     T_IMM|T_BITS_16,                                          [C_BYTE,     0xE7]),
    t!(OUT,     T_IMM|T_BITS_16,                                          [C_BYTE,     0xEF]),
    t!(POP,     T_REG|T_BITS_16,                                          [C_PLUS_REG, 0x58]),
    t!(POP,     T_SEG|T_SEG_ES|T_BITS_16,                                 [C_BYTE,     0x07]),
    t!(POP,     T_SEG|T_SEG_CS|T_BITS_16,                                 [C_BYTE,     0x0F]),
    t!(POP,     T_SEG|T_SEG_SS|T_BITS_16,                                 [C_BYTE,     0x17]),
    t!(POP,     T_SEG|T_SEG_DS|T_BITS_16,                                 [C_BYTE,     0x1F]),
    t!(POP,     T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0x8F]),
    t!(POPF,                                                              [C_BYTE,     0x9D]),
    t!(PUSH,    T_REG|T_BITS_16,                                          [C_PLUS_REG, 0x50]),
    t!(PUSH,    T_SEG|T_SEG_ES|T_BITS_16,                                 [C_BYTE,     0x06]),
    t!(PUSH,    T_SEG|T_SEG_CS|T_BITS_16,                                 [C_BYTE,     0x0E]),
    t!(PUSH,    T_SEG|T_SEG_SS|T_BITS_16,                                 [C_BYTE,     0x16]),
    t!(PUSH,    T_SEG|T_SEG_DS|T_BITS_16,                                 [C_BYTE,     0x1E]),
    t!(PUSH,    T_REG|T_MEM|T_BITS_16,                                    [C_BYTE,     0xFF]),
    t!(PUSHF,                                                             [C_BYTE,     0x9C]),
    t!(RCL,                                                               [C_BYTE,     0xD0]),
    t!(RCL,                                                               [C_BYTE,     0xD1]),
    t!(RCL,                                                               [C_BYTE,     0xD2]),
    t!(RCL,                                                               [C_BYTE,     0xD3]),
    t!(RCR,                                                               [C_BYTE,     0xD0]),
    t!(RCR,                                                               [C_BYTE,     0xD1]),
    t!(RCR,                                                               [C_BYTE,     0xD2]),
    t!(RCR,                                                               [C_BYTE,     0xD3]),
    t!(REP,                                                               [C_BYTE,     0xF2]),
    t!(REPZ,                                                              [C_BYTE,     0xF3]),
    t!(RET,                                                               [C_BYTE,     0xC3]),
    t!(RET,                                                               [C_BYTE,     0xCB]),
    t!(RET,     T_DISP|T_BITS_16,                                         [C_BYTE,     0xC2]),
    t!(RET,     T_DISP|T_BITS_16,                                         [C_BYTE,     0xCA]),
    t!(ROL,                                                               [C_BYTE,     0xD0]),
    t!(ROL,                                                               [C_BYTE,     0xD1]),
    t!(ROL,                                                               [C_BYTE,     0xD2]),
    t!(ROL,                                                               [C_BYTE,     0xD3]),
    t!(ROR,                                                               [C_BYTE,     0xD0]),
    t!(ROR,                                                               [C_BYTE,     0xD1]),
    t!(ROR,                                                               [C_BYTE,     0xD2]),
    t!(ROR,                                                               [C_BYTE,     0xD3]),
    t!(SAHF,                                                              [C_BYTE,     0x9E]),
    t!(SAR,                                                               [C_BYTE,     0xD0]),
    t!(SAR,                                                               [C_BYTE,     0xD1]),
    t!(SAR,                                                               [C_BYTE,     0xD2]),
    t!(SAR,                                                               [C_BYTE,     0xD3]),
    t!(SBB,     T_IMM|T_BITS_8,              T_REG|T_REG_AL_AX|T_BITS_8,  [C_BYTE,     0x0E]),
    t!(SBB,     T_IMM|T_BITS_16,             T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE,     0x0F]),
    t!(SBB,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x18]),
    t!(SBB,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x19]),
    t!(SBB,     T_IMM|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x80]),
    t!(SBB,     T_IMM|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x82]),
    t!(SBB,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x1A, C_MOD_REG_RM]),
    t!(SBB,     T_IMM|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x81]),
    t!(SBB,     T_IMM|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x83]),
    t!(SBB,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x1B]),
    t!(SCASB,                                                             [C_BYTE,     0xAE]),
    t!(SCASW,                                                             [C_BYTE,     0xAF]),
    t!(SHL,                                                               [C_BYTE,     0xD0]),
    t!(SHL,                                                               [C_BYTE,     0xD1]),
    t!(SHL,                                                               [C_BYTE,     0xD2]),
    t!(SHL,                                                               [C_BYTE,     0xD3]),
    t!(SHR,                                                               [C_BYTE,     0xD0]),
    t!(SHR,                                                               [C_BYTE,     0xD1]),
    t!(SHR,                                                               [C_BYTE,     0xD2]),
    t!(SHR,                                                               [C_BYTE,     0xD3]),
    t!(STC,                                                               [C_BYTE,     0xF9]),
    t!(STD,                                                               [C_BYTE,     0xFD]),
    t!(STI,                                                               [C_BYTE,     0xFB]),
    t!(STOSB,                                                             [C_BYTE,     0xAA]),
    t!(STOSW,                                                             [C_BYTE,     0xAB]),
    t!(SUB,     T_IMM|T_BITS_8,              T_REG|T_REG_AL_AX|T_BITS_8,  [C_BYTE,     0x2C]),
    t!(SUB,     T_IMM|T_BITS_16,             T_REG|T_REG_AL_AX|T_BITS_16, [C_BYTE,     0x2D]),
    t!(SUB,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x28]),
    t!(SUB,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x29]),
    t!(SUB,     T_IMM|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x80]),
    t!(SUB,     T_IMM|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x82]),
    t!(SUB,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x2A, C_MOD_REG_RM]),
    t!(SUB,     T_IMM|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x81]),
    t!(SUB,     T_IMM|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x83]),
    t!(SUB,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x2B]),
    t!(TEST,    T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0xA8]),
    t!(TEST,    T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0xF6]),
    t!(TEST,    T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0xA9]),
    t!(TEST,    T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0xF7]),
    t!(TEST,    T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x84, C_MOD_REG_RM]),
    t!(TEST,    T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x85]),
    t!(WAIT,                                                              [C_BYTE,     0x9B]),
    t!(XCHG,    T_REG|T_REG_AL_AX|T_BITS_16, T_REG|T_BITS_16,             [C_PLUS_REG, 0x90]),
    t!(XCHG,    T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x86, C_MOD_REG_RM]),
    t!(XCHG,    T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x87]),
    t!(XLAT,                                                              [C_BYTE,     0xD7]),
    t!(XOR,     T_REG|T_REG_AL_AX|T_BITS_8,  T_IMM|T_BITS_8,              [C_BYTE,     0x34]),
    t!(XOR,     T_REG|T_MEM|T_BITS_8,        T_IMM|T_BITS_8,              [C_BYTE,     0x80]),
    t!(XOR,     T_REG|T_REG_AL_AX|T_BITS_16, T_IMM|T_BITS_16,             [C_BYTE,     0x35]),
    t!(XOR,     T_REG|T_MEM|T_BITS_16,       T_IMM|T_BITS_16,             [C_BYTE,     0x81]),
    t!(XOR,     T_REG|T_MEM|T_BITS_8,        T_REG|T_BITS_8,              [C_BYTE,     0x30]),
    t!(XOR,     T_REG|T_MEM|T_BITS_16,       T_REG|T_BITS_16,             [C_BYTE,     0x31]),
    t!(XOR,     T_REG|T_BITS_8,              T_REG|T_MEM|T_BITS_8,        [C_BYTE,     0x32, C_MOD_REG_RM]),
    t!(XOR,     T_REG|T_BITS_16,             T_REG|T_MEM|T_BITS_16,       [C_BYTE,     0x33]),
];

pub fn find_match(
    operation: Operation,
    dst: TypeFlags,
    src: TypeFlags,
) -> Option<&'static Template> {
    let (dst_size, src_size) = match (size::only(dst), size::only(src)) {
        (d, 0) if d != 0 => (d, d),
        (0, s) if s != 0 => (s, s),
        (d, s) if d != 0 && s != 0 => (d, s),
        (0, 0) => (0, 0),
        _ => unreachable!(),
    };

    for t in TEMPLATES {
        if t.operation != operation {
            continue;
        }

        // dst

        if t.dst == T_NONE && dst != T_NONE {
            continue;
        }

        if class::only(dst) != class::only(t.dst) {
            continue;
        }
        if size::only(t.dst) != 0 && dst_size != size::only(t.dst) {
            continue;
        }

        // src

        if t.src == T_NONE && src != T_NONE {
            continue;
        }

        if class::only(src) != class::only(t.src) {
            continue;
        }
        if size::only(t.src) != 0 && src_size != size::only(t.src) {
            continue;
        }

        return Some(t);
    }
    None
}

fn operand_to_type(operand: &Operand) -> TypeFlags {
    match operand {
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

fn format_type_flags(flags: TypeFlags) -> String {
    let mut parts = vec![];

    if flags & T_IMM != 0 {
        parts.push("IMM");
    }

    if flags & T_REG != 0 {
        parts.push("REG");
    }

    if flags & T_MEM != 0 {
        parts.push("MEM");
    }

    if flags & T_BITS_8 != 0 {
        parts.push("BITS_8");
    }

    if flags & T_BITS_16 != 0 {
        parts.push("BITS_16");
    }

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

macro_rules! reg {
    (al) => {{
        Operand::Register(SizedRegisterEncoding(
            RegisterEncoding::AlAx,
            OperandSize::Byte,
        ))
    }};

    (ax) => {{
        Operand::Register(SizedRegisterEncoding(
            RegisterEncoding::AlAx,
            OperandSize::Word,
        ))
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
        let tests: &[(Instruction, &[Code])] = &[
            (insn!(AAA), &[C_BYTE, 0x37]),
            (insn!(AAD), &[C_BYTE, 0xD5, C_BYTE, 0xA0]),
            (insn!(AAD, imm8!(0x10)), &[C_BYTE, 0xD5, C_IMM_BYTE]),
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

                let max = std::cmp::min(codes.len(), m.codes.len());

                assert_eq!(
                    codes[..max],
                    m.codes[..max],
                    "Incorrect codes for [{}]",
                    instruction
                );
            }
        }
    }
}
