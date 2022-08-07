use crate::op_flags::*;
use crate::Template;
use mrc_instruction::Operation;

macro_rules! t {
    ($op:ident, $dst:expr, $src:expr, $codes:expr) => {{
        Template {
            op: Operation::$op,
            dst: $dst,
            src: $src,
            codes: $codes,
        }
    }};
}

pub const TEMPLATES: &[Template] = &[
    // ADD
    t!(ADD, REG_GPR | BITS_8, MEMORY, &[]),
    t!(ADD, REG_GPR | BITS_8, REG_GPR | BITS_8, &[]),
    t!(ADD, REG_GPR | BITS_16, MEMORY, &[]),
    t!(ADD, REG_GPR | BITS_16, REG_GPR | BITS_16, &[]),
    t!(ADD, MEMORY, REG_GPR | BITS_8, &[]),
    t!(ADD, REG_GPR | BITS_8, REG_GPR | BITS_8, &[]),
    t!(ADD, MEMORY, REG_GPR | BITS_16, &[]),
    t!(ADD, REG_GPR | BITS_16, REG_GPR | BITS_16, &[]),
    t!(ADD, REG_MEM_GPR | BITS_16, IMMEDIATE | BITS_8, &[]),
    t!(ADD, REG_AL, IMMEDIATE, &[]),
    t!(ADD, REG_AX, SIGNED_BYTE_WORD, &[]),
    t!(ADD, REG_AX, IMMEDIATE, &[]),
    t!(ADD, REG_MEM_GPR | BITS_8, IMMEDIATE, &[]),
    t!(ADD, REG_MEM_GPR | BITS_16, SIGNED_BYTE_WORD, &[]),
    t!(ADD, REG_MEM_GPR | BITS_16, IMMEDIATE, &[]),
    t!(ADD, MEMORY, IMMEDIATE | BITS_8, &[]),
    t!(ADD, MEMORY, SIGNED_BYTE_WORD | BITS_16, &[]),
    t!(ADD, MEMORY, IMMEDIATE | BITS_16, &[]),
    t!(ADD, REG_MEM_GPR | BITS_8, IMMEDIATE, &[]),
    // MOV
    t!(MOV, MEMORY, REG_SEG, &[]),
    t!(MOV, REG_GPR | BITS_16, REG_SEG, &[]),
    t!(MOV, REG_SEG, MEMORY, &[]),
    t!(MOV, REG_SEG, REG_GPR | BITS_16, &[]),
    t!(MOV, REG_SEG, REG_GPR | BITS_16, &[]),
    t!(MOV, REG_AL, MEM_OFF, &[]),
    t!(MOV, REG_AX, MEM_OFF, &[]),
    t!(MOV, MEM_OFF, REG_AL, &[]),
    t!(MOV, MEM_OFF, REG_AX, &[]),
    t!(MOV, MEMORY, REG_GPR | BITS_8, &[]),
    t!(MOV, REG_GPR | BITS_8, REG_GPR | BITS_8, &[]),
    t!(MOV, MEMORY, REG_GPR | BITS_16, &[]),
    t!(MOV, REG_GPR | BITS_16, REG_GPR | BITS_16, &[]),
    t!(MOV, REG_GPR | BITS_8, MEMORY, &[]),
    t!(MOV, REG_GPR | BITS_8, REG_GPR | BITS_8, &[]),
    t!(MOV, REG_GPR | BITS_16, MEMORY, &[]),
    t!(MOV, REG_GPR | BITS_16, REG_GPR | BITS_16, &[]),
    t!(MOV, REG_GPR | BITS_8, IMMEDIATE, &[]),
    t!(MOV, REG_GPR | BITS_16, IMMEDIATE, &[]),
    t!(MOV, REG_MEM_GPR | BITS_8, IMMEDIATE, &[]),
    t!(MOV, REG_MEM_GPR | BITS_16, IMMEDIATE, &[]),
    t!(MOV, MEMORY, IMMEDIATE | BITS_8, &[]),
    t!(MOV, MEMORY, IMMEDIATE | BITS_16, &[]),
];
