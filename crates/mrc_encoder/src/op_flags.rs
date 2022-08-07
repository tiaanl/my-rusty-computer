#![allow(unused)]

pub type OpFlags = u32;

const fn mask(bits: OpFlags, shift: OpFlags) -> OpFlags {
    ((1 << bits) - 1) << shift
}

const fn bit(bit: OpFlags, shift: OpFlags) -> OpFlags {
    1 << (shift + bit)
}

pub trait HasFlags {
    fn contains(&self, flags: OpFlags) -> bool;
}

impl HasFlags for OpFlags {
    #[inline(always)]
    fn contains(&self, flags: OpFlags) -> bool {
        *self & flags == flags
    }
}

macro_rules! section {
    ($type:ident, $shift:literal, $bits:literal) => {
        #[allow(dead_code)]
        pub mod $type {
            use super::OpFlags;
            const SHIFT: OpFlags = $shift;
            const BITS: OpFlags = $bits;
            const MASK: OpFlags = super::mask(BITS, SHIFT);
            pub const fn bit(bit: OpFlags) -> OpFlags {
                super::bit(bit, SHIFT)
            }
            pub fn only(flags: OpFlags) -> OpFlags {
                flags & MASK
            }
        }
    };
}

section!(op_type, 0, 4);
section!(reg_class, 4, 2);
section!(sub, 6, 4);
section!(size, 11, 5);

macro_rules! flag {
    ($name:ident, $value:expr) => {
        pub const $name: OpFlags = $value;
    };
}

flag!(REGISTER, op_type::bit(0));
flag!(IMMEDIATE, op_type::bit(1));
flag!(REG_MEM, op_type::bit(2)); // Can be used in R/M code.
flag!(MEMORY, op_type::bit(3) | REG_MEM);

flag!(BITS_8, size::bit(0)); // 8-bit byte
flag!(BITS_16, size::bit(1)); // 16-bit word
flag!(FAR, size::bit(2)); // segment:offset
flag!(NEAR, size::bit(3));
flag!(SHORT, size::bit(4)); // relative within -128..127

flag!(UNITY, sub::bit(0) | IMMEDIATE); // value == 1
flag!(SIGNED_BYTE_WORD, sub::bit(1) | IMMEDIATE); // value is in the range -128..127 mod 2^16

flag!(REG_CLASS_GPR, reg_class::bit(0));
flag!(REG_CLASS_SEG, reg_class::bit(1));

flag!(REG_MEM_GPR, REG_CLASS_GPR | REG_MEM); // Integer operand
flag!(REG_GPR, REG_CLASS_GPR | REG_MEM | REGISTER);
flag!(REG_SEG, REG_CLASS_SEG | BITS_16 | REGISTER); // Any segment register

flag!(REG_ACCUM, sub::bit(1) | REG_CLASS_GPR | REG_MEM | REGISTER); // accumulator: AL, AX, EAX, RAX
flag!(
    REG_AL,
    sub::bit(1) | REG_CLASS_GPR | BITS_8 | REG_MEM | REGISTER
);
flag!(
    REG_AX,
    sub::bit(1) | REG_CLASS_GPR | BITS_16 | REG_MEM | REGISTER
);
flag!(
    REG_COUNT,
    sub::bit(5) | sub::bit(2) | REG_CLASS_GPR | REG_MEM | REGISTER
); // counter: CL, CX, ECX, RCX
flag!(
    REG_CL,
    sub::bit(5) | sub::bit(2) | REG_CLASS_GPR | BITS_8 | REG_MEM | REGISTER
);
flag!(
    REG_CX,
    sub::bit(5) | sub::bit(2) | REG_CLASS_GPR | BITS_16 | REG_MEM | REGISTER
);
flag!(
    REG_DL,
    sub::bit(5) | sub::bit(3) | REG_CLASS_GPR | BITS_8 | REG_MEM | REGISTER
); // data: DL, DX, EDX, RDX
flag!(
    REG_DX,
    sub::bit(5) | sub::bit(3) | REG_CLASS_GPR | BITS_16 | REG_MEM | REGISTER
);
flag!(
    REG_HIGH,
    sub::bit(5) | sub::bit(4) | REG_CLASS_GPR | BITS_8 | REG_MEM | REGISTER
); // high regs: AH, CH, DH, BH
flag!(REG_NOTACC, sub::bit(5)); // non-accumulator register
flag!(
    REG8NA,
    sub::bit(5) | REG_CLASS_GPR | BITS_8 | REG_MEM | REGISTER
); //  8-bit non-acc GPR
flag!(
    REG16NA,
    sub::bit(5) | REG_CLASS_GPR | BITS_16 | REG_MEM | REGISTER
); // 16-bit non-acc GPR

flag!(MEM_OFF, sub::bit(0) | MEMORY); // Simple [address] offset - absolute.
