pub type OpFlags = u16;

const fn gen_mask(bits: OpFlags, shift: OpFlags) -> OpFlags {
    ((1 << bits) - 1) << shift
}

const fn gen_bit(bit: OpFlags, shift: OpFlags) -> OpFlags {
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

macro_rules! bits_section {
    ($type:ident, $shift:literal, $bits:literal) => {
        #[allow(dead_code)]
        pub mod $type {
            use super::OpFlags;
            const SHIFT: OpFlags = $shift;
            const BITS: OpFlags = $bits;
            const MASK: OpFlags = super::gen_mask(BITS, SHIFT);
            pub const fn gen_bit(bit: OpFlags) -> OpFlags {
                super::gen_bit(bit, SHIFT)
            }
            pub fn only(flags: OpFlags) -> OpFlags {
                flags & MASK
            }
        }
    };
}

bits_section!(op_type, 0, 4);
bits_section!(reg_class, 4, 2);
bits_section!(sub_class, 6, 4);
bits_section!(size, 11, 5);

// class::MASK | sub_class::MASK | size::MASK == OpFlags::MAX

macro_rules! op_type {
    ($name:ident, $value:expr) => {
        pub const $name: OpFlags = $value;
    };
}

pub const T_NONE: OpFlags = 0;

// Operand Types

op_type!(T_REG, op_type::gen_bit(0)); // Register, e.g. AX, CL
op_type!(T_IMM, op_type::gen_bit(1)); // Immediate, e.g. 0x12, 0x1234
op_type!(T_REG_MEM, op_type::gen_bit(2)); // For r/m, e.g. effective address operands.
op_type!(T_MEM, op_type::gen_bit(3) | T_REG_MEM);

op_type!(T_REG_CLASS_GPR, reg_class::gen_bit(0)); // General purpose register.
op_type!(T_REG_CLASS_SEG, reg_class::gen_bit(1)); // Segment register.

// Register Classes

op_type!(T_RM_GPR, T_REG_CLASS_GPR | T_REG_MEM);
op_type!(T_REG_GPR, T_REG_CLASS_GPR | T_REG_MEM | T_REG);
op_type!(T_REG_8, T_REG_CLASS_GPR | T_REG_MEM | T_REG | T_BITS_8);
op_type!(T_REG_16, T_REG_CLASS_GPR | T_REG_MEM | T_REG | T_BITS_16);

// Segment Registers

macro_rules! seg {
    ($name:ident, $f1:literal, $f2:literal) => {
        op_type!(
            $name,
            sub_class::gen_bit($f1) | sub_class::gen_bit($f2) | T_REG_CLASS_SEG | T_REG | T_BITS_16
        );
    };
}

seg!(T_SEG_ES, 0, 2);
seg!(T_SEG_CS, 1, 2);
seg!(T_SEG_SS, 0, 3);
seg!(T_SEG_DS, 1, 3);

// Special General Purpose Registers

op_type!(
    T_REG_ACCUM,
    sub_class::gen_bit(0) | T_REG_CLASS_GPR | T_REG_MEM | T_REG
);
op_type!(T_REG_AL, T_REG_ACCUM | T_BITS_8);
op_type!(T_REG_AX, T_REG_ACCUM | T_BITS_16);

op_type!(
    T_REG_COUNT,
    sub_class::gen_bit(1) | T_REG_CLASS_GPR | T_REG_MEM | T_REG
);
op_type!(T_REG_CL, T_REG_COUNT | T_BITS_8);
op_type!(T_REG_CX, T_REG_COUNT | T_BITS_16);

op_type!(
    T_REG_DL,
    sub_class::gen_bit(2) | T_REG_CLASS_GPR | T_REG_MEM | T_REG | T_BITS_8
);
op_type!(
    T_REG_DX,
    sub_class::gen_bit(2) | T_REG_CLASS_GPR | T_REG_MEM | T_REG | T_BITS_16
);

// Special Effective Address Types

op_type!(T_MEM_OFF, sub_class::gen_bit(0) | T_MEM);

// Special Immediate Types

op_type!(T_UNITY, sub_class::gen_bit(0) | T_IMM); // Value == 1
op_type!(T_SIGNED_BYTE_WORD, sub_class::gen_bit(1) | T_IMM); // Value in range -128..127 mod 2^16

// Sizes

op_type!(T_BITS_8, size::gen_bit(0));
op_type!(T_BITS_16, size::gen_bit(1));
op_type!(T_FAR, size::gen_bit(2));
op_type!(T_NEAR, size::gen_bit(3));
op_type!(T_SHORT, size::gen_bit(4));

pub fn format_type_flags(flags: OpFlags) -> String {
    let mut parts = vec![];

    macro_rules! do_flag {
        ($f:expr, $s:literal) => {{
            if flags.contains($f) {
                parts.push($s);
            }
        }};
    }

    // Class

    do_flag!(T_REG, "T_REG");
    do_flag!(T_IMM, "T_IMM");
    do_flag!(T_REG_MEM, "T_REG_MEM");
    do_flag!(T_MEM, "T_MEM");

    // Size

    do_flag!(T_BITS_8, "T_BITS_8");
    do_flag!(T_BITS_16, "T_BITS_16");

    parts.to_owned().join("|")
}
