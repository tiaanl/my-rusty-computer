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
