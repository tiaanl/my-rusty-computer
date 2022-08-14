pub const O_DST: u8 = 0x00;
pub const O_SRC: u8 = 0x01 << 7;

pub const C_BYTE: u8 = 0x00;
pub const C_IMM_BYTE: u8 = 0x01;
pub const C_IMM_BYTE_UNSIGNED: u8 = 0x02;
pub const C_IMM_BYTE_SIGNED: u8 = 0x03;
pub const C_IMM_WORD: u8 = 0x04;
pub const C_IMM_WORD_DWORD: u8 = 0x05;
pub const C_IMM_WORD_DWORD_QWORD: u8 = 0x06;
pub const C_PLUS_REG: u8 = 0x07;
pub const C_MOD_REG_RM: u8 = 0x08;
pub const C_MOD_RM: u8 = 0x09;
pub const C_REL_8: u8 = 0x0A;
pub const C_REL_16: u8 = 0x0B;

pub const C_SEG: u8 = 0x0C; // ???
