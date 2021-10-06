use std::fmt;

use crate::modrm::RegisterOrMemory;

#[derive(PartialEq, Debug)]
pub enum Error {
    CouldNotCreateOperandFromModRmEncoding(RegisterOrMemory),
    CouldNotReadExtraBytes,
    InvalidDataSizeEncoding(u8),
    InvalidIndirectMemoryEncoding(u8),
    InvalidModRmEncoding(u8),
    InvalidModRmMode(u8),
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidSegmentEncoding(u8),
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::InvalidOpCode(op_code) => write!(f, "invalid op code: {:#04X}", op_code),
            Error::InvalidModRmEncoding(mod_rm_byte) => {
                write!(f, "invalid modR/M encoding: {:#04X}", mod_rm_byte)
            }
            Error::CouldNotCreateOperandFromModRmEncoding(ref register_or_memory) => {
                write!(f, "Could not create operand from mod reg r/m encoding. ({:?})", register_or_memory)
            }
            Error::CouldNotReadExtraBytes => {
                write!(f, "Could not fetch extra bytes from bus.")
            }
            Error::InvalidDataSizeEncoding(byte) => {
                write!(f, "Could not determine data size from encoding ({:02x})", byte)
            }
            _ => todo!()
        }
    }
}
