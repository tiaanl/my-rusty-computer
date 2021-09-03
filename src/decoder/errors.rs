use crate::decoder::mod_rm::RegisterOrMemory;
use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Error {
    CouldNotCreateOperandFromModRMEncoding(RegisterOrMemory),
    CouldNotReadExtraBytes,
    InvalidDataSizeEncoding(u8),
    InvalidIndirectMemoryEncoding(u8),
    InvalidModRMEncoding(u8),
    InvalidModRMMode(u8),
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidSegmentEncoding(u8),
}

pub type Result<T> = std::result::Result<T, Error>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Error::InvalidOpCode(op_code) => write!(f, "invalid op code: {:#04x}", op_code),
            Error::InvalidModRMEncoding(mod_rm_byte) => {
                write!(f, "invalid modR/M encoding: {:#04x}", mod_rm_byte)
            }
            _ => write!(f, "unknown error"),
        }
    }
}
