use std::fmt;

#[derive(PartialEq, Debug)]
pub enum DecodeError {
    CouldNotReadExtraBytes,
    InvalidDataSizeEncoding(u8),
    InvalidIndirectMemoryEncoding(u8),
    InvalidModRmEncoding(u8),
    InvalidOpCode(u8),
    InvalidRegisterEncoding(u8),
    InvalidSegmentEncoding(u8),
}

pub type Result<T> = std::result::Result<T, DecodeError>;

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DecodeError::CouldNotReadExtraBytes => {
                write!(f, "Could not fetch extra bytes from stream")
            }
            DecodeError::InvalidDataSizeEncoding(byte) => write!(
                f,
                "Could not determine data size from encoding ({:#04x})",
                byte
            ),
            DecodeError::InvalidIndirectMemoryEncoding(encoding) => {
                write!(f, "Invalid indirect memory encoding ({:#10b})", encoding)
            }
            DecodeError::InvalidModRmEncoding(encoding) => {
                write!(f, "Invalid modR/M encoding ({:#04x})", encoding)
            }
            DecodeError::InvalidOpCode(op_code) => write!(f, "Invalid op code ({:#04x})", op_code),
            DecodeError::InvalidRegisterEncoding(encoding) => {
                write!(f, "Invalid register encoding ({:#10b})", encoding)
            }
            DecodeError::InvalidSegmentEncoding(encoding) => {
                write!(f, "Invalid segment encoding ({:#10b})", encoding)
            }
        }
    }
}
