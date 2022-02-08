use std::io::Error;

#[derive(thiserror::Error, Debug)]
pub enum DecodeError {
    #[error("End of input reached")]
    EndOfInput,

    #[error("Could not fetch extra bytes from stream")]
    CouldNotReadExtraBytes,

    #[error("Could not determine data size from encoding ({0:#04x})")]
    InvalidDataSizeEncoding(u8),

    #[error("Invalid indirect memory encoding ({0:#10b})")]
    InvalidIndirectMemoryEncoding(u8),

    #[error("Invalid modR/M encoding ({0:#04x})")]
    InvalidModRmEncoding(u8),

    #[error("Invalid op code ({0:#04x})")]
    InvalidOpCode(u8),

    #[error("Invalid register encoding ({0:#10b})")]
    InvalidRegisterEncoding(u8),

    #[error("Invalid segment encoding ({0:#10b})")]
    InvalidSegmentEncoding(u8),

    #[error("A prefix was found, but followed by an invalid instruction ({0:#04x})")]
    InvalidPrefix(u8),
}

impl From<std::io::Error> for DecodeError {
    fn from(_: Error) -> Self {
        Self::CouldNotReadExtraBytes
    }
}

pub type Result<T> = std::result::Result<T, DecodeError>;
