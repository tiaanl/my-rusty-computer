use std::fmt::{Display, Formatter};

use crate::bus::Address;
use mrc_decoder::Error as DecodeError;

#[derive(Debug)]
pub enum Error {
    AddressNotMapped(Address),
    AddressOutOfRange(Address),
    DecodeError(DecodeError),
    IllegalDataAccess,
    IllegalInstruction,
}

pub type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::AddressNotMapped(address) => {
                write!(f, "Address not mapped! ({:02x})", address)
            }
            Error::AddressOutOfRange(address) => {
                write!(f, "Address out of range! ({:02x})", address)
            }
            Error::IllegalDataAccess => {
                write!(f, "Trying to access data in invalid way!")
            }
            Error::DecodeError(err) => {
                write!(f, "Could not decode instruction! {}", err)
            }
            Error::IllegalInstruction => {
                write!(f, "Illegal instruction!")
            }
        }
    }
}
