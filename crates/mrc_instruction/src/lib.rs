//! This crate holds all the structs and constants to represent an instruction for the 8086
//! microprocessor (and friends).

mod address;
pub mod db;
pub mod db2;
mod instruction;
mod operand;
mod operation;
mod register;

pub mod data;

pub use address::{Address, RelativeToAddress};
pub use instruction::{Instruction, OperandSet, Repeat};
pub use operand::{AddressingMode, Displacement, Immediate, Operand, OperandSize};
pub use operation::Operation;
pub use register::{RegisterEncoding, Segment, SizedRegisterEncoding};
