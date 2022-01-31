//! This crate holds all the structs and constants to represent an instruction for the 8086
//! microprocessor (and friends).

mod address;
mod instruction;
mod operation;
mod register;

pub use address::Address;
pub use instruction::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandKind,
    Repeat,
};
pub use operation::Operation;
pub use register::{Register, Segment, SizedRegister};
