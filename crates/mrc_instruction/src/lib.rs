//! This crate holds all the structs and constants to represent an instruction for the 8086
//! microprocessor (and friends).

mod address;
mod instruction;
mod operation;

pub use address::Address;

pub use instruction::{
    AddressingMode, Displacement, Instruction, Operand, OperandSet, OperandSize, OperandType,
    Register, Repeat, Segment, SizedRegister,
};

pub use operation::Operation;
