use crate::codes::Code;
use crate::OpFlags;
use mrc_instruction::Operation;

pub struct Template<'code> {
    pub op: Operation,
    pub dst: OpFlags,
    pub src: OpFlags,
    pub codes: &'code [Code],
}
