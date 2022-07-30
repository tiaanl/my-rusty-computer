#![allow(unused)]

use crate::EncodeError::InvalidOperands;
use mrc_instruction::{
    template::codes::*, Displacement, Instruction, Operand, OperandSet, OperandSize, Operation,
    RegisterEncoding, Segment, SizedRegisterEncoding,
};
use std::convert::Infallible;
use std::hash::Hasher;

#[derive(Debug)]
pub enum EncodeError {
    InvalidOperands,
}

// pub fn size_in_bytes(instruction: &Instruction) -> Result<u16, EncodeError> {
//     let template = match mrc_instruction::template::find(instruction) {
//         Some(template) => template,
//         None => return Err(EncodeError::InvalidOperands),
//     };
//
//     println!("template: {}", template);
//
//     let mut size = 0;
//     let mut i = 0;
//     loop {
//         let code = template.codes[i];
//         match code {
//             C_END => break,
//
//             C_BYTE => {
//                 size += 1;
//                 i += 1;
//             }
//
//             C_REG_BASE => {
//                 size += 1;
//                 i += 1;
//             }
//
//             C_MOD_RM => todo!(),
//
//             C_MOD_REG_RM => size += 1,
//
//             C_IMM_BYTE => todo!(),
//
//             C_IMM_WORD => size += 2,
//
//             C_IMM_BYTE_SIGN => todo!(),
//             C_IMM_WORD_SIGN => todo!(),
//             C_DISP_BYTE => todo!(),
//
//             C_DISP_WORD => size += 2,
//
//             C_SEG_OFF => todo!(),
//
//             _ => todo!(),
//         }
//
//         i += 1;
//     }
//
//     Ok(size)
// }
