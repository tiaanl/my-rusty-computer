use crate::codes::*;
use crate::emitter::Emitter;
use crate::{Instruction, OpFlags};
use mrc_instruction::Operation;
use std::convert::Infallible;

#[derive(Debug)]
pub struct Template<'code> {
    pub op: Operation,
    pub oprs_count: u8,
    pub oprs: [OpFlags; 2],
    pub codes: &'code [u8],
}

impl Template<'_> {
    pub fn size_in_bytes(&self, _ins: &Instruction) -> u8 {
        let mut total = 0;

        let mut it = self.codes.iter();
        while let Some(&code) = it.next() {
            match code {
                C_BYTE => {
                    it.next().unwrap();
                    total += 1;
                }

                C_PLUS_REG => {
                    it.next().unwrap();
                    total += 1;
                }

                C_REL_8 => total += 1,
                C_REL_16 => total += 2,

                C_MOD_REG_RM => {
                    // FIXME: Take memory into account.
                    total += 1;
                }

                c => todo!("code: {:02X}", c),
            }
        }

        total
    }

    pub fn encode(&self, ins: &Instruction, offset: u16, size: u16) -> Result<Vec<u8>, Infallible> {
        let mut out = Emitter(vec![]);

        println!("encoding: {:?}", self);

        let mut it = self.codes.iter();
        while let Some(&code) = it.next() {
            let opr = (code >> 7) as usize;
            match code & 0b01111111 {
                C_BYTE => {
                    let byte = it.next().unwrap();
                    out.byte(*byte);
                }

                C_IMM_BYTE => {
                    let value = ins.oprs[opr].imm;
                    out.immediate_word(value);
                }

                C_REL_8 => {
                    let rel = (ins.oprs[opr].imm as i32) - ((offset as i32) + (size as i32));
                    out.immediate_byte(rel as i8 as u8);
                }

                C_REL_16 => {
                    let rel = (ins.oprs[opr].imm as i32) - ((offset as i32) + (size as i32));
                    out.immediate_word(rel as i16 as u16);
                }

                c => todo!("CODE: {:02X}", c),
            }
        }

        Ok(out.0)
    }
}

#[macro_export]
macro_rules! t {
    ($op:ident, $oprs_count:expr, $dst:expr, $src:expr, $codes:expr) => {{
        Template {
            op: Operation::$op,
            oprs_count: $oprs_count,
            oprs: [$dst, $src],
            codes: $codes,
        }
    }};
}
