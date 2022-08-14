#![allow(dead_code)]

mod codes;
mod emitter;
mod gen;
mod op_flags;
mod template;
mod templates;

use crate::op_flags::*;
use crate::template::Template;
use crate::templates::TEMPLATES;
use mrc_instruction::Operation;

#[derive(Debug, Default)]
pub struct Operand {
    /// The type of operand.
    op_flags: OpFlags,

    /// The segment encoding used for a memory operand.
    seg: Option<u8>,

    /// An immediate value on the operand.
    imm: u16,

    /// A register encoding (gpr or segment)
    reg: u8,
}

impl Operand {
    pub fn immediate(value: u16) -> Self {
        Self {
            op_flags: IMMEDIATE,
            seg: None,
            imm: value,
            reg: 0,
        }
    }

    pub fn register(reg: u8) -> Self {
        Self {
            op_flags: REGISTER,
            seg: None,
            imm: 0,
            reg,
        }
    }

    pub fn direct_address(addr: u16, seg: Option<u8>) -> Self {
        Self {
            op_flags: MEMORY,
            seg,
            imm: addr,
            reg: 0,
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    pub op: Operation,
    pub oprs_count: u8,
    pub oprs: [Operand; 2],
}

impl Instruction {
    pub fn new(op: Operation, oprs_count: u8, dst: Operand, src: Operand) -> Self {
        Self {
            op,
            oprs_count,
            oprs: [dst, src],
        }
    }
}

#[derive(Debug)]
pub enum MatchError {
    InvalidOperation,
}

fn find_in_templates<'a>(
    templates: &'a [Template],
    ins: &Instruction,
) -> Result<Option<&'a Template<'a>>, MatchError> {
    // println!("matching: {:?}", ins);

    for template in templates {
        if template.op != ins.op {
            continue;
        }

        if template.oprs_count != ins.oprs_count {
            continue;
        }

        // Match each operand.
        for i in 0..ins.oprs_count as usize {
            if op_type::only(ins.oprs[i].op_flags) != op_type::only(template.oprs[i]) {
                continue;
            }
        }

        // println!("found: {:?}", template);

        return Ok(Some(template));
    }

    Ok(None)
}

#[inline(always)]
pub fn find_template(ins: &Instruction) -> Result<Option<&'static Template<'static>>, MatchError> {
    find_in_templates(TEMPLATES, ins)
}

#[cfg(test)]
mod tests {
    use crate::op_flags::IMMEDIATE;
    use crate::{find_in_templates, t, Instruction, Operand, Template};
    use mrc_instruction::Operation;

    macro_rules! assert_template {
        ($templates:expr, $code:expr, $op:ident) => {{
            let template = find_in_templates(
                $templates,
                &Instruction::new(Operation::$op, 0, Operand::default(), Operand::default()),
            )
            .unwrap()
            .unwrap();
            assert_eq!($code, template.codes[0]);
        }};

        ($templates:expr, $code:expr, $op:ident, $dst:expr) => {{
            let template = find_in_templates(
                $templates,
                &Instruction::new(Operation::$op, 1, $dst, Operand::default()),
            )
            .unwrap()
            .unwrap();
            assert_eq!($code, template.codes[0]);
        }};
    }

    #[test]
    fn basic() {
        let templates = &[t!(STI, 0, 0, 0, &[0x00]), t!(CLI, 0, 0, 0, &[0x01])];

        assert_template!(templates, 0x00, STI);
        assert_template!(templates, 0x01, CLI);
    }

    #[test]
    fn immediate() {
        let templates = &[t!(JMP, 0, 0, 0, &[0x01]), t!(JMP, 1, IMMEDIATE, 0, &[0x00])];

        assert_template!(templates, 0x00, JMP, Operand::immediate(0x1234));
    }
}
