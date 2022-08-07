mod codes;
mod op_flags;
mod template;
mod templates;

use crate::op_flags::OpFlags;
use crate::template::Template;
use crate::templates::TEMPLATES;
use mrc_instruction::Operation;

#[derive(Debug)]
pub enum MatchError {
    InvalidOperation,
}

fn find_in_templates<'a>(
    templates: &'a [Template],
    op: Operation,
    _dst: OpFlags,
    _src: OpFlags,
) -> Result<Option<&'a Template<'a>>, MatchError> {
    for template in templates {
        if template.op != op {
            return Err(MatchError::InvalidOperation);
        }
    }

    Ok(None)
}

#[inline(always)]
pub fn find_template(
    op: Operation,
    dst: OpFlags,
    src: OpFlags,
) -> Result<Option<&'static Template<'static>>, MatchError> {
    find_in_templates(TEMPLATES, op, dst, src)
}

#[cfg(test)]
mod tests {
    use crate::{find_in_templates, Template};
    use mrc_instruction::Operation;

    #[test]
    fn basic() {
        let template = find_in_templates(
            &[Template {
                op: Operation::STI,
                dst: 0,
                src: 0,
                codes: &[],
            }],
            Operation::STI,
            0,
            0,
        )
        .unwrap()
        .unwrap();

        assert_eq!(Operation::STI, template.op);
    }
}
