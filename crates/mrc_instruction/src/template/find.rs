#![allow(dead_code, unused)]

use super::codes::*;
use super::op_flags::*;
use super::templates::{Template, TEMPLATES};
use crate::Operation;

const PRINT: bool = true;

fn find_in_templates<'a>(
    templates: &'a [Template],
    op: Operation,
    dst: OpFlags,
    src: OpFlags,
) -> Option<&'a Template<'a>> {
    if PRINT {
        println!(
            "finding: {:?} [{}] [{}]",
            op,
            format_type_flags(dst),
            format_type_flags(src)
        );
    }
    for temp in templates {
        macro_rules! check_type_flags {
            ($tf:ident) => {{
                if $tf != T_NONE {
                    if !temp.$tf.contains(op_type::only($tf)) {
                        if PRINT {
                            println!("class does not match");
                        }
                        continue;
                    }

                    if sub_class::only(temp.$tf) != 0 && !$tf.contains(sub_class::only(temp.$tf)) {
                        if PRINT {
                            println!("sub class does not match");
                        }
                        continue;
                    }

                    if size::only(temp.$tf) > 0 && size::only($tf) > size::only(temp.$tf) {
                        if PRINT {
                            println!("size does not match");
                        }
                        continue;
                    }
                }
            }};
        }

        if temp.operation != op {
            continue;
        }

        if PRINT {
            print!(
                "  {:?} [{}] [{}] ",
                temp.operation,
                format_type_flags(temp.dst),
                format_type_flags(temp.src),
            );
        }

        check_type_flags!(dst);
        check_type_flags!(src);

        if PRINT {
            println!();
        }

        return Some(temp);
    }

    None
}

#[inline]
pub fn find(op: Operation, dst: OpFlags, src: OpFlags) -> Option<&'static Template<'static>> {
    find_in_templates(TEMPLATES[op as usize], op, dst, src)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::template::templates::t;

    #[test]
    fn basic() {
        use Operation::*;

        #[rustfmt::skip]
        let templates = &[
            t(ADD, T_REG_GPR|T_BITS_8,  T_MEM,                        &[0x00]),
            t(ADD, T_REG_GPR|T_BITS_8,  T_REG_GPR|T_BITS_8,           &[0x01]),
            t(ADD, T_REG_GPR|T_BITS_16, T_MEM,                        &[0x02]),
            t(ADD, T_REG_GPR|T_BITS_16, T_REG_GPR|T_BITS_16,          &[0x03]),
            t(ADD, T_MEM,               T_REG_GPR|T_BITS_8,           &[0x04]),
            t(ADD, T_REG_GPR|T_BITS_8,  T_REG_GPR|T_BITS_8,           &[0x05]),
            t(ADD, T_MEM,               T_REG_GPR|T_BITS_16,          &[0x06]),
            t(ADD, T_REG_GPR|T_BITS_16, T_REG_GPR|T_BITS_16,          &[0x07]),
            t(ADD, T_RM_GPR|T_BITS_16,  T_IMM|T_BITS_8,               &[0x08]),
            t(ADD, T_REG_AL,            T_IMM,                        &[0x09]),
            t(ADD, T_REG_AX,            T_SIGNED_BYTE_WORD,           &[0x0A]),
            t(ADD, T_REG_AX,            T_IMM,                        &[0x0B]),
            t(ADD, T_RM_GPR|T_BITS_8,   T_IMM,                        &[0x0C]),
            t(ADD, T_RM_GPR|T_BITS_16,  T_SIGNED_BYTE_WORD,           &[0x0D]),
            t(ADD, T_RM_GPR|T_BITS_16,  T_IMM,                        &[0x0E]),
            t(ADD, T_MEM,               T_IMM|T_BITS_8,               &[0x0F]),
            t(ADD, T_MEM,               T_SIGNED_BYTE_WORD|T_BITS_16, &[0x10]),
            t(ADD, T_MEM,               T_IMM|T_BITS_16,              &[0x11]),
            t(ADD, T_RM_GPR|T_BITS_8,   T_IMM,                        &[0x12]),
        ];

        #[rustfmt::skip]
        let tests: &[(Operation, OpFlags, OpFlags, &[Code])] = &[
            (ADD, T_REG | T_REG_8, T_NONE, &[0x00]),  // mov bl, [0x1234]
        ];

        for test in tests {
            let (operation, destination, source, codes) = *test;
            let m = find(operation, destination, source);
            assert!(
                m.is_some(),
                "No match for [{}, {}, {}]",
                operation,
                format_type_flags(destination),
                format_type_flags(source)
            );
            if let Some(m) = m {
                assert_eq!(
                    operation,
                    m.operation,
                    "Incorrect operation for [{}, {}, {}]",
                    operation,
                    format_type_flags(destination),
                    format_type_flags(source)
                );

                let max = codes.len();

                assert!(
                    m.codes.len() >= max,
                    "Not enough codes for [{}, {}, {}]",
                    operation,
                    format_type_flags(destination),
                    format_type_flags(source)
                );

                assert_eq!(
                    codes[..max],
                    m.codes[..max],
                    "Incorrect codes for [{}, {}, {}] code:[{}] m.codes:[{}]",
                    operation,
                    format_type_flags(destination),
                    format_type_flags(source),
                    codes_to_string(codes),
                    codes_to_string(m.codes),
                );
            }
        }
    }
}
