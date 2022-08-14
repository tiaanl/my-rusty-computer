#![allow(dead_code)]

use regex::Regex;
use std::io::Write;

#[derive(Debug)]
pub struct Line {
    mnemonic: String,
    operands: String,
    codes: String,
    other: String,
}

fn parse_lines(source: &str) -> Vec<Line> {
    let skip_line = Regex::new(r"^\s*(;.*|)$").unwrap();
    let regex = Regex::new(r"^\s*(\S+)\s+(\S+)\s+(\S+|\[.*])\s+(\S+)\s*$").unwrap();
    source
        .split('\n')
        .filter(|line| !skip_line.is_match(line))
        .map(|line| {
            let captures = regex.captures(line);
            if let Some(captures) = captures {
                Line {
                    mnemonic: captures[1].to_owned(),
                    operands: captures[2].to_owned(),
                    codes: captures[3].to_owned(),
                    other: captures[4].to_owned(),
                }
            } else {
                panic!("Invalid line format! [{}]", line);
            }
        })
        .filter(|line| line.other.find("8086").is_some())
        .filter(|line| line.operands != "ignore")
        .filter(|line| line.codes.find("resb").is_none())
        .filter(|line| {
            line.mnemonic != "EQU"
                && line.mnemonic != "FWAIT"
                && line.mnemonic != "IRETW"
                && line.mnemonic != "Jcc"
                && line.mnemonic != "PAUSE"
                && line.mnemonic != "POPFW"
                && line.mnemonic != "PUSHFW"
                && line.mnemonic != "RETD"
                && line.mnemonic != "RETF"
                && line.mnemonic != "RETFD"
                && line.mnemonic != "RETFW"
                && line.mnemonic != "RETN"
                && line.mnemonic != "RETND"
                && line.mnemonic != "RETNW"
                && line.mnemonic != "RETW"
                && line.mnemonic != "XLATB"
        })
        .collect::<Vec<Line>>()
}

fn print_line(w: &mut impl Write, line: &Line) -> Result<(), std::io::Error> {
    write!(w, "    t!(")?;

    let mnemonic = match line.mnemonic.as_str() {
        "INT03" => "INT3",
        "LOOPE" => "LOOPZ",
        "LOOPNE" => "LOOPNZ",
        "SAL" => "SHL",
        c => c,
    };

    write!(w, "{}", mnemonic)?;
    write!(w, ", ")?;

    // Operands
    let operands = line
        .operands
        .split(',')
        .map(|operand| {
            operand
                .split('|')
                .filter(|s| *s != "void")
                .flat_map(|s| {
                    if s.ends_with("8") {
                        vec![s.replace("8", ""), "bits_8".to_owned()]
                    } else if s.ends_with("16") {
                        vec![s.replace("16", ""), "bits_16".to_owned()]
                    } else {
                        vec![s.to_owned()]
                    }
                })
                .map(|s| match s.as_str() {
                    "mem" => "memory".to_owned(),
                    "memory_offs" => "mem_offs".to_owned(),
                    "imm" => "immediate".to_owned(),
                    "rm" => "reg_mem_gpr".to_owned(),
                    "reg" => "reg_gpr".to_owned(),
                    "sbyteword" => "sign_byte_word".to_owned(),
                    "reg_sreg" => "reg_seg".to_owned(),
                    "reg_es" => "seg_es".to_owned(),
                    "reg_cs" => "seg_cs".to_owned(),
                    "reg_ss" => "seg_ss".to_owned(),
                    "reg_ds" => "seg_ds".to_owned(),
                    _ => s,
                })
                .map(|s| s.to_uppercase())
                .collect::<Vec<String>>()
                .join("|")
        })
        .filter(|s| !s.is_empty())
        .collect::<Vec<String>>();

    write!(w, "{}, ", operands.len())?;
    for operand in operands.iter() {
        write!(w, "{}, ", operand)?;
    }
    for _ in operands.len()..2 {
        write!(w, "0, ")?;
    }

    write!(w, "&[")?;
    let codes_parts = Regex::new(r"^\[(([^\s:]*):*([^\s:]*):|)\s*(.*\S)\s*]$").unwrap();
    let codes_split = Regex::new(r"\s+").unwrap();
    if let Some(captures) = codes_parts.captures(line.codes.as_str()) {
        // let ops = captures.get(2).map(|c| c.as_str().to_owned());
        let codes = captures
            .get(4)
            .map(|c| c.as_str())
            .expect("Line should have codes");

        let codes = codes_split
            .split(codes)
            .map(|s| s.to_owned())
            .collect::<Vec<String>>();

        let mut parts = vec![];
        codes.iter().for_each(|c| match c.as_str() {
            "ib" => parts.push("C_IMM_BYTE".to_owned()),
            "ib,u" => parts.push("C_IMM_BYTE_UNSIGNED".to_owned()),
            "ib,s" => parts.push("C_IMM_BYTE_SIGNED".to_owned()),
            "iw" => parts.push("C_IMM_WORD".to_owned()),
            "iwd" => parts.push("C_IMM_WORD_DWORD".to_owned()),
            "iwdq" => parts.push("C_IMM_WORD_DWORD_QWORD".to_owned()),
            "seg" => parts.push("C_SEG".to_owned()),
            "rel" => parts.push("C_REL".to_owned()),
            "rel8" => parts.push("C_REL_8".to_owned()),
            "repe" => {}
            "o16" => {}
            "o32" => {}
            "a16" => {}
            "hle" => {}
            "odf" => {}
            "adf" => {}
            "hlexr" => {}
            "norexb" => {}
            "nof3" => {}
            "f3i" => {}
            "wait" => {}
            "hlenl" => {}
            "jcc8" => {}
            "jlen" => {}

            c if c.starts_with('/') && c.len() == 2 => match c[1..].chars().next().unwrap() {
                'r' => parts.push("C_MOD_REG_RM".to_owned()),
                _ => {
                    parts.push("C_MOD_RM".to_owned());
                    parts.push(format!("{:#04X}", u8::from_str_radix(&c[1..], 16).unwrap()));
                }
            },

            c if c.ends_with("+r") => {
                let p = c.replace("+r", "");
                let value = u8::from_str_radix(p.as_str(), 16).unwrap();
                parts.push("C_PLUS_REG".to_owned());
                parts.push(format!("{:#04X}", value));
            }

            c if c.ends_with("+c") => {
                let p = c.replace("+c", "");
                let value = u8::from_str_radix(p.as_str(), 16).unwrap();
                parts.push("C_PLUS_C".to_owned());
                parts.push(format!("{:#04X}", value));
            }

            c => match u8::from_str_radix(c, 16) {
                Ok(value) => {
                    parts.push("C_BYTE".to_owned());
                    parts.push(format!("{:#04X}", value))
                }
                Err(_) => todo!("DOIT {}", c),
            },
        });

        write!(w, "{}", parts.join(", "))?;
    }
    write!(w, "]")?;

    writeln!(w, "),")
}

#[cfg(test)]
mod tests {
    use super::{parse_lines, print_line};

    #[test]
    fn comments() {
        let source = include_str!("/home/tilo/code/nasm/x86/insns.dat");
        let lines = parse_lines(source);
        for line in lines.iter() {
            let w = &mut std::io::stdout();
            print_line(w, line).unwrap();
        }
    }
}
