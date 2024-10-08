#![allow(dead_code)]

#[path = "src/template/op_flags.rs"]
mod op_flags;

use op_flags::*;
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::io::Write;
use std::process::Command;

#[path = "src/template/codes.rs"]
mod codes;
use codes::*;

// use data_sheet::get_data_sheet;

#[derive(Eq, PartialEq)]
struct Line {
    mnemonic: String,
    destination: OpFlags,
    source: OpFlags,
    codes: Vec<Code>,
}

impl PartialOrd for Line {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Line {
    fn cmp(&self, other: &Self) -> Ordering {
        fn cmp_type_flags(left: OpFlags, right: OpFlags) -> Ordering {
            match op_type::only(left).cmp(&op_type::only(right)) {
                Ordering::Equal => sub_class::only(right).cmp(&sub_class::only(left)),
                s => s,
            }
        }

        match self.mnemonic.cmp(&other.mnemonic) {
            Ordering::Equal => match cmp_type_flags(self.destination, other.destination) {
                Ordering::Equal => match cmp_type_flags(self.source, other.source) {
                    Ordering::Equal => {
                        size::only(self.destination).cmp(&size::only(other.destination))
                    }
                    s => s,
                },
                s => s,
            },
            s => s,
        }
    }
}

impl Debug for Line {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Line {{ mnemonic: \"{}\", destination: {}, source: {}, codes: {:?} }}",
            self.mnemonic,
            format_type_flags(self.destination),
            format_type_flags(self.source),
            self.codes
        )
    }
}

fn push_line(lines: &mut Vec<Line>, mnemonic: &str, dst: OpFlags, src: OpFlags, codes: Vec<Code>) {
    // if dst != T_NONE && op_type::only(dst) != T_SEG_OFF {
    //     assert!(
    //         op_type::only(dst) != 0 && size::only(dst) != 0,
    //         "{} ({})",
    //         dst,
    //         format_type_flags(dst)
    //     );
    // }

    lines.push(Line {
        mnemonic: mnemonic.to_owned(),
        destination: dst,
        source: src,
        codes,
    });
}

fn data_to(v: Vec<String>, to: &str) -> Vec<String> {
    v.iter()
        .filter(|&s| s.as_str() != "data if w = 1" && s.as_str() != "data if s:w = 01")
        .map(|s| {
            if s.as_str() == "data" {
                to.to_owned()
            } else {
                s.clone()
            }
        })
        .collect()
}

fn data_low_high_to(v: Vec<String>, to: &str) -> Vec<String> {
    v.iter()
        .filter(|&s| s.as_str() != "data-high")
        .map(|s| {
            if s.as_str() == "data-low" {
                to.to_owned()
            } else {
                s.clone()
            }
        })
        .collect()
}

fn disp_to(v: Vec<String>, to: &str) -> Vec<String> {
    v.iter()
        .filter(|&s| s.as_str() != "disp-high")
        .map(|s| {
            if s.as_str() == "disp-low" {
                to.to_owned()
            } else {
                s.clone()
            }
        })
        .collect()
}

fn addr_to(v: Vec<String>, to: &str) -> Vec<String> {
    v.iter()
        .filter(|&s| s.as_str() != "addr-high")
        .map(|s| {
            if s.as_str() == "addr-low" {
                to.to_owned()
            } else {
                s.clone()
            }
        })
        .collect()
}

fn seg_off_to(v: Vec<String>, to: &str) -> Vec<String> {
    v.iter()
        .filter(|&s| {
            s.as_str() != "offset-high" && s.as_str() != "seg-low" && s.as_str() != "seg-high"
        })
        .map(|s| {
            if s.as_str() == "offset-low" {
                to.to_owned()
            } else {
                s.clone()
            }
        })
        .collect()
}

fn set_size(flags: OpFlags, size: OpFlags) -> OpFlags {
    if op_type::only(flags) != 0 {
        // If there is already a size set, then we just leave it.
        if size::only(flags) != 0 {
            flags
        } else {
            flags | size
        }
    } else {
        flags
    }
}

fn byte_to_code(byte: &str) -> Vec<Code> {
    match byte {
        "mod reg r/m" | "mod 0 reg r/m" => vec![C_MOD_REG_RM],
        "mod 0 0 0 r/m" => vec![C_MOD_RM, 0],
        "mod 0 0 1 r/m" => vec![C_MOD_RM, 1],
        "mod 0 1 0 r/m" => vec![C_MOD_RM, 2],
        "mod 0 1 1 r/m" => vec![C_MOD_RM, 3],
        "mod 1 0 0 r/m" => vec![C_MOD_RM, 4],
        "mod 1 0 1 r/m" => vec![C_MOD_RM, 5],
        "mod 1 1 0 r/m" => vec![C_MOD_RM, 6],
        "mod 1 1 1 r/m" => vec![C_MOD_RM, 7],
        "imm8" | "port" | "type" => vec![C_IMM_BYTE],
        "imm16" => vec![C_IMM_WORD],
        "addr" => vec![C_IMM_WORD],
        "simm8" => vec![C_IMM_SIGN_BYTE],
        "disp" => vec![C_DISP_BYTE],
        "disp16" => vec![C_DISP_WORD],
        "seg_off" => vec![C_SEG_OFF],
        _ => match u8::from_str_radix(byte.replace(' ', "").as_str(), 2) {
            Ok(b) => vec![C_BYTE, b],
            Err(_) => {
                if byte.ends_with(" reg") {
                    let byte = byte.replace(" reg", " 0 0 0").replace(' ', "");
                    vec![C_REG_BASE, u8::from_str_radix(byte.as_str(), 2).unwrap()]
                } else {
                    todo!("code: {}", byte)
                }
            }
        },
    }
}

fn bytes_to_codes(bytes: Vec<String>) -> Vec<Code> {
    let mut result = vec![];

    bytes.iter().for_each(|b| {
        for c in byte_to_code(b) {
            result.push(c);
        }
    });

    result
}

fn dst_and_src_from_string(mnemonic: &str, s: &str) -> (OpFlags, OpFlags) {
    let mnemonic = mnemonic.to_lowercase();
    let mnemonic = mnemonic.as_str();

    match mnemonic {
        "lds" => (T_REG | T_BITS_16, T_MEM),
        "loop" | "loopnz" | "loopz" => (T_IMM, T_NONE),
        "mul" => (T_REG | T_MEM, T_NONE),
        "div" => (T_REG | T_MEM, T_NONE),
        "imul" => (T_REG | T_MEM, T_NONE),
        "idiv" => (T_REG | T_MEM, T_NONE),

        _ => match s {
            "Register/Memory to/from Register" => (T_REG, T_REG | T_MEM),
            "Immediate to Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate to Register" => (T_REG, T_IMM),
            "Memory to Accumulator" => (T_REG_ACCUM, T_MEM),
            "Accumulator to Memory" => (T_MEM, T_REG_ACCUM),
            "Register/Memory to Segment Register" => (T_REG | T_BITS_16, T_REG | T_MEM),
            "Segment Register to Register/Memory" => (T_REG | T_MEM, T_REG | T_BITS_16),
            "Register/Memory" => (T_REG | T_MEM, T_NONE),
            "Register" => (T_REG, T_NONE),
            "Segment Register" => (T_REG | T_BITS_16, T_NONE),
            "Register/Memory with Register" => (T_REG, T_REG | T_MEM),
            "Register with Accumulator" => (T_REG_ACCUM, T_REG),
            "Fixed Port" => match mnemonic {
                "in" => (T_REG_ACCUM, T_IMM | T_BITS_8),
                "out" => (T_IMM | T_BITS_8, T_REG_ACCUM),
                _ => unreachable!(),
            },
            "Variable Port" => match mnemonic {
                "in" => (T_REG_ACCUM, T_REG_DX),
                "out" => (T_REG_DX, T_REG_ACCUM),
                _ => unreachable!(),
            },
            "" => (T_NONE, T_NONE),
            "Reg./Memory with Register to Either" => (T_REG, T_REG | T_MEM),
            "Immediate to Accumulator" => (T_REG_ACCUM, T_IMM),
            "Reg./Memory and Register to Either" => (T_REG, T_REG | T_MEM),
            "Immediate from Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate from Accumulator" => (T_REG_ACCUM, T_IMM),
            "Register/memory" => (T_REG | T_MEM, T_NONE),
            "Register/Memory and Register" => (T_REG, T_REG | T_MEM),
            "Immediate with Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate with Accumulator" => (T_REG_ACCUM, T_IMM),
            "Immediate Data and Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate Data and Accumulator" => (T_REG_ACCUM, T_IMM),
            "Direct Within Segment" => (T_IMM | T_BITS_16, T_NONE),
            "Indirect Within Segment" => (T_REG | T_MEM | T_BITS_16, T_NONE),
            "Direct Intersegment" => (T_MEM_OFF, T_NONE),
            "Indirect Intersegment" => (T_REG | T_MEM | T_BITS_16, T_NONE),
            // This is a short jump, which we don't handle right now, because we need the "short"
            // keyword, so for now only match on the near jump.
            // "Direct Within Segment-Short" => (T_IMM | T_BITS_8, T_NONE),
            "Direct Within Segment-Short" => (T_NONE, T_NONE),
            "Within Segment" => (T_NONE, T_NONE),
            "Within Seg Adding Immed to SP" => (T_IMM | T_BITS_16, T_NONE),
            "Intersegment" => (T_NONE, T_NONE),
            "Intersegment Adding Immediate to SP" => todo!(),
            "Type Specified" => (T_IMM | T_BITS_8, T_NONE),
            "Type 3" => (T_NONE, T_NONE),
            _ => unreachable!("operands: {}", s),
        },
    }
}

#[inline(always)]
fn write_line(writer: &mut impl Write, line: &str) {
    writer.write_all(line.as_bytes()).unwrap();
}

#[inline(always)]
fn write_lines(writer: &mut impl Write, lines: &[&str]) {
    for line in lines {
        write_line(writer, line);
    }
}

fn generate_templates(writer: &mut impl Write, categories: &[data_sheet::Category]) {
    for category in categories.iter() {
        // println!("Category: {}", &category.name);

        for instruction in category.instructions.iter() {
            // println!(
            //     "    Instruction: {} - {}",
            //     &instruction.mnemonic, &instruction.description
            // );

            let l = format!(
                "#[rustfmt::skip]\npub(crate) const INSTRUCTIONS_{}: &[Template] = &[\n",
                instruction.mnemonic
            );
            writer.write_all(l.as_bytes()).unwrap();

            let mut lines = vec![];

            for encoding in instruction.encodings.iter() {
                // println!("        {:?}", encoding);

                if encoding.bytes[0].ends_with(" d w") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 0");
                    let bytes = data_to(bytes, "imm8");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(src, T_BITS_8),
                        set_size(dst, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 1");
                    let bytes = data_to(bytes, "imm16");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(src, T_BITS_16),
                        set_size(dst, T_BITS_16),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 0");
                    let bytes = data_to(bytes, "imm8");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 1");
                    let bytes = data_to(bytes, "imm16");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" s w") {
                    // let (dst, src) = dst_and_src_from_string(
                    //     instruction.mnemonic.as_str(),
                    //     encoding.operands.as_str(),
                    // );
                    //
                    // let mut bytes = encoding.bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 0 0");
                    // let bytes = data_to(bytes, "imm8");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.as_str(),
                    //     set_size(dst, T_BITS_8),
                    //     set_size(src, T_BITS_8),
                    //     bytes_to_codes(bytes),
                    // );
                    //
                    // let mut bytes = encoding.bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 0 1");
                    // let bytes = data_to(bytes, "imm16");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.as_str(),
                    //     set_size(dst, T_BITS_16),
                    //     set_size(src, T_BITS_16),
                    //     bytes_to_codes(bytes),
                    // );
                    //
                    // // let mut bytes = encoding.bytes.clone();
                    // // bytes[0] = bytes[0].replace(" s w", " 1 0");
                    // // let bytes = data_to(bytes, "imm8");
                    // // push_line(
                    // //     &mut lines,
                    // //     instruction.mnemonic.as_str(),
                    // //     set_size(dst, T_BITS_8),
                    // //     set_size(src, T_BITS_8),
                    // //     bytes_to_codes(bytes),
                    // // );
                    //
                    // let mut bytes = encoding.bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 1 1");
                    // let bytes = data_to(bytes, "simm8");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.as_str(),
                    //     set_size(dst, T_BITS_16),
                    //     set_size(src, T_BITS_8) | T_SIGNED,
                    //     bytes_to_codes(bytes),
                    // );
                } else if encoding.bytes[0].ends_with(" v w") {
                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 0");
                    let bytes = data_to(bytes, "imm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        T_REG | T_MEM | T_BITS_8,
                        T_UNITY | T_BITS_8,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 1");
                    let bytes = data_to(bytes, "simm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        T_REG | T_MEM | T_BITS_16,
                        T_UNITY | T_BITS_8,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 0");
                    let bytes = data_to(bytes, "imm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        T_REG | T_MEM | T_BITS_8,
                        T_REG_CL,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 1");
                    let bytes = data_to(bytes, "imm16");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        T_REG | T_MEM | T_BITS_16,
                        T_REG_CX,
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" w") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mnemonic = match instruction.mnemonic.as_str() {
                        "MOVS" => "MOVSB".to_owned(),
                        "CMPS" => "CMPSB".to_owned(),
                        "SCAS" => "SCASB".to_owned(),
                        "LODS" => "LODSB".to_owned(),
                        "STOS" => "STOSB".to_owned(),
                        _ => instruction.mnemonic.clone(),
                    };

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 0");

                    // let bytes = if T_MEM_DIR.contains(dst) || T_MEM_DIR.contains(src) {
                    //     addr_to(bytes, "imm16")
                    // } else {
                    //     data_to(bytes, "imm8")
                    // };

                    push_line(
                        &mut lines,
                        mnemonic.as_str(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mnemonic = match instruction.mnemonic.as_str() {
                        "MOVS" => "MOVSW".to_owned(),
                        "CMPS" => "CMPSW".to_owned(),
                        "SCAS" => "SCASW".to_owned(),
                        "LODS" => "LODSW".to_owned(),
                        "STOS" => "STOSW".to_owned(),
                        _ => instruction.mnemonic.clone(),
                    };

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 1");

                    // let bytes = if T_MEM_DIR.contains(dst) || T_MEM_DIR.contains(src) {
                    //     addr_to(bytes, "imm16")
                    // } else {
                    //     data_to(bytes, "imm16")
                    // };

                    push_line(
                        &mut lines,
                        mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" w reg") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 0 reg");
                    let bytes = data_to(bytes, "imm8");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 1 reg");
                    let bytes = data_to(bytes, "imm16");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" reg") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    if instruction.mnemonic == "PUSH"
                        || instruction.mnemonic == "POP"
                        || instruction.mnemonic == "XCHG"
                        || instruction.mnemonic == "INC"
                        || instruction.mnemonic == "DEC"
                    {
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            set_size(dst, T_BITS_16),
                            T_NONE,
                            bytes_to_codes(encoding.bytes.clone()),
                        );
                    } else {
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst,
                            src,
                            bytes_to_codes(encoding.bytes.clone()),
                        );
                    }
                } else if encoding.bytes[0].contains(" reg ") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );
                    if (instruction.mnemonic == "PUSH" || instruction.mnemonic == "POP")
                        && dst.contains(T_REG)
                    {
                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 0");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst | T_SEG_ES,
                            src,
                            bytes_to_codes(bytes),
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 1");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst | T_SEG_CS,
                            src,
                            bytes_to_codes(bytes),
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 0");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst | T_SEG_SS,
                            src,
                            bytes_to_codes(bytes),
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 1");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst | T_SEG_DS,
                            src,
                            bytes_to_codes(bytes),
                        );
                    } else {
                        todo!()
                    }
                } else if encoding.bytes[0].ends_with(" z") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 0");
                    push_line(&mut lines, "REP", dst, src, bytes_to_codes(bytes));

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 1");
                    push_line(&mut lines, "REPNE", dst, src, bytes_to_codes(bytes));
                } else if encoding.bytes[0].ends_with(" x x x") {
                    // skipping ESC
                } else {
                    let (dst, src) = if instruction.description.starts_with("Jump on ") {
                        (T_IMM, T_NONE)
                    } else {
                        dst_and_src_from_string(
                            instruction.mnemonic.as_str(),
                            encoding.operands.as_str(),
                        )
                    };

                    let bytes = encoding.bytes.clone();

                    let mut bytes = if instruction.mnemonic == "RET"
                        || instruction.mnemonic == "CALL"
                        || instruction.mnemonic == "JMP"
                    {
                        seg_off_to(
                            disp_to(data_low_high_to(bytes, "disp16"), "disp16"),
                            "seg_off",
                        )
                    } else {
                        bytes
                    };

                    let mnemonic = if encoding.bytes[0] == "1 1 0 0 1 1 0 0"
                        && instruction.mnemonic == "INT"
                    {
                        "INT3".to_owned()
                    } else {
                        instruction.mnemonic.clone()
                    };

                    let dst = if (mnemonic == "PUSH" || mnemonic == "POP")
                        && dst.contains(T_REG | T_MEM)
                    {
                        set_size(dst, T_BITS_16)
                    } else {
                        dst
                    };

                    let dst = if mnemonic == "MOV"
                        && src.contains(T_REG)
                        && dst.contains(T_REG | T_MEM)
                    {
                        set_size(dst, T_BITS_16)
                    } else {
                        dst
                    };

                    let src = if mnemonic == "MOV"
                        && dst.contains(T_REG)
                        && src.contains(T_REG | T_MEM)
                    {
                        set_size(src, T_BITS_16)
                    } else {
                        src
                    };

                    bytes[0] = bytes[0].replace(' ', "");

                    match u8::from_str_radix(bytes[0].as_str(), 2) {
                        Ok(_) => push_line(
                            &mut lines,
                            mnemonic.as_str(),
                            dst,
                            src,
                            bytes_to_codes(bytes),
                        ),
                        _ => todo!("op_code: {}", &encoding.bytes[0]),
                    }
                }
            }

            lines.sort();

            for line in lines {
                let s = if line.destination == T_NONE && line.source == T_NONE {
                    format!(
                        "    t!({}, [{}]),\n",
                        line.mnemonic,
                        codes_to_string(&line.codes)
                    )
                } else if line.source == T_NONE {
                    format!(
                        "    t!({}, {}, [{}]),\n",
                        line.mnemonic,
                        format_type_flags(line.destination),
                        codes_to_string(&line.codes)
                    )
                } else {
                    format!(
                        "    t!({}, {}, {}, [{}]),\n",
                        line.mnemonic,
                        format_type_flags(line.destination),
                        format_type_flags(line.source),
                        codes_to_string(&line.codes)
                    )
                };

                writer.write_all(s.as_bytes()).unwrap();
            }

            writer.write_all("];\n\n".as_bytes()).unwrap();
        }
    }

    writer
        .write_all("pub const TEMPLATES: &[&[Template]] = &[\n".as_bytes())
        .unwrap();

    for category in categories {
        for instruction in &category.instructions {
            writer
                .write_all(format!("    INSTRUCTIONS_{},\n", instruction.mnemonic).as_bytes())
                .unwrap();
        }
    }

    writer.write_all("];\n".as_bytes()).unwrap();
}

fn generate_operations(writer: &mut impl Write, categories: &[data_sheet::Category]) {
    writer
        .write_all(
            "#[derive(Clone, Copy, Debug, PartialEq, Eq)]\npub enum Operation {\n".as_bytes(),
        )
        .unwrap();

    let mut first_category = true;
    for category in categories {
        if category.instructions.is_empty() {
            continue;
        }

        if first_category {
            first_category = false;
        } else {
            writer.write_all("\n".as_bytes()).unwrap();
        }

        write_line(writer, format!("    // {}\n", category.name).as_str());

        for instruction in &category.instructions {
            write_line(
                writer,
                format!(
                    "    {}, // {}\n",
                    &instruction.mnemonic, &instruction.description
                )
                .as_str(),
            );
        }
    }

    write_lines(writer, &[]);

    write_line(writer, "}\n\nconst MNEMONICS_STR: &[&str] = &[\n");

    for category in categories.iter() {
        for instruction in category.instructions.iter() {
            write_line(
                writer,
                format!("    \"{}\",\n", instruction.mnemonic).as_str(),
            );
        }
    }

    write_line(writer, "];\n\n");

    write_lines(
        writer,
        &[
            "impl std::fmt::Display for Operation {\n",
            "    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {\n",
            "        write!(f, \"{}\", MNEMONICS_STR[*self as usize])\n",
            "    }\n",
            "}\n",
            "\n",
            "impl std::str::FromStr for Operation {\n",
            "    type Err = ();\n",
            "\n",
            "    fn from_str(s: &str) -> Result<Self, Self::Err> {\n",
            "        use Operation::*;\n",
            "\n",
            "        Ok(match s.to_string().to_lowercase().as_str() {\n",
        ],
    );

    for category in categories.iter() {
        for instruction in category.instructions.iter() {
            write_line(
                writer,
                format!(
                    "            \"{}\" => {},\n",
                    instruction.mnemonic.to_lowercase(),
                    instruction.mnemonic
                )
                .as_str(),
            );

            for alias in instruction.aliases.iter() {
                write_line(
                    writer,
                    format!(
                        "            \"{}\" => {},\n",
                        alias.to_lowercase(),
                        instruction.mnemonic
                    )
                    .as_str(),
                );
            }
        }
    }

    write_lines(
        writer,
        &[
            "            _ => return Err(()),\n",
            "        })\n",
            "    }\n",
            "}\n",
        ],
    );
}

const ABOVE: &str = "//! This file is generated by [build.rs]. DO NOT MODIFY.

use crate::{Operation};
use super::type_flags::*;
use super::codes::*;

pub struct Template {
    pub operation: Operation,
    pub dst: TypeFlags,
    pub src: TypeFlags,
    pub codes: &'static [Code],
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            \"{} [{}, {}] [{}]\",
            self.operation,
            format_type_flags(self.dst),
            format_type_flags(self.src),
            codes_to_string(self.codes),
        )
    }
}

macro_rules! t {
    ($operation:ident, [$($more:expr),+]) => {{
        Template {
            operation: Operation::$operation,
            dst: T_NONE,
            src: T_NONE,
            codes: &[$($more),*],
        }
    }};

    ($operation:ident, $dst:expr, [$($more:expr),+]) => {{
        Template {
            operation: Operation::$operation,
            dst: $dst,
            src: T_NONE,
            codes: &[$($more),*],
        }
    }};

    ($operation:ident, $dst:expr, $src:expr, [$($more:expr),+]) => {{
        Template {
            operation: Operation::$operation,
            dst: $dst,
            src: $src,
            codes: &[$($more),*],
        }
    }};
}
";

fn run_rustfmt(path: impl ToString) {
    Command::new("rustfmt")
        .arg(path.to_string())
        .status()
        .unwrap();
}

fn main() {
    // println!("cargo:rerun-if-changed=build.rs");
    // println!("cargo:rerun-if-changed=src/template/templates.rs");
    //
    // let data_sheet = get_data_sheet();
    //
    // let mut file = std::fs::File::create("src/template/templates.rs").unwrap();
    // file.write_all(ABOVE.as_bytes()).unwrap();
    // generate_templates(&mut file, data_sheet.as_slice());
    // run_rustfmt("src/template/templates.rs");
    //
    // let mut file = std::fs::File::create("src/operation2.rs").unwrap();
    // generate_operations(&mut file, data_sheet.as_slice());
    // run_rustfmt("src/operation2.rs");
}
