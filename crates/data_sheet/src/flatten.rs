#![allow(dead_code)]

#[path = "../../mrc_instruction/src/template/op_flags.rs"]
mod op_flags;

use crate::Category;
use op_flags::*;

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
            "Memory to Accumulator" => (T_REG_AL, T_MEM),
            "Accumulator to Memory" => (T_MEM, T_REG | T_REG_AX),
            "Register/Memory to Segment Register" => (T_REG | T_BITS_16, T_REG | T_MEM),
            "Segment Register to Register/Memory" => (T_REG | T_MEM, T_REG | T_BITS_16),
            "Register/Memory" => (T_REG | T_MEM, T_NONE),
            "Register" => (T_REG, T_NONE),
            "Segment Register" => (T_REG | T_BITS_16, T_NONE),
            "Register/Memory with Register" => (T_REG, T_REG | T_MEM),
            "Register with Accumulator" => (T_REG_ACCUM, T_REG),
            "Fixed Port" => match mnemonic {
                "in" => (T_REG | T_REG_ACCUM, T_IMM | T_BITS_8),
                "out" => (T_IMM | T_BITS_8, T_REG | T_REG_ACCUM),
                _ => unreachable!(),
            },
            "Variable Port" => match mnemonic {
                "in" => (T_REG | T_REG_ACCUM, T_REG | T_REG | T_BITS_16),
                "out" => (T_REG | T_REG | T_BITS_16, T_REG | T_REG_ACCUM),
                _ => unreachable!(),
            },
            "" => (T_NONE, T_NONE),
            "Reg./Memory with Register to Either" => (T_REG, T_REG | T_MEM),
            "Immediate to Accumulator" => (T_REG | T_REG_ACCUM, T_IMM),
            "Reg./Memory and Register to Either" => (T_REG, T_REG | T_MEM),
            "Immediate from Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate from Accumulator" => (T_REG | T_REG_ACCUM, T_IMM),
            "Register/memory" => (T_REG | T_MEM, T_NONE),
            "Register/Memory and Register" => (T_REG, T_REG | T_MEM),
            "Immediate with Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate with Accumulator" => (T_REG | T_REG_ACCUM, T_IMM),
            "Immediate Data and Register/Memory" => (T_REG | T_MEM, T_IMM),
            "Immediate Data and Accumulator" => (T_REG | T_REG_ACCUM, T_IMM),
            "Direct Within Segment" => (T_IMM | T_BITS_16, T_NONE),
            "Indirect Within Segment" => (T_REG | T_MEM | T_BITS_16, T_NONE),
            "Direct Intersegment" => todo!(),
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

#[derive(Debug)]
pub struct Line {
    mnemonic: String,
    destination: String,
    source: String,
    codes: Vec<String>,
}

fn collapse_codes(codes: &[String], replace: &str, find: &[&str]) -> Vec<String> {
    codes
        .iter()
        .filter(|&s| !find[1..].contains(&s.as_str()))
        .map(|s| {
            if s.as_str() == find[0] {
                replace.to_owned()
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

fn bytes_to_codes(bytes: Vec<String>) -> Vec<String> {
    bytes
}

fn push_line(
    lines: &mut Vec<Line>,
    mnemonic: String,
    destination: OpFlags,
    source: OpFlags,
    codes: Vec<String>,
) {
    lines.push(Line {
        mnemonic,
        destination: format_type_flags(destination),
        source: format_type_flags(source),
        codes,
    })
}

pub fn flatten(categories: &[Category]) -> Vec<Line> {
    let mut lines = vec![];

    for category in categories.iter() {
        // println!("Category: {}", &category.name);

        for instruction in category.instructions.iter() {
            // println!(
            //     "    Instruction: {} - {}",
            //     &instruction.mnemonic, &instruction.description
            // );

            for encoding in instruction.encodings.iter() {
                // println!("        {:?}", encoding);

                // Clean up codes.
                let clean_bytes = collapse_codes(
                    &encoding.bytes[..],
                    "data",
                    &["data-low", "data-high", "data if w = 1", "data if s:w = 01"],
                );
                let clean_bytes =
                    collapse_codes(&clean_bytes[..], "offset", &["offset-low", "offset-high"]);
                let clean_bytes =
                    collapse_codes(&clean_bytes[..], "segment", &["seg-low", "seg-high"]);
                let clean_bytes = collapse_codes(&clean_bytes[..], "disp8", &["disp"]);
                let clean_bytes =
                    collapse_codes(&clean_bytes[..], "disp16", &["disp-low", "disp-high"]);
                let clean_bytes =
                    collapse_codes(&clean_bytes[..], "addr", &["addr-low", "addr-high"]);

                if clean_bytes[0].ends_with(" d w") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 0");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        set_size(src, T_BITS_8),
                        set_size(dst, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 1");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        set_size(src, T_BITS_16),
                        set_size(dst, T_BITS_16),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 0");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 1");
                    assert_ne!(src, 0);
                    assert_ne!(dst, 0);
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
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
                    // let mut bytes = clean_bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 0 0");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.clone(),
                    //     set_size(dst, T_BITS_8),
                    //     set_size(src, T_BITS_8),
                    //     bytes_to_codes(bytes),
                    // );
                    //
                    // let mut bytes = clean_bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 0 1");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.clone(),
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
                    // let mut bytes = clean_bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 1 1");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.clone(),
                    //     set_size(dst, T_BITS_16),
                    //     set_size(src, T_BITS_8) | T_SIGNED,
                    //     bytes_to_codes(bytes),
                    // );
                } else if encoding.bytes[0].ends_with(" v w") {
                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        T_REG | T_MEM | T_BITS_8,
                        T_UNITY,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        T_REG | T_MEM | T_BITS_16,
                        T_UNITY | T_BITS_8,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        T_REG | T_MEM | T_BITS_8,
                        T_REG_CL,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
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

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 0");

                    push_line(
                        &mut lines,
                        mnemonic.clone(),
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

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 1");

                    push_line(
                        &mut lines,
                        mnemonic.clone(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" w reg") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 0 reg");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 1 reg");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
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
                            instruction.mnemonic.clone(),
                            set_size(dst, T_BITS_16),
                            T_NONE,
                            bytes_to_codes(clean_bytes.clone()),
                        );
                    } else {
                        push_line(
                            &mut lines,
                            instruction.mnemonic.clone(),
                            dst,
                            src,
                            bytes_to_codes(clean_bytes.clone()),
                        );
                    }
                } else if encoding.bytes[0].contains(" reg ") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" reg ", "0 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        dst | T_SEG_ES,
                        src,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" reg ", "0 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        dst | T_SEG_CS,
                        src,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" reg ", "1 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        dst | T_SEG_SS,
                        src,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" reg ", "1 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.clone(),
                        dst | T_SEG_DS,
                        src,
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" z") {
                    let (dst, src) = dst_and_src_from_string(
                        instruction.mnemonic.as_str(),
                        encoding.operands.as_str(),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 0");
                    push_line(
                        &mut lines,
                        "REP".to_owned(),
                        dst,
                        src,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = clean_bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 1");
                    push_line(
                        &mut lines,
                        "REPNE".to_owned(),
                        dst,
                        src,
                        bytes_to_codes(bytes),
                    );
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

                    let bytes = clean_bytes.clone();

                    let mnemonic = if encoding.bytes[0] == "1 1 0 0 1 1 0 0"
                        && instruction.mnemonic == "INT"
                    {
                        "INT3".to_owned()
                    } else {
                        instruction.mnemonic.clone()
                    };

                    // let dst = if (mnemonic == "PUSH" || mnemonic == "POP")
                    //     && dst.contains(T_REG | T_MEM)
                    // {
                    //     set_size(dst, T_BITS_16)
                    // } else {
                    //     dst
                    // };
                    //
                    // let dst = if mnemonic == "MOV"
                    //     && src.contains(T_SEG)
                    //     && dst.contains(T_REG | T_MEM)
                    // {
                    //     set_size(dst, T_BITS_16)
                    // } else {
                    //     dst
                    // };
                    //
                    // let src = if mnemonic == "MOV"
                    //     && dst.contains(T_SEG)
                    //     && src.contains(T_REG | T_MEM)
                    // {
                    //     set_size(src, T_BITS_16)
                    // } else {
                    //     src
                    // };

                    match u8::from_str_radix(bytes[0].replace(' ', "").as_str(), 2) {
                        Ok(_) => push_line(
                            &mut lines,
                            mnemonic.clone(),
                            dst,
                            src,
                            bytes_to_codes(bytes),
                        ),
                        _ => todo!("op_code: {}", &encoding.bytes[0]),
                    }
                }
            }

            // lines.sort();

            // for line in lines {
            //     let s = if line.destination == T_NONE && line.source == T_NONE {
            //         format!(
            //             "    t!({}, [{}]),\n",
            //             line.mnemonic,
            //             codes_to_string(&line.codes)
            //         )
            //     } else if line.source == T_NONE {
            //         format!(
            //             "    t!({}, {}, [{}]),\n",
            //             line.mnemonic,
            //             format_type_flags(line.destination),
            //             codes_to_string(&line.codes)
            //         )
            //     } else {
            //         format!(
            //             "    t!({}, {}, {}, [{}]),\n",
            //             line.mnemonic,
            //             format_type_flags(line.destination),
            //             format_type_flags(line.source),
            //             codes_to_string(&line.codes)
            //         )
            //     };
            //
            //     writer.write_all(s.as_bytes()).unwrap();
            // }
        }
    }

    lines
}
