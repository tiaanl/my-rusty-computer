use crate::parser::Category;
use mrc_instruction::db2::{
    class, codes_to_string, format_type_flags, size, sub_class, Code, HasFlags, TypeFlags, C_BYTE,
    C_DISP_BYTE, C_DISP_WORD, C_IMM_BYTE, C_IMM_BYTE_SIGN, C_IMM_WORD, C_IMM_WORD_SIGN,
    C_MOD_REG_RM, C_MOD_RM, C_REG_BASE, C_SEG_OFF, T_BITS_16, T_BITS_8, T_DISP, T_IMM, T_MEM,
    T_NONE, T_REG, T_REG_AL_AX, T_SEG, T_SEG_CS, T_SEG_DS, T_SEG_ES, T_SEG_OFF, T_SEG_SS, T_SIGNED,
};
use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};

mod parser;

#[derive(Eq, PartialEq)]
struct Line {
    mnemonic: String,
    destination: TypeFlags,
    source: TypeFlags,
    codes: Vec<Code>,
}

impl PartialOrd for Line {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Line {
    fn cmp(&self, other: &Self) -> Ordering {
        fn cmp_type_flags(left: TypeFlags, right: TypeFlags) -> Ordering {
            match class::only(left).cmp(&class::only(right)) {
                Ordering::Equal => match size::only(left).cmp(&size::only(right)) {
                    Ordering::Equal => sub_class::only(right).cmp(&sub_class::only(left)),
                    s => s,
                },
                s => s,
            }
        }

        match self.mnemonic.cmp(&other.mnemonic) {
            Ordering::Equal => match cmp_type_flags(self.destination, other.destination) {
                Ordering::Equal => cmp_type_flags(self.source, other.source),
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

fn dst_and_src_from_string(s: &str) -> (TypeFlags, TypeFlags) {
    match s {
        "Register/Memory to/from Register" => (T_REG, T_REG | T_MEM),
        "Immediate to Register/Memory" => (T_REG | T_MEM, T_IMM),
        "Immediate to Register" => (T_REG, T_IMM),
        "Memory to Accumulator" => (T_REG | T_REG_AL_AX, T_MEM),
        "Accumulator to Memory" => (T_MEM, T_REG | T_REG_AL_AX),
        "Register/Memory to Segment Register" => (T_SEG | T_BITS_16, T_REG | T_MEM),
        "Segment Register to Register/Memory" => (T_REG | T_MEM, T_SEG | T_BITS_16),
        "Register/Memory" => (T_REG | T_MEM, T_NONE),
        "Register" => (T_REG, T_NONE),
        "Segment Register" => (T_SEG | T_BITS_16, T_NONE),
        "Register/Memory with Register" => (T_REG, T_REG | T_MEM),
        "Register with Accumulator" => (T_REG | T_REG_AL_AX, T_REG),
        "Fixed Port" => (T_IMM, T_NONE),
        "Variable Port" => (T_IMM, T_NONE),
        "" => (T_NONE, T_NONE),
        "Reg./Memory with Register to Either" => (T_REG, T_REG | T_MEM),
        "Immediate to Accumulator" => (T_REG | T_REG_AL_AX, T_IMM),
        "Reg./Memory and Register to Either" => (T_REG, T_REG | T_MEM),
        "Immediate from Register/Memory" => (T_IMM, T_REG | T_MEM),
        "Immediate from Accumulator" => (T_IMM, T_REG | T_REG_AL_AX),
        "Register/memory" => (T_REG | T_MEM, T_NONE),
        "Register/Memory and Register" => (T_REG, T_REG | T_MEM),
        "Immediate with Register/Memory" => (T_REG | T_MEM, T_IMM),
        "Immediate with Accumulator" => (T_REG | T_REG_AL_AX, T_IMM),
        "Immediate Data and Register/Memory" => (T_REG | T_MEM, T_IMM),
        "Immediate Data and Accumulator" => (T_REG | T_REG_AL_AX, T_IMM),
        "Direct Within Segment" => (T_DISP | T_BITS_16, T_NONE),
        "Indirect Within Segment" => (T_REG | T_MEM | T_BITS_16, T_NONE),
        "Direct Intersegment" => (T_SEG_OFF, T_NONE),
        "Indirect Intersegment" => (T_REG | T_MEM | T_BITS_16, T_NONE),
        "Direct Within Segment-Short" => (T_DISP | T_BITS_8, T_NONE),
        "Within Segment" => (T_NONE, T_NONE),
        "Within Seg Adding Immed to SP" => (T_DISP | T_BITS_16, T_NONE),
        "Intersegment" => (T_NONE, T_NONE),
        "Intersegment Adding Immediate to SP" => (T_DISP | T_BITS_16, T_NONE),
        "Type Specified" => (T_IMM | T_BITS_8, T_NONE),
        "Type 3" => (T_NONE, T_NONE),
        _ => unreachable!("operands: {}", s),
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
        "addr" => vec![C_IMM_WORD],       // todo,
        "simm8" => vec![C_IMM_BYTE_SIGN], // todo,
        "simm16" => vec![C_IMM_WORD_SIGN],
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

fn push_line(
    lines: &mut Vec<Line>,
    mnemonic: &str,
    dst: TypeFlags,
    src: TypeFlags,
    codes: Vec<Code>,
) {
    if dst != T_NONE && class::only(dst) != T_SEG_OFF {
        assert!(
            class::only(dst) != 0 && size::only(dst) != 0,
            "{} ({})",
            dst,
            format_type_flags(dst)
        );
    }

    lines.push(Line {
        mnemonic: mnemonic.to_owned(),
        destination: dst,
        source: src,
        codes,
    });
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

fn set_size(flags: TypeFlags, size: TypeFlags) -> TypeFlags {
    if flags != T_NONE {
        debug_assert_eq!(size::only(flags), 0);
        flags | size
    } else {
        flags
    }
}

fn generate_table(categories: &[Category]) {
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

                if encoding.bytes[0].ends_with(" d w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

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
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 0 0");
                    let bytes = data_to(bytes, "imm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 0 1");
                    let bytes = data_to(bytes, "imm16");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );

                    // let mut bytes = encoding.bytes.clone();
                    // bytes[0] = bytes[0].replace(" s w", " 1 0");
                    // let bytes = data_to(bytes, "imm8");
                    // push_line(
                    //     &mut lines,
                    //     instruction.mnemonic.as_str(),
                    //     set_size(dst, T_BITS_8),
                    //     set_size(src, T_BITS_8),
                    //     bytes_to_codes(bytes),
                    // );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 1 1");
                    let bytes = data_to(bytes, "simm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16) | T_SIGNED,
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" v w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 0");
                    let bytes = data_to(bytes, "imm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_8),
                        set_size(src, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 1");
                    let bytes = data_to(bytes, "simm16");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 0");
                    let bytes = data_to(bytes, "imm8");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(src, T_BITS_8),
                        set_size(dst, T_BITS_8),
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 1");
                    let bytes = data_to(bytes, "imm16");

                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        set_size(src, T_BITS_16),
                        set_size(dst, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

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

                    let bytes = if T_MEM.contains(dst) || T_MEM.contains(src) {
                        addr_to(bytes, "imm16")
                    } else {
                        data_to(bytes, "imm16")
                    };
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

                    let bytes = if T_MEM.contains(dst) || T_MEM.contains(src) {
                        addr_to(bytes, "imm16")
                    } else {
                        data_to(bytes, "imm16")
                    };

                    push_line(
                        &mut lines,
                        mnemonic.as_str(),
                        set_size(dst, T_BITS_16),
                        set_size(src, T_BITS_16),
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" w reg") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

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
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

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
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());
                    if (instruction.mnemonic == "PUSH" || instruction.mnemonic == "POP")
                        && dst.contains(T_SEG)
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
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        dst,
                        src,
                        bytes_to_codes(bytes),
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 1");
                    push_line(
                        &mut lines,
                        format!("{}Z", instruction.mnemonic).as_str(),
                        dst,
                        src,
                        bytes_to_codes(bytes),
                    );
                } else if encoding.bytes[0].ends_with(" x x x") {
                    // skipping ESC
                } else {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

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
                        && src.contains(T_SEG)
                        && dst.contains(T_REG | T_MEM)
                    {
                        set_size(dst, T_BITS_16)
                    } else {
                        dst
                    };

                    let src = if mnemonic == "MOV"
                        && dst.contains(T_SEG)
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
        }
    }

    // lines.sort_by(|l, r| l.mnemonic.cmp(&r.mnemonic));
    lines.sort();

    for line in lines {
        if line.destination == T_NONE && line.source == T_NONE {
            println!("t!({}, [{}]),", line.mnemonic, codes_to_string(&line.codes));
        } else if line.source == T_NONE {
            println!(
                "t!({}, {}, [{}]),",
                line.mnemonic,
                format_type_flags(line.destination),
                codes_to_string(&line.codes)
            );
        } else {
            println!(
                "t!({}, {}, {}, [{}]),",
                line.mnemonic,
                format_type_flags(line.destination),
                format_type_flags(line.source),
                codes_to_string(&line.codes)
            );
        }
    }
}

fn main() {
    let data_sheet = include_str!("../data_sheet.txt");

    let mut lines = parser::Lines::from_vec(data_sheet.split('\n').map(|s| s.to_owned()).collect());

    let categories = parser::parse_categories(&mut lines);

    // for category in categories {
    //     println!("{}", category.name);
    //     for instruction in category.instructions {
    //         println!("  - {} ({})", instruction.mnemonic, instruction.description);
    //         for encoding in instruction.encodings {
    //             println!("      - {:40}  {:?}", encoding.operands, encoding.bytes);
    //         }
    //     }
    // }

    // categories.iter().for_each(|c| {
    //     c.instructions.iter().for_each(|i| {
    //         i.encodings.iter().for_each(|e| {
    //             println!("{:?}", split_operands(e.operands.clone()));
    //         })
    //     })
    // });

    generate_table(&categories[..]);
}

#[cfg(test)]
mod tests {
    use crate::Line;
    use mrc_instruction::db2::{T_BITS_16, T_BITS_8, T_IMM, T_MEM, T_REG, T_REG_AL_AX};

    macro_rules! line {
        ($mnemonic:literal, $dst:expr, $src: expr) => {{
            Line {
                mnemonic: $mnemonic.to_string(),
                destination: $dst,
                source: $src,
                codes: vec![],
            }
        }};
    }

    #[test]
    fn sorted_on_size() {
        let mut sorted = vec![
            line!("ADC", T_REG | T_BITS_16, T_IMM | T_BITS_16),
            line!("ADC", T_REG | T_BITS_8, T_IMM | T_BITS_8),
        ];
        sorted.sort();
        assert_eq!(
            sorted,
            vec![
                line!("ADC", T_REG | T_BITS_8, T_IMM | T_BITS_8),
                line!("ADC", T_REG | T_BITS_16, T_IMM | T_BITS_16),
            ]
        );
    }

    #[test]
    fn sorted_on_class() {
        let mut sorted = vec![
            line!("ADC", T_REG | T_MEM, T_REG),
            line!("ADC", T_REG | T_REG_AL_AX, T_IMM),
            line!("ADC", T_REG, T_REG | T_MEM),
            line!("ADC", T_REG | T_MEM, T_IMM),
        ];
        sorted.sort();
        assert_eq!(
            sorted,
            vec![
                line!("ADC", T_REG | T_REG_AL_AX, T_IMM),
                line!("ADC", T_REG, T_REG | T_MEM),
                line!("ADC", T_REG | T_MEM, T_IMM),
                line!("ADC", T_REG | T_MEM, T_REG),
            ],
        );
    }
}
