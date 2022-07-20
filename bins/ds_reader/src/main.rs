use crate::parser::Category;
use std::cmp::Ordering;

mod parser;

#[repr(u8)]
#[derive(Eq, Ord, PartialEq, PartialOrd, Debug)]
enum Operand {
    None,

    Imm8,
    Imm16,

    Acc8,
    Acc16,
    Reg8,
    Reg16,

    SegEs,
    SegCs,
    SegSs,
    SegDs,
    Seg,

    Mem8,
    Mem16,

    SegOff,

    RegMem8,
    RegMem16,

    Disp8,
    Disp16,
}

#[derive(Eq)]
struct Line {
    mnemonic: String,
    destination: Operand,
    source: Operand,
    codes: Vec<String>,
}

fn operand_value(operand: &str) -> Operand {
    match operand {
        "Acc16" => Operand::Acc16,
        "Acc8" => Operand::Acc8,
        "Disp16" => Operand::Disp16,
        "Disp8" => Operand::Disp8,
        "Imm16" => Operand::Imm16,
        "Imm8" => Operand::Imm8,
        "Mem16" => Operand::Mem16,
        "Mem8" => Operand::Mem8,
        "None" => Operand::None,
        "Reg16" => Operand::Reg16,
        "Reg8" => Operand::Reg8,
        "RegMem16" => Operand::RegMem16,
        "RegMem8" => Operand::RegMem8,
        "Seg" => Operand::Seg,
        "SegCs" => Operand::SegCs,
        "SegDs" => Operand::SegDs,
        "SegEs" => Operand::SegEs,
        "SegOff" => Operand::SegOff,
        "SegSs" => Operand::SegSs,
        _ => todo!("operand: {}", operand),
    }
}

impl Ord for Line {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.mnemonic == other.mnemonic {
            if self.source == other.source {
                if self.destination == other.destination {
                    Ordering::Equal
                } else {
                    self.destination.cmp(&other.destination)
                }
            } else {
                self.source.cmp(&other.source)
            }
        } else {
            self.mnemonic.cmp(&other.mnemonic)
        }
    }
}

impl PartialOrd for Line {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Line {
    fn eq(&self, other: &Self) -> bool {
        self.mnemonic == other.mnemonic
            && self.destination == other.destination
            && self.source == other.source
            && self.codes == other.codes
    }
}

fn dst_and_src_from_string(s: &str) -> (&str, &str) {
    match s {
        "Register/Memory to/from Register" => ("Reg", "RegMem"),
        "Immediate to Register/Memory" => ("RegMem", "Imm"),
        "Immediate to Register" => ("Reg", "Imm"),
        "Memory to Accumulator" => ("Acc", "Mem"),
        "Accumulator to Memory" => ("Mem", "Acc"),
        "Register/Memory to Segment Register" => ("Seg", "RegMem"),
        "Segment Register to Register/Memory" => ("RegMem", "Seg"),
        "Register/Memory" => ("RegMem", "None"),
        "Register" => ("Reg", "None"),
        "Segment Register" => ("Seg", "None"),
        "Register/Memory with Register" => ("Reg", "RegMem"),
        "Register with Accumulator" => ("Acc", "Reg"),
        "Fixed Port" => ("Imm", "None"),
        "Variable Port" => ("Imm", "None"),
        "" => ("None", "None"),
        "Reg./Memory with Register to Either" => ("Reg", "RegMem"),
        "Immediate to Accumulator" => ("Acc", "Imm"),
        "Reg./Memory and Register to Either" => ("Reg", "RegMem"),
        "Immediate from Register/Memory" => ("Imm", "RegMem"),
        "Immediate from Accumulator" => ("Imm", "Acc"),
        "Register/memory" => ("RegMem", "None"),
        "Register/Memory and Register" => ("Reg", "RegMem"),
        "Immediate with Register/Memory" => ("RegMem", "Imm"),
        "Immediate with Accumulator" => ("Acc", "Imm"),
        "Immediate Data and Register/Memory" => ("RegMem", "Imm"),
        "Immediate Data and Accumulator" => ("Acc", "Imm"),
        "Direct Within Segment" => ("Disp16", "None"),
        "Indirect Within Segment" => ("RegMem16", "None"),
        "Direct Intersegment" => ("SegOff", "None"),
        "Indirect Intersegment" => ("RegMem16", "None"),
        "Direct Within Segment-Short" => ("Disp8", "None"),
        "Within Segment" => ("None", "None"),
        "Within Seg Adding Immed to SP" => ("Disp16", "None"),
        "Intersegment" => ("None", "None"),
        "Intersegment Adding Immediate to SP" => ("Disp16", "None"),
        "Type Specified" => ("Imm8", "None"),
        "Type 3" => ("None", "None"),
        _ => todo!("operands: {}", s),
    }
}

fn byte_to_code(byte: &str) -> String {
    match byte {
        "mod reg r/m" | "mod 0 reg r/m" => "ModRegRM".to_owned(),
        "mod 0 0 0 r/m" => "ModRM(0)".to_owned(),
        "mod 0 0 1 r/m" => "ModRM(1)".to_owned(),
        "mod 0 1 0 r/m" => "ModRM(2)".to_owned(),
        "mod 0 1 1 r/m" => "ModRM(3)".to_owned(),
        "mod 1 0 0 r/m" => "ModRM(4)".to_owned(),
        "mod 1 0 1 r/m" => "ModRM(5)".to_owned(),
        "mod 1 1 0 r/m" => "ModRM(6)".to_owned(),
        "mod 1 1 1 r/m" => "ModRM(7)".to_owned(),
        "imm8" => "Imm8".to_owned(),
        "imm16" => "Imm16".to_owned(),
        "addr" => "Addr".to_owned(),
        "port" => "Imm8".to_owned(),
        "simm16" => "SignImm8".to_owned(),
        "disp" => "Disp8".to_owned(),
        "disp16" => "Disp16".to_owned(),
        "type" => "Imm8".to_owned(),
        "seg_off" => "SegOff".to_owned(),
        _ => match u8::from_str_radix(byte.replace(' ', "").as_str(), 2) {
            Ok(b) => format!("Byte({:#04X})", b),
            Err(_) => {
                if byte.ends_with(" reg") {
                    let byte = byte.replace(" reg", " 0 0 0").replace(' ', "");
                    format!(
                        "PlusReg({:#04X})",
                        u8::from_str_radix(byte.as_str(), 2).unwrap()
                    )
                } else {
                    todo!("code: {}", byte)
                }
            }
        },
    }
}

fn push_line(lines: &mut Vec<Line>, mnemonic: &str, dst: String, src: String, codes: &[String]) {
    let codes: Vec<String> = codes.iter().map(|c| byte_to_code(c.as_str())).collect();

    lines.push(Line {
        mnemonic: mnemonic.to_owned(),
        destination: operand_value(dst.as_str()),
        source: operand_value(src.as_str()),
        codes: codes.to_owned(),
    });
}

fn op_to_byte(op: &str) -> String {
    if op == "None" {
        op.to_owned()
    } else {
        format!("{}8", op)
    }
}

fn op_to_word(op: &str) -> String {
    if op == "None" {
        op.to_owned()
    } else {
        format!("{}16", op)
    }
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
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(src),
                        op_to_byte(dst),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(src),
                        op_to_word(dst),
                        &data_to(bytes, "imm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "imm16")[..],
                    );
                } else if encoding.bytes[0].ends_with(" s w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 0 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 0 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "simm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 1 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 1 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "imm16")[..],
                    );
                } else if encoding.bytes[0].ends_with(" v w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "simm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 0");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(src),
                        op_to_byte(dst),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 1");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(src),
                        op_to_word(dst),
                        &data_to(bytes, "imm16")[..],
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
                    if dst == "Mem" || src == "Mem" {
                        push_line(
                            &mut lines,
                            mnemonic.as_str(),
                            op_to_byte(dst),
                            op_to_byte(src),
                            &addr_to(bytes, "addr")[..],
                        );
                    } else {
                        push_line(
                            &mut lines,
                            mnemonic.as_str(),
                            op_to_byte(dst),
                            op_to_byte(src),
                            &data_to(bytes, "addr")[..],
                        );
                    }

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
                    if dst == "Mem" || src == "Mem" {
                        push_line(
                            &mut lines,
                            mnemonic.as_str(),
                            op_to_word(dst),
                            op_to_word(src),
                            &addr_to(bytes, "imm16")[..],
                        );
                    } else {
                        push_line(
                            &mut lines,
                            mnemonic.as_str(),
                            op_to_word(dst),
                            op_to_word(src),
                            &data_to(bytes, "imm16")[..],
                        );
                    }
                } else if encoding.bytes[0].ends_with(" w reg") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 0 reg");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 1 reg");
                    push_line(
                        &mut lines,
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "imm16")[..],
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
                            op_to_word(dst),
                            op_to_word(src),
                            &encoding.bytes[..],
                        );
                    } else {
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            dst.to_owned(),
                            src.to_owned(),
                            &encoding.bytes[..],
                        );
                    }
                } else if encoding.bytes[0].contains(" reg ") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());
                    if (instruction.mnemonic == "PUSH" || instruction.mnemonic == "POP")
                        && dst == "Seg"
                    {
                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 0");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            "SegEs".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 1");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            "SegCs".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 0");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            "SegSs".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 1");
                        push_line(
                            &mut lines,
                            instruction.mnemonic.as_str(),
                            "SegDs".to_owned(),
                            src.to_owned(),
                            &bytes[..],
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
                        dst.to_owned(),
                        src.to_owned(),
                        &bytes[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 1");
                    push_line(
                        &mut lines,
                        format!("{}Z", instruction.mnemonic).as_str(),
                        dst.to_owned(),
                        src.to_owned(),
                        &bytes[..],
                    );
                } else if encoding.bytes[0].ends_with(" x x x") {
                    // skipping ESC
                } else {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let codes = encoding.bytes.clone();

                    let codes = if instruction.mnemonic == "RET"
                        || instruction.mnemonic == "CALL"
                        || instruction.mnemonic == "JMP"
                    {
                        seg_off_to(
                            disp_to(data_low_high_to(codes, "disp16"), "disp16"),
                            "seg_off",
                        )
                    } else {
                        codes
                    };

                    let mnemonic = if encoding.bytes[0] == "1 1 0 0 1 1 0 0"
                        && instruction.mnemonic == "INT"
                    {
                        "INT3".to_owned()
                    } else {
                        instruction.mnemonic.clone()
                    };

                    let dst = if (mnemonic == "PUSH" || mnemonic == "POP") && dst == "RegMem" {
                        "RegMem16".to_owned()
                    } else {
                        dst.to_owned()
                    };

                    let dst = if mnemonic == "MOV" && src == "Seg" && dst == "RegMem" {
                        "RegMem16".to_owned()
                    } else {
                        dst
                    };

                    let src = if mnemonic == "MOV" && dst == "Seg" && src == "RegMem" {
                        "RegMem16".to_owned()
                    } else {
                        src.to_owned()
                    };

                    match u8::from_str_radix(encoding.bytes[0].replace(' ', "").as_str(), 2) {
                        Ok(_) => push_line(&mut lines, mnemonic.as_str(), dst, src, &codes[..]),
                        _ => todo!("op_code: {}", &encoding.bytes[0]),
                    }
                }
            }
        }
    }

    lines.sort();

    for line in lines {
        println!(
            "id!({}, {:?}, {:?}, &[{}]),",
            line.mnemonic,
            line.destination,
            line.source,
            line.codes.join(", ")
        );
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
