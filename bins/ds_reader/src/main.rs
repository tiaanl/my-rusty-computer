use crate::parser::Category;

mod parser;

fn dst_and_src_from_string(s: &str) -> (&str, &str) {
    match s {
        "Register/Memory to/from Register" => ("reg", "rm"),
        "Immediate to Register/Memory" => ("rm", "imm"),
        "Immediate to Register" => ("reg", "imm"),
        "Memory to Accumulator" => ("acc", "mem"),
        "Accumulator to Memory" => ("mem", "acc"),
        "Register/Memory to Segment Register" => ("seg", "rm"),
        "Segment Register to Register/Memory" => ("rm", "seg"),
        "Register/Memory" => ("rm", "none"),
        "Register" => ("reg", "none"),
        "Segment Register" => ("seg", "none"),
        "Register/Memory with Register" => ("reg", "rm"),
        "Register with Accumulator" => ("acc", "reg"),
        "Fixed Port" => ("imm", "none"),
        "Variable Port" => ("imm", "none"),
        "" => ("none", "none"),
        "Reg./Memory with Register to Either" => ("reg", "rm"),
        "Immediate to Accumulator" => ("acc", "imm"),
        "Reg./Memory and Register to Either" => ("reg", "rm"),
        "Immediate from Register/Memory" => ("imm", "rm"),
        "Immediate from Accumulator" => ("imm", "acc"),
        "Register/memory" => ("rm", "none"),
        "Register/Memory and Register" => ("reg", "rm"),
        "Immediate with Register/Memory" => ("rm", "imm"),
        "Immediate with Accumulator" => ("acc", "imm"),
        "Immediate Data and Register/Memory" => ("rm", "imm"),
        "Immediate Data and Accumulator" => ("acc", "imm"),
        "Direct Within Segment" => ("disp", "none"),
        "Indirect Within Segment" => ("disp", "none"),
        "Direct Intersegment" => ("disp", "none"),
        "Indirect Intersegment" => ("disp", "none"),
        "Direct Within Segment-Short" => ("disp", "none"),
        "Within Segment" => ("disp", "none"),
        "Within Seg Adding Immed to SP" => ("disp", "none"),
        "Intersegment" => ("disp", "none"),
        "Intersegment Adding Immediate to SP" => ("disp", "none"),
        "Type Specified" => ("imm", "none"),
        "Type 3" => ("imm", "none"),
        _ => todo!("operands: {}", s),
    }
}

fn byte_to_code(byte: &str) -> String {
    match byte {
        "mod reg r/m" => "ModRegRM".to_owned(),
        "mod 0 0 0 r/m" => "\\0".to_owned(),
        "mod 0 0 1 r/m" => "\\1".to_owned(),
        "mod 0 1 0 r/m" => "\\2".to_owned(),
        "mod 0 1 1 r/m" => "\\3".to_owned(),
        "mod 1 0 0 r/m" => "\\4".to_owned(),
        "mod 1 0 1 r/m" => "\\5".to_owned(),
        "mod 1 1 0 r/m" => "\\6".to_owned(),
        "mod 1 1 1 r/m" => "\\7".to_owned(),
        "imm8" => "ImmByte".to_owned(),
        "imm16" => "ImmWord".to_owned(),
        "addr-low" => "AddrLow".to_owned(),
        "addr-high" => "AddrHigh".to_owned(),
        "mod 0 reg r/m" => "Seg".to_owned(),
        "port" => "ImmByte".to_owned(),
        "simm16" => "SignImm16".to_owned(),
        "disp-low" => "TODO".to_owned(),
        "disp-high" => "TODO".to_owned(),
        "offset-low" => "TODO".to_owned(),
        "offset-high" => "TODO".to_owned(),
        "seg-low" => "TODO".to_owned(),
        "seg-high" => "TODO".to_owned(),
        "disp" => "TODO".to_owned(),
        "data-low" => "TODO".to_owned(),
        "data-high" => "TODO".to_owned(),
        "type" => "TODO".to_owned(),
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

fn push_line(mnemonic: &str, dst: String, src: String, codes: &[String]) {
    let codes: Vec<String> = codes.iter().map(|c| byte_to_code(c.as_str())).collect();

    println!("{}, {}, {}, {:?}", mnemonic, dst, src, codes);
}

fn op_to_byte(op: &str) -> String {
    format!("{}8", op)
}

fn op_to_word(op: &str) -> String {
    format!("{}16", op)
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

fn generate_table(categories: &[Category]) {
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
                        instruction.mnemonic.as_str(),
                        op_to_byte(src),
                        op_to_byte(dst),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 0 1");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(src),
                        op_to_word(dst),
                        &data_to(bytes, "imm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 0");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" d w", " 1 1");
                    push_line(
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
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 0 1");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "simm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 1 0");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" s w", " 1 1");
                    push_line(
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
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 0 1");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "simm16")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 0");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_byte(src),
                        op_to_byte(dst),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" v w", " 1 1");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(src),
                        op_to_word(dst),
                        &data_to(bytes, "imm16")[..],
                    );
                } else if encoding.bytes[0].ends_with(" w") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 0");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w", " 1");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "imm16")[..],
                    );
                } else if encoding.bytes[0].ends_with(" w reg") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 0 reg");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_byte(dst),
                        op_to_byte(src),
                        &data_to(bytes, "imm8")[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" w reg", " 1 reg");
                    push_line(
                        instruction.mnemonic.as_str(),
                        op_to_word(dst),
                        op_to_word(src),
                        &data_to(bytes, "imm16")[..],
                    );
                } else if encoding.bytes[0].ends_with(" reg") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());

                    push_line(
                        instruction.mnemonic.as_str(),
                        dst.to_owned(),
                        src.to_owned(),
                        &encoding.bytes[..],
                    );
                } else if encoding.bytes[0].contains(" reg ") {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());
                    if (instruction.mnemonic == "PUSH" || instruction.mnemonic == "POP")
                        && dst == "seg"
                    {
                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 0");
                        push_line(
                            instruction.mnemonic.as_str(),
                            "seg_es".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "0 1");
                        push_line(
                            instruction.mnemonic.as_str(),
                            "seg_cs".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 0");
                        push_line(
                            instruction.mnemonic.as_str(),
                            "seg_ss".to_owned(),
                            src.to_owned(),
                            &bytes[..],
                        );

                        let mut bytes = encoding.bytes.clone();
                        bytes[0] = bytes[0].replace(" reg ", "1 1");
                        push_line(
                            instruction.mnemonic.as_str(),
                            "seg_ds".to_owned(),
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
                        instruction.mnemonic.as_str(),
                        dst.to_owned(),
                        src.to_owned(),
                        &bytes[..],
                    );

                    let mut bytes = encoding.bytes.clone();
                    bytes[0] = bytes[0].replace(" z", " 1");
                    push_line(
                        format!("{}Z", instruction.mnemonic).as_str(),
                        dst.to_owned(),
                        src.to_owned(),
                        &bytes[..],
                    );
                } else if encoding.bytes[0].ends_with(" x x x") {
                    // skipping ESC
                } else {
                    let (dst, src) = dst_and_src_from_string(encoding.operands.as_str());
                    match u8::from_str_radix(encoding.bytes[0].replace(' ', "").as_str(), 2) {
                        Ok(_) => push_line(
                            instruction.mnemonic.as_str(),
                            dst.to_owned(),
                            src.to_owned(),
                            &encoding.bytes[..],
                        ),
                        _ => todo!("op_code: {}", &encoding.bytes[0]),
                    }
                }
            }
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
