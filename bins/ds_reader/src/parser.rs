pub struct Lines {
    lines: Vec<String>,
    pos: usize,
}

impl Lines {
    pub fn from_vec(lines: Vec<String>) -> Self {
        Self { lines, pos: 0 }
    }

    fn current(&self) -> String {
        self.lines[self.pos].clone()
    }

    fn next(&mut self) {
        self.pos += 1;
    }
}

#[derive(Debug)]
pub struct Encoding {
    pub operands: String,
    pub bytes: Vec<String>,
}

impl Encoding {
    fn from_parts(parts: &[String]) -> Self {
        Self {
            operands: parts[0].clone(),
            bytes: parts[1..].to_owned(),
        }
    }

    // fn operands(parts: Vec<String>) -> (String, String) {
    //     match parts.len() {
    //         2 => (
    //             Self::parse_operand(parts[1].clone()),
    //             Self::parse_operand(parts[0].clone()),
    //         ),
    //         1 => (Self::parse_operand(parts[0].clone()), "none".to_owned()),
    //         0 => ("none".to_owned(), "none".to_owned()),
    //         _ => panic!(),
    //     }
    // }
    //
    // fn parse_operand(operand: String) -> String {
    //     match operand.to_lowercase().as_str() {
    //         "memory" => "mem",
    //         "register" => "reg",
    //         "register/memory" | "reg./memory" => "reg_mem",
    //         "immediate" | "fixed port" | "immediate data" => "imm",
    //         "accumulator" => "acc",
    //         "segment register" => "seg",
    //         "variable port" => "none",
    //         "" => "none",
    //         _ => "TODO",
    //     }
    //     .to_owned()
    // }
}

#[derive(Debug, Default)]
pub struct Instruction {
    pub mnemonic: String,
    pub description: String,
    pub encodings: Vec<Encoding>,
}

#[derive(Debug, Default)]
pub struct Category {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

fn is_category_header(s: &str) -> bool {
    s.find(':').is_none() && s.find('|').is_none() && s.find('=').is_none()
}

fn is_instruction_header(s: &str) -> bool {
    (s.find('=').is_some() || s.ends_with(':')) && s.find('|').is_none()
}

fn is_inline_instruction_header(s: &str) -> bool {
    // XLAT = Translate Byte to AL                 | 1 1 0 1 0 1 1 1

    if let Some(equal) = s.find('=') {
        if let Some(split) = s.find('|') {
            if equal < split {
                return true;
            }
        }
    }

    false
}

fn is_encoding(s: &str) -> bool {
    let first_split = s.find('|');
    let first_equal = s.find('=');

    if let Some(split) = first_split {
        if let Some(equal) = first_equal {
            if equal < split {
                return false;
            }
        }
    }

    first_split.is_some()
}

fn split_mnemonic_description(input: String) -> (String, String) {
    let parts: Vec<String> = input
        .split('=')
        .map(|s| s.trim().to_owned())
        .take(2)
        .collect();

    let mnemonic = parts[0].clone();
    let description = if parts[1].ends_with(':') {
        parts[1][..parts[1].len() - 1].to_owned()
    } else {
        parts[1].clone()
    };
    let description = description[0..description.len()].to_owned();

    (mnemonic, description)
}

fn parse_category_header(input: String) -> Option<String> {
    if is_category_header(input.as_str()) {
        Some(input)
    } else {
        None
    }
}

fn parse_instruction_header(input: String) -> Option<(String, String)> {
    if is_instruction_header(input.as_str()) {
        Some(split_mnemonic_description(input))
    } else {
        None
    }
}

fn parse_inline_instruction(input: String) -> Option<Instruction> {
    if !is_inline_instruction_header(input.as_str()) {
        return None;
    }

    let mut parts: Vec<String> = input.split('|').map(|s| s.trim().to_owned()).collect();

    let (mnemonic, description) = split_mnemonic_description(parts[0].clone());
    let mut instruction = Instruction {
        mnemonic,
        description,
        ..Instruction::default()
    };

    parts[0].clear();

    instruction.encodings.push(Encoding::from_parts(&parts[..]));

    Some(instruction)
}

fn parse_encoding(input: String) -> Option<Encoding> {
    // Register/Memory to Segment Register         | 1 0 0 0 1 1 1 0 | mod 0 reg r/m
    if is_encoding(input.as_str()) {
        let parts: Vec<String> = input.split('|').map(|s| s.trim().to_owned()).collect();

        Some(Encoding::from_parts(&parts[..]))
    } else {
        None
    }
}

fn parse_encodings(lines: &mut Lines, instruction: &mut Instruction) {
    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }
        let current = lines.current();

        if let Some(encoding) = parse_encoding(current) {
            lines.next();

            instruction.encodings.push(encoding);
        } else {
            break;
        }
    }
}

fn parse_instructions(lines: &mut Lines, category: &mut Category) {
    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }

        if let Some((mnemonic, description)) = parse_instruction_header(lines.current()) {
            lines.next();

            let mut instruction = Instruction {
                mnemonic,
                description,
                ..Instruction::default()
            };

            parse_encodings(lines, &mut instruction);

            category.instructions.push(instruction);
        } else if let Some(instruction) = parse_inline_instruction(lines.current()) {
            lines.next();

            category.instructions.push(instruction);
        } else {
            break;
        }
    }
}

pub fn parse_categories(lines: &mut Lines) -> Vec<Category> {
    let mut categories = vec![];
    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }
        let current = lines.current();
        lines.next();

        if let Some(category_header) = parse_category_header(current) {
            let mut category = Category {
                name: category_header,
                ..Category::default()
            };

            parse_instructions(lines, &mut category);

            categories.push(category);
        } else {
            break;
        }
    }

    categories
}
