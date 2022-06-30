#![allow(dead_code)]

struct Lines {
    lines: Vec<String>,
    pos: usize,
}

impl Lines {
    fn current(&self) -> String {
        self.lines[self.pos].clone()
    }
}

#[derive(Debug)]
struct Encoding {
    operands: String,
    bytes: Vec<String>,

    destination: String,
    source: String,
}

fn split_multiple(subject: String, sep: &[&str]) -> Vec<String> {
    let mut result = vec![subject];

    for sep in sep.iter() {
        result = result[0].split(sep).map(|s| s.to_owned()).collect();
        if result.len() > 1 {
            break;
        }
    }

    result
}

impl Encoding {
    fn from_parts(parts: &[String]) -> Self {
        let operands = if parts[0].ends_with(" to Either") {
            parts[0].replace(" to Either", "")
        } else {
            parts[0].clone()
        };
        let operands = split_multiple(
            operands,
            &[" with ", " to/from ", " to ", " and ", " from "],
        );
        let (destination, source) = Self::operands(operands);

        Self {
            operands: parts[0].clone(),
            bytes: parts[1..].to_owned(),
            destination,
            source,
        }
    }

    fn operands(parts: Vec<String>) -> (String, String) {
        match parts.len() {
            2 => (
                Self::parse_operand(parts[1].clone()),
                Self::parse_operand(parts[0].clone()),
            ),
            1 => (Self::parse_operand(parts[0].clone()), "none".to_owned()),
            0 => ("none".to_owned(), "none".to_owned()),
            _ => panic!(),
        }
    }

    fn parse_operand(operand: String) -> String {
        match operand.to_lowercase().as_str() {
            "memory" => "mem",
            "register" => "reg",
            "register/memory" | "reg./memory" => "reg_mem",
            "immediate" | "fixed port" | "immediate data" => "imm",
            "accumulator" => "acc",
            "segment register" => "seg",
            "variable port" => "none",
            "" => "none",
            _ => "", // _ => todo!("{}", operand),
        }
        .to_owned()
    }
}

#[derive(Debug)]
struct Instruction {
    mnemonic: String,
    description: String,
    encodings: Vec<Encoding>,
}

#[derive(Debug, Default)]
struct Category {
    name: String,
    instructions: Vec<Instruction>,
}

fn is_category_header(s: &str) -> bool {
    s.find(':').is_none() && s.find('|').is_none() && s.find('=').is_none()
}

fn is_instruction_header(s: &str) -> bool {
    s.find('=').is_some() || s.ends_with(':')
}

fn parse_instruction(lines: &mut Lines, mnemonic: String, description: String) -> Instruction {
    let mut instruction = Instruction {
        mnemonic,
        description,
        encodings: vec![],
    };

    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }
        let current = lines.current();

        let parts: Vec<String> = current.split('|').map(|s| s.trim().to_owned()).collect();

        if parts[0].ends_with(':') || parts[0].find('=').is_some() {
            break;
        }

        if is_category_header(parts[0].as_str()) {
            break;
        }

        instruction
            .encodings
            .push(Encoding::from_parts(parts.as_slice()));

        lines.pos += 1;
    }

    instruction
}

fn parse_category(lines: &mut Lines, name: String) -> Category {
    let mut category = Category {
        name,
        instructions: vec![],
    };

    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }
        let current = lines.current();
        debug_assert!(
            is_category_header(current.as_str()),
            "not a category header: {}",
            current
        );
        println!("parse_category: {}", &current);

        if current.is_empty() {
            lines.pos += 1;
            continue;
        }

        // If we reach a category heading again, then break out.
        if is_category_header(current.as_str()) {
            break;
        }

        // Here we can encounter 2 different lines.
        //   1 - Normal instruction header: IN = Input From:
        //   2 - An inline instruction: XLAT = Translate Byte to AL                 | 1 1 0 1 0 1 1 1

        let split_mnemonic_description = |s: String| {
            let parts: Vec<String> = s.split('=').map(|s| s.trim().to_owned()).take(2).collect();

            let mnemonic = parts[0].clone();
            let description = parts[1].clone();
            let description = description[0..description.len()].to_owned();

            (mnemonic, description)
        };

        if current.find('|').is_some() {
            let parts: Vec<String> = current.split('|').map(|s| s.trim().to_owned()).collect();

            let (mnemonic, description) = split_mnemonic_description(parts[0].to_owned());

            let mut parts_ = vec!["".to_owned()];
            parts.iter().skip(1).for_each(|p| parts_.push(p.clone()));

            let instruction = Instruction {
                mnemonic,
                description,
                encodings: vec![Encoding::from_parts(parts_.as_slice())],
            };

            category.instructions.push(instruction);

            lines.pos += 1;
        } else {
            let (mnemonic, description) = split_mnemonic_description(current);

            lines.pos += 1;

            category
                .instructions
                .push(parse_instruction(lines, mnemonic, description));
        }
    }

    category
}

fn parse_lines(lines: &mut Lines) -> Vec<Category> {
    let mut categories = vec![];
    loop {
        if lines.pos >= lines.lines.len() {
            break;
        }
        let current = lines.current();
        lines.pos += 1;

        if current.is_empty() {
            continue;
        }

        categories.push(parse_category(lines, current));
    }

    categories
}

fn main() {
    let data_sheet = include_str!("../data_sheet.txt");

    let mut lines = Lines {
        lines: data_sheet.split('\n').map(|s| s.to_owned()).collect(),
        pos: 0,
    };

    for category in parse_lines(&mut lines) {
        println!("{}", category.name);
        for instruction in category.instructions {
            println!("  - {} ({})", instruction.mnemonic, instruction.description);
            for encoding in instruction.encodings {
                println!("      - {:40}  {:?}", encoding.operands, encoding.bytes);
            }
        }
    }
}
