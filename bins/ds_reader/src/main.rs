mod parser;

fn main() {
    let data_sheet = include_str!("../data_sheet.txt");

    let mut lines = parser::Lines::from_vec(data_sheet.split('\n').map(|s| s.to_owned()).collect());

    let categories = parser::parse_categories(&mut lines);

    for category in categories {
        println!("{}", category.name);
        for instruction in category.instructions {
            println!("  - {} ({})", instruction.mnemonic, instruction.description);
            for encoding in instruction.encodings {
                println!("      - {:40}  {:?}", encoding.operands, encoding.bytes);
            }
        }
    }

    // categories.iter().for_each(|c| {
    //     c.instructions.iter().for_each(|i| {
    //         i.encodings.iter().for_each(|e| {
    //             println!("{:?}", split_operands(e.operands.clone()));
    //         })
    //     })
    // });
}
