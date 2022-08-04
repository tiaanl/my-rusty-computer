mod flatten;
pub(crate) mod parser;

use crate::parser::{Encoding, Instruction};
pub use parser::Category;

pub use flatten::flatten;

pub fn get_data_sheet() -> Vec<Category> {
    let data_sheet = include_str!("../data_sheet.txt");

    let mut lines = parser::Lines::from_vec(data_sheet.split('\n').map(|s| s.to_owned()).collect());
    let mut categories = parser::parse_categories(&mut lines);

    // For some reason the data sheet does not have the NOP instruction, so let's just add it.
    match categories
        .iter_mut()
        .find(|c| c.name == "PROCESSOR CONTROL")
    {
        Some(category) => category.instructions.push(Instruction {
            mnemonic: "NOP".to_owned(),
            aliases: vec![],
            description: "Cycle the CPU without permorning an action.".to_owned(),
            encodings: vec![Encoding {
                operands: "".to_owned(),
                bytes: vec!["1 0 0 1 0 0 0 0".to_owned()],
            }],
        }),
        None => unreachable!(),
    }

    for category in categories.iter_mut() {
        category.instructions.sort_by_key(|i| i.mnemonic.clone());
    }

    categories
}
