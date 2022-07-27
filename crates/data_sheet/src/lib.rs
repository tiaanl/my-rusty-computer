pub(crate) mod parser;

pub use parser::Category;

pub fn get_data_sheet() -> Vec<Category> {
    let data_sheet = include_str!("../data_sheet.txt");

    let mut lines = parser::Lines::from_vec(data_sheet.split('\n').map(|s| s.to_owned()).collect());
    parser::parse_categories(&mut lines)
}
