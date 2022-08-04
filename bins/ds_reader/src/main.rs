use data_sheet::flatten;

fn main() {
    let ds = data_sheet::get_data_sheet();

    // for category in ds.iter() {
    //     println!("{}", category.name);
    //     println!();
    //
    //     for instruction in category.instructions.iter() {
    //         print!("  {}", instruction.mnemonic);
    //         if !instruction.aliases.is_empty() {
    //             print!(" ({})", instruction.aliases.join(", "));
    //         }
    //         println!(" - {}", instruction.description);
    //
    //         for encoding in instruction.encodings.iter() {
    //             if encoding.operands.is_empty() {
    //                 print!("    None");
    //             } else {
    //                 print!("    {}", encoding.operands);
    //             }
    //             if !encoding.bytes.is_empty() {
    //                 print!(" - {}", encoding.bytes.join(" | "));
    //             }
    //             println!();
    //         }
    //
    //         println!();
    //     }
    // }

    let lines = flatten(ds.as_slice());
    for l in lines.iter() {
        println!("{:?}", l);
    }
}
