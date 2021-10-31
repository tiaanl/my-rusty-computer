use mrc_assembler::lexer::{tokenize, TokenKind};
use std::path::Path;

fn parse_file<P: AsRef<Path>>(path: P) -> std::io::Result<()> {
    let data = std::fs::read_to_string(path)?;

    let tokens = tokenize(data.as_str());
    let mut count = 0usize;
    for token in tokens
        .into_iter()
        .filter(|t| t.kind != TokenKind::Whitespace && t.kind != TokenKind::LineComment)
    {
        println!("{:?}", token);
        count += 1;
        if count == 10 {
            break;
        }
    }
    println!("{} tokens", count);

    Ok(())
}

fn main() {
    let matches = clap::App::new("mrc-asm")
        .arg(clap::Arg::with_name("sources").min_values(1))
        .get_matches();

    // We can unwrap here, because `min_values` is set to at least 1.
    let files: Vec<_> = matches.values_of("sources").unwrap().collect();
    for file in files {
        parse_file(file).unwrap();
    }
}
