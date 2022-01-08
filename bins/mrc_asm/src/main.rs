use std::path::Path;
use structopt::StructOpt;

fn parse_file<P: AsRef<Path>>(_path: P) -> std::io::Result<()> {
    /*
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
    */

    Ok(())
}

#[derive(StructOpt)]
struct Opt {
    /// Sources files to assemble
    sources: Vec<String>,
}

fn main() {
    let opts = Opt::from_args();

    // TODO: Can `structopt` handle this case?
    if opts.sources.is_empty() {
        eprintln!("At least one source file is required");
        return;
    }

    // We can unwrap here, because `min_values` is set to at least 1.
    for file in opts.sources {
        parse_file(file).unwrap();
    }
}
