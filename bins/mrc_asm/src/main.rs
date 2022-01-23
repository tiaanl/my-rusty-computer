use std::path::Path;
use structopt::StructOpt;

fn parse_file<P: AsRef<Path> + std::fmt::Display>(path: P) {
    let data = match std::fs::read_to_string(path.as_ref()) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Could not parse file: {} ({})", path, err);
            return;
        }
    };

    match mrc_assembler::parser::parse_lines(data.as_str()) {
        Ok(lines) => {
            for line in lines.1.iter() {
                println!("{}", line);
            }
        }
        Err(err) => eprintln!("error: {:?}", err),
    }
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
        parse_file(file);
    }
}
