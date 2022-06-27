use mrc_parser::ast::Span;
use std::path::Path;
use structopt::StructOpt;

fn print_source_pos(source: &str, span: &Span, path: Option<&str>) {
    let prev_new_line = if let Some(found) = source[..span.start].rfind('\n') {
        found + 1
    } else {
        0
    };

    let next_new_line = if let Some(found) = source[span.start..].find('\n') {
        span.start + found
    } else {
        source.len()
    };

    let fragment = &source[prev_new_line..next_new_line];

    let line = source[0..span.start].matches('\n').count() + 1;
    let column = span.start - prev_new_line;

    if let Some(path) = path {
        println!("{}:{}:{}", path, line, column + 1);
    }

    println!("{}", fragment);
    for _ in 0..column {
        print!(" ");
    }
    let end = if span.start == span.end {
        span.end + 1
    } else {
        span.end
    };
    for _ in span.start..end {
        print!("^");
    }
    println!();
}

fn parse_file<P: AsRef<Path> + std::fmt::Display>(path: P) {
    let data = match std::fs::read_to_string(path.as_ref()) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Could not parse file: {} ({})", path, err);
            return;
        }
    };

    let source = data.as_str();

    if let Err(err) = mrc_parser::parse(source, &mut |l| println!("{}", l)) {
        eprintln!("ERROR: {}", err);
        print_source_pos(source, err.span(), None);
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
