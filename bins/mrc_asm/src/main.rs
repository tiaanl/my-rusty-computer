use mrc_compiler::Compiler;
use mrc_parser::ast::Span;
use mrc_parser::ParserError;
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

#[derive(StructOpt)]
struct Opt {
    /// Source file to compile.
    source: String,
}

fn main() {
    let opts = Opt::from_args();

    let data = match std::fs::read_to_string(&opts.source) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Could not open source file: {} ({})", opts.source, err);
            return;
        }
    };

    let source = data.as_str();

    let mut compiler = Compiler::default();

    if let Err(err) = mrc_parser::parse(source, &mut compiler) {
        match err {
            ParserError::Stopped(span, err) => {
                eprintln!("PARSER ERROR: {}", err);
                print_source_pos(source, &span, Some(opts.source.as_str()));
            }

            err => {
                eprintln!("PARSER ERROR: {}", err);
                print_source_pos(source, err.span(), Some(opts.source.as_str()));
            }
        }

        return;
    }

    if let Err(err) = compiler.compile() {
        eprintln!("COMPILE ERROR: {}", err);
        print_source_pos(source, err.span(), Some(opts.source.as_str()));
    }
}
