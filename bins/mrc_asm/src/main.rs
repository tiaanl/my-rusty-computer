use mrc_compiler::diagnostics::Diagnostics;
use mrc_compiler::{ast::Span, compiler::Compiler, Parser};
use structopt::StructOpt;

fn _print_source_pos(source: &str, span: &Span, path: Option<&str>) {
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

    /// Only parse the source file and print the syntax tree.
    #[structopt(short)]
    parse_only: bool,
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

    let mut diags = Diagnostics::new(source, opts.source.clone());
    let mut compiler = Compiler::default();

    let mut parser = Parser::new(source);

    if opts.parse_only {
        let mut lines = vec![];

        loop {
            match parser.parse_line() {
                Ok(Some(line)) => {
                    lines.push(line);
                }
                Ok(None) => break,
                Err(err) => {
                    diags.error(format!("{}", err), err.span().clone());
                    break;
                }
            }
        }

        lines.iter().for_each(|l| println!("{}", l));
    } else {
        loop {
            match parser.parse_line() {
                Ok(Some(line)) => compiler.push_line(line),
                Ok(None) => break,
                Err(err) => {
                    diags.error(format!("{}", err), err.span().clone());
                    break;
                }
            }
        }

        if let Err(err) = compiler.compile() {
            // eprintln!("COMPILE ERROR: {}", err);
            // print_source_pos(source, err.span(), Some(opts.source.as_str()));
            diags.error(format!("{}", err), err.span().clone());
        }
    }

    if !diags.is_empty() {
        diags.print(&mut std::io::stderr()).unwrap();
    }
}
