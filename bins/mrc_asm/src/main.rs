use mrc_compiler::diagnostics::Diagnostics;
use mrc_compiler::{compiler::Compiler, parser::Parser};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    /// Source file to compile.
    source: String,

    /// The location where to write the compiled binary.
    #[structopt(short, default_value = "out.com")]
    output: String,

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
                    diags.error(&err, err.span().clone());
                    break;
                }
            }
        }

        lines.iter().for_each(|l| println!("{}", l));
    } else {
        loop {
            match parser.parse_line() {
                Ok(Some(line)) => {
                    if let Err(err) = compiler.push_line(line) {
                        diags.error(&err, err.span().clone());
                        break;
                    }
                }
                Ok(None) => break,
                Err(err) => {
                    diags.error(&err, err.span().clone());
                    break;
                }
            }
        }

        match compiler.compile() {
            Ok(bytes) => {
                std::fs::write(opts.output, bytes).unwrap();
            }
            Err(err) => {
                diags.error(&err, err.span().clone());
            }
        }
    }

    if !diags.is_empty() {
        diags.print(&mut std::io::stderr()).unwrap();
    }
}
