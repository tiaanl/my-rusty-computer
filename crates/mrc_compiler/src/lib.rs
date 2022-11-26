pub mod ast;
pub mod compiler;
pub mod diagnostics;
mod encoder;
pub mod lexer;
mod operations;
pub mod parser;

#[derive(Debug)]
pub enum CompileError {
    ParserError(parser::ParserError),
    CompileError(compiler::CompileError),
}

pub fn compile(source: &str) -> Result<Vec<u8>, CompileError> {
    let mut parser = parser::Parser::new(source);
    let mut compiler = compiler::Compiler::default();

    while let Some(line) = parser.parse_line().map_err(CompileError::ParserError)? {
        compiler
            .push_line(line)
            .map_err(CompileError::CompileError)?;
    }

    compiler.compile().map_err(CompileError::CompileError)
}
