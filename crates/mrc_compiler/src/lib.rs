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

impl CompileError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompileError::ParserError(err) => err.span(),
            CompileError::CompileError(err) => err.span(),
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ParserError(err) => err.fmt(f),
            CompileError::CompileError(err) => err.fmt(f),
        }
    }
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
