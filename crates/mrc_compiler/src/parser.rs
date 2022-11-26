use crate::ast;
use crate::lexer::{Cursor, LiteralKind, PunctuationKind, Token};
use crate::operations::Operation;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum ParserError {
    Expected(ast::Span, String, String),
    InstructionExpected(ast::Span, String),
    SecondOperandExpected(ast::Span, String),
    OperandExpected(ast::Span, String),
    InvalidPrefixOperator(ast::Span),
    DataDefinitionWithoutData(ast::Span),
    SegmentOrAddressExpected(ast::Span),
    InvalidIndirectEncoding(ast::Span, ast::Register, Option<ast::Register>),
    UnterminatedStringLiteral(ast::Span),
}

impl ParserError {
    pub fn span(&self) -> &ast::Span {
        match self {
            ParserError::Expected(span, _, _)
            | ParserError::InstructionExpected(span, _)
            | ParserError::SecondOperandExpected(span, _)
            | ParserError::OperandExpected(span, _)
            | ParserError::InvalidPrefixOperator(span)
            | ParserError::DataDefinitionWithoutData(span)
            | ParserError::SegmentOrAddressExpected(span)
            | ParserError::InvalidIndirectEncoding(span, ..)
            | ParserError::UnterminatedStringLiteral(span) => span,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Expected(_, expected, found) => {
                write!(f, "expected {}, found {}.", expected, found)
            }
            ParserError::InstructionExpected(_, found) => {
                write!(f, "Instruction expected, found {}.", found)
            }
            ParserError::SecondOperandExpected(_, found) => {
                write!(f, "Second operand expected, found {}.", found)
            }
            ParserError::OperandExpected(_, found) => {
                write!(f, "Operand expected, found {}.", found)
            }
            ParserError::InvalidPrefixOperator(_) => write!(f, "Invalid prefix operator."),
            ParserError::DataDefinitionWithoutData(_) => write!(f, "Data definition without data."),
            ParserError::SegmentOrAddressExpected(_) => {
                write!(f, "Address or segment override (e.g. [CS:...]) expected.")
            }
            ParserError::InvalidIndirectEncoding(_, first, second) => {
                if let Some(second) = second {
                    write!(
                        f,
                        "The registers {} + {} is not a valid indirect encoding.",
                        first, second
                    )?;
                } else {
                    write!(
                        f,
                        "The register {} is not a valid indirect encoding.",
                        first
                    )?;
                }

                write!(
                    f,
                    " Valid combinations are: BX+SI, BX+DI, BP+SI, BP+DI, SI, DI, BX, BP."
                )
            }
            ParserError::UnterminatedStringLiteral(_) => {
                write!(f, "Unterminated string literal.")
            }
        }
    }
}

struct FoundToken<'a>(Token, &'a str);

impl<'a> Display for FoundToken<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            Token::Whitespace(_) => write!(f, "whitespace"),
            Token::Identifier(_) => {
                write!(f, "identifier \"{}\"", self.1)
            }
            Token::Literal(_, literal_kind) => match literal_kind {
                LiteralKind::Number(value) => write!(f, "number \"{}\"", value),
                LiteralKind::String(terminated) => {
                    if *terminated {
                        write!(f, "string \"{}\"", self.1)
                    } else {
                        write!(f, "unterminated string \"{}\"", self.1)
                    }
                }
            },
            Token::Punctuation(_, _) => {
                write!(f, "punctuation \"{}\"", self.1)
            }
            Token::Comment(_) => write!(f, "comment \"{}\"", self.1),
            Token::NewLine(_) => write!(f, "new line"),
            Token::EndOfFile(_) => write!(f, "end of file"),
            Token::Invalid(_, c) => write!(f, "invalid character \"{}\"", c),
        }
    }
}

impl ast::Operator {
    pub fn evaluate(&self, left: i32, right: i32) -> i32 {
        match self {
            ast::Operator::Add => left + right,
            ast::Operator::Subtract => left - right,
            ast::Operator::Multiply => left * right,
            ast::Operator::Divide => left / right,
        }
    }
}

#[derive(Clone)]
pub struct Parser<'a> {
    cursor: Cursor<'a>,

    /// The current token we are parsing.
    token: Token,

    /// Position in the cursor where the current token starts.
    token_start: usize,

    // Position in the cursor where the last meaningful token ended.
    last_token_end: usize,
}

#[derive(Clone)]
struct Checkpoint<'a>(Parser<'a>);

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut parser = Self {
            cursor: Cursor::new(source),
            token: Token::EndOfFile(0),
            token_start: 0,
            last_token_end: 0,
        };

        // Initialize the current token with the first token that we can fetch from the lexer.
        parser.next_token();

        parser
    }

    fn token_range(&self) -> std::ops::Range<usize> {
        self.token_start..self.token_start + self.token.len()
    }

    fn token_source(&self) -> &'a str {
        self.cursor.source_at(self.token_start, self.token.len())
    }

    fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint(self.clone())
    }

    fn jump_to_checkpoint(&mut self, checkpoint: Checkpoint<'a>) {
        *self = checkpoint.0;
    }

    pub fn parse_line(&mut self) -> Result<Option<ast::Line>, ParserError> {
        // Skip empty lines.
        while let Token::NewLine(_) = self.token {
            self.next_token();
        }

        if let Token::EndOfFile(_) = self.token {
            return Ok(None);
        }

        Ok(Some(match self.token {
            Token::Identifier(_) => {
                if let Some(line) = self.parse_instruction_or_meta()? {
                    line
                } else {
                    // If we don't recognize the identifier, we assume it's a label.
                    ast::Line::Label(self.parse_label()?)
                }
            }

            _ => return Err(self.instruction_expected()),
        }))
    }

    fn next_token(&mut self) {
        self.last_token_end = self.token_start + self.token.len();
        self.token_start = self.cursor.pos();

        loop {
            let token = self.cursor.next_token();
            match token {
                Token::Comment(_) | Token::Whitespace(_) => {
                    self.token_start = self.cursor.pos();
                    continue;
                }
                _ => {
                    self.token = token;
                    break;
                }
            }
        }
    }

    /// The current token is required to be a new line.  If it is, then consume it, otherwise we
    /// report an error.
    fn require_new_line(&mut self) -> Result<(), ParserError> {
        if let Token::NewLine(_) = self.token {
            self.next_token();
            Ok(())
        } else if let Token::EndOfFile(_) = self.token {
            Ok(())
        } else {
            Err(self.expected("new line".to_string()))
        }
    }

    fn parse_instruction_or_meta(&mut self) -> Result<Option<ast::Line>, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(..)));

        let identifier = self.token_source();

        if let Ok(operation) = Operation::from_str(identifier) {
            Ok(Some(self.parse_instruction(operation)?))
        } else {
            match identifier.to_lowercase().as_str() {
                "equ" => Ok(Some(self.parse_constant()?)),
                "db" => Ok(Some(self.parse_data(1)?)),
                "dw" => Ok(Some(self.parse_data(2)?)),
                "times" => Ok(Some(self.parse_times()?)),
                _ => Ok(None),
            }
        }
    }

    fn parse_label(&mut self) -> Result<ast::Label, ParserError> {
        let start = self.token_start;

        // We only capture the label part.
        let end = self.token_start + self.token.len();

        // Consume the token that holds the label.
        let identifier = self.token_source();
        self.next_token();

        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
            self.next_token();
        }

        Ok(ast::Label(start..end, identifier.to_owned()))
    }

    fn parse_instruction(&mut self, operation: Operation) -> Result<ast::Line, ParserError> {
        let start = self.token_start;

        // Consume the operation.
        self.next_token();

        static PREFIXES: &[Operation] = &[Operation::REP, Operation::REPNE];

        let (operands, end) = if !PREFIXES.contains(&operation) {
            let res = (self.parse_operands()?, self.last_token_end);
            self.require_new_line()?;
            res
        } else {
            let operands = ast::Operands::None(self.last_token_end..self.last_token_end);
            (operands, self.last_token_end)
        };

        Ok(ast::Line::Instruction(ast::Instruction {
            span: start..end,
            operation,
            operands,
        }))
    }

    fn parse_operands(&mut self) -> Result<ast::Operands, ParserError> {
        if matches!(self.token, Token::NewLine(_) | Token::EndOfFile(_)) {
            Ok(ast::Operands::None(
                self.last_token_end..self.last_token_end,
            ))
        } else {
            let start = self.token_start;

            let destination = self.parse_operand(None)?;

            match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => Ok(ast::Operands::Destination(
                    start..self.last_token_end,
                    destination,
                )),

                Token::Punctuation(_, PunctuationKind::Comma) => {
                    self.next_token();
                    let source = self.parse_operand(None)?;

                    Ok(ast::Operands::DestinationAndSource(
                        start..self.last_token_end,
                        destination,
                        source,
                    ))
                }

                _ => Err(self.expected("comma".to_string())),
            }
        }
    }

    fn parse_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand, ParserError> {
        let start = self.token_start;

        let result = match self.token {
            Token::Punctuation(_, PunctuationKind::OpenBracket) => {
                self.parse_memory_operand(data_size)
            }

            Token::Identifier(_) => {
                let identifier = self.token_source();
                if let Ok(register) = ast::Register::from_str(identifier) {
                    self.next_token();
                    Ok(ast::Operand::Register(start..self.last_token_end, register))
                } else if let Ok(segment) = ast::Segment::from_str(identifier) {
                    self.next_token();
                    Ok(ast::Operand::Segment(start..self.last_token_end, segment))
                } else if let Ok(data_size) = ast::DataSize::from_str(identifier) {
                    self.next_token();
                    self.parse_operand(Some(data_size))
                } else {
                    self.parse_immediate_or_far()
                }
            }

            _ => self.parse_immediate_or_far(),
        }?;

        Ok(result)
    }

    fn parse_immediate_or_far(&mut self) -> Result<ast::Operand, ParserError> {
        let start = self.token_start;

        let expression = self.parse_expression()?;
        if let Some(offset) = self.parse_far()? {
            // The first value is the segment and if we parsed an expression with `parse_far` that
            // would be the offset.  Values are stored offset first in the operand, matching the
            // order they values are encoded.
            Ok(ast::Operand::Far(
                start..self.last_token_end,
                offset,
                expression,
            ))
        } else {
            Ok(ast::Operand::Immediate(
                start..self.last_token_end,
                expression,
            ))
        }
    }

    fn parse_far(&mut self) -> Result<Option<ast::Expression>, ParserError> {
        if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
            self.next_token();
            let expr = self.parse_expression()?;
            Ok(Some(expr))
        } else {
            Ok(None)
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, ParserError> {
        self.parse_expression_with_precedence(0)
    }

    fn parse_memory_operand(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand, ParserError> {
        assert!(matches!(
            self.token,
            Token::Punctuation(_, PunctuationKind::OpenBracket)
        ));

        let start = self.token_start;

        // Consume the opening bracket.
        self.next_token();

        let mut segment_override = None;
        let mut indirect_encoding = None;

        let expression = match self.token {
            Token::Identifier(_) => {
                let identifier = self.token_source();
                // If the first identifier is a segment, then we have an override.
                if let Ok(segment) = ast::Segment::from_str(identifier) {
                    segment_override = Some(segment);
                    self.next_token();

                    if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
                        self.next_token();
                    } else {
                        return Err(
                            self.expected("Colon (:) required after segment override.".to_owned())
                        );
                    }
                }

                indirect_encoding = self.parse_indirect_encoding()?;

                if matches!(
                    self.token,
                    Token::Punctuation(_, PunctuationKind::CloseBracket)
                ) {
                    // Don't consume the closing bracket here, just state that there is no
                    // expression here.
                    None
                } else {
                    Some(self.parse_expression()?)
                }
            }

            Token::Literal(_, LiteralKind::Number(_)) => Some(self.parse_expression()?),

            _ => return Err(ParserError::SegmentOrAddressExpected(self.token_range())),
        };

        if matches!(
            self.token,
            Token::Punctuation(_, PunctuationKind::CloseBracket)
        ) {
            self.next_token();
        } else {
            return Err(self.expected("closing bracket for memory address".to_owned()));
        }

        Ok(if let Some(indirect_encoding) = indirect_encoding {
            ast::Operand::Indirect(
                start..self.last_token_end,
                indirect_encoding,
                expression,
                data_size,
                segment_override,
            )
        } else {
            ast::Operand::Direct(
                start..self.last_token_end,
                expression.unwrap(),
                data_size,
                segment_override,
            )
        })
    }

    fn parse_indirect_encoding(&mut self) -> Result<Option<ast::IndirectEncoding>, ParserError> {
        let start = self.token_start;

        let first = if let Some(register) = self.parse_register()? {
            match register {
                ast::Register::Word(reg @ ast::WordRegister::Bx)
                | ast::Register::Word(reg @ ast::WordRegister::Bp)
                | ast::Register::Word(reg @ ast::WordRegister::Si)
                | ast::Register::Word(reg @ ast::WordRegister::Di) => {
                    // Consume the first register.
                    self.next_token();

                    Some(reg)
                }
                reg => {
                    // We found a register, but it is not a valid first register, so this is an
                    // error.
                    return Err(ParserError::InvalidIndirectEncoding(
                        start..self.last_token_end,
                        reg,
                        None,
                    ));
                }
            }
        } else {
            None
        };

        if first.is_none() {
            return Ok(None);
        }

        let second = if let Token::Punctuation(_, PunctuationKind::Plus) = self.token {
            let checkpoint = self.checkpoint();

            // Consume the +.
            self.next_token();

            if let Some(register) = self.parse_register()? {
                match register {
                    ast::Register::Word(reg @ ast::WordRegister::Si)
                    | ast::Register::Word(reg @ ast::WordRegister::Di) => {
                        // Consume the second register.
                        self.next_token();

                        Some(reg)
                    }
                    reg => {
                        return Err(ParserError::InvalidIndirectEncoding(
                            start..self.token_range().end,
                            ast::Register::Word(first.unwrap()),
                            Some(reg),
                        ));
                    }
                }
            } else {
                // The token after the first register was a + sign, but the second one is not a
                // register, so we have to backtrack to before the + to allow the expression parser
                // to parse a PrefixOperator in stead of just the value.
                self.jump_to_checkpoint(checkpoint);

                None
            }
        } else {
            None
        };

        // We can unwrap here, because we already checked if first is None.
        match (first.unwrap(), second) {
            (ast::WordRegister::Bx, Some(ast::WordRegister::Si)) => {
                Ok(Some(ast::IndirectEncoding::BxSi))
            }
            (ast::WordRegister::Bx, Some(ast::WordRegister::Di)) => {
                Ok(Some(ast::IndirectEncoding::BxDi))
            }
            (ast::WordRegister::Bp, Some(ast::WordRegister::Si)) => {
                Ok(Some(ast::IndirectEncoding::BpSi))
            }
            (ast::WordRegister::Bp, Some(ast::WordRegister::Di)) => {
                Ok(Some(ast::IndirectEncoding::BpDi))
            }
            (ast::WordRegister::Bx, None) => Ok(Some(ast::IndirectEncoding::Bx)),
            (ast::WordRegister::Bp, None) => Ok(Some(ast::IndirectEncoding::Bp)),
            (ast::WordRegister::Si, None) => Ok(Some(ast::IndirectEncoding::Si)),
            (ast::WordRegister::Di, None) => Ok(Some(ast::IndirectEncoding::Di)),
            _ => unreachable!(),
        }
    }

    fn parse_register(&mut self) -> Result<Option<ast::Register>, ParserError> {
        if let Token::Identifier(_) = self.token {
            let source = self.token_source();
            if let Ok(register) = ast::Register::from_str(source) {
                Ok(Some(register))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn parse_constant(&mut self) -> Result<ast::Line, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        let start = self.token_start;

        // Consume the "equ" keyword.
        self.next_token();

        let expression = self.parse_expression()?;

        let end = self.last_token_end;

        self.require_new_line()?;

        Ok(ast::Line::Constant(start..end, expression))
    }

    fn parse_data(&mut self, bytes_per_value: usize) -> Result<ast::Line, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        let data_definition_token_span = self.token_range();

        let start = self.token_start;

        // Consume the "Dx" keyword.
        self.next_token();

        let mut data = Vec::<u8>::new();

        loop {
            match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => break,

                Token::Punctuation(_, PunctuationKind::Comma) => {
                    self.next_token();
                    continue;
                }

                Token::Literal(_, LiteralKind::String(terminated)) => {
                    if !terminated {
                        return Err(ParserError::UnterminatedStringLiteral(self.token_range()));
                    }

                    let source = self.token_source();
                    let source = &source[1..source.len() - 1]; // Strip the quotes
                    self.next_token();

                    for b in source.as_bytes() {
                        data.push(*b);
                    }
                }

                Token::Literal(_, LiteralKind::Number(number)) => {
                    (number as u32)
                        .to_le_bytes()
                        .iter()
                        .take(bytes_per_value)
                        .for_each(|b| data.push(*b));

                    self.next_token();
                }

                _ => return Err(self.expected("literal expected for data definition".to_owned())),
            }
        }

        let end = self.last_token_end;

        if data.is_empty() {
            return Err(ParserError::DataDefinitionWithoutData(
                data_definition_token_span,
            ));
        }

        self.require_new_line()?;

        Ok(ast::Line::Data(start..end, data))
    }

    fn parse_times(&mut self) -> Result<ast::Line, ParserError> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));
        debug_assert!(self.token_source().to_lowercase().as_str() == "times");

        // Consume the "times" keyword.
        self.next_token();

        // Should be followed by the number of times to repeat the content.
        let expression = self.parse_expression()?;

        if let Some(line_content) = self.parse_line()? {
            Ok(ast::Line::Times(0..0, expression, Box::new(line_content)))
        } else {
            todo!()
        }
    }

    fn expected(&self, expected: String) -> ParserError {
        ParserError::Expected(
            self.token_range(),
            expected,
            FoundToken(self.token.clone(), self.token_source()).to_string(),
        )
    }

    fn instruction_expected(&self) -> ParserError {
        ParserError::InstructionExpected(
            self.token_range(),
            format!("{}", FoundToken(self.token.clone(), self.token_source())),
        )
    }
}

/// Utility functions for tokens.
impl Token {
    fn operator(&self) -> Option<ast::Operator> {
        Some(match self {
            Token::Punctuation(_, punctuation) => match punctuation {
                PunctuationKind::Plus => ast::Operator::Add,
                PunctuationKind::Minus => ast::Operator::Subtract,
                PunctuationKind::Star => ast::Operator::Multiply,
                PunctuationKind::ForwardSlash => ast::Operator::Divide,
                _ => return None,
            },
            _ => return None,
        })
    }
}

impl<'a> Parser<'a> {
    fn prefix_precedence(operator: ast::Operator) -> Result<((), u8), ParserError> {
        Ok(match operator {
            ast::Operator::Add | ast::Operator::Subtract => ((), 5),
            _ => return Err(ParserError::InvalidPrefixOperator(0..0)),
        })
    }

    fn infix_precedence(operator: ast::Operator) -> (u8, u8) {
        match operator {
            ast::Operator::Add | ast::Operator::Subtract => (1, 2),
            ast::Operator::Multiply | ast::Operator::Divide => (3, 4),
        }
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: u8,
    ) -> Result<ast::Expression, ParserError> {
        let start = self.token_start;

        let mut left = match &self.token {
            Token::Punctuation(_, PunctuationKind::OpenParenthesis) => {
                self.next_token();

                let left = self.parse_expression_with_precedence(0)?;

                if let Token::Punctuation(_, PunctuationKind::CloseParenthesis) = self.token {
                    self.next_token();

                    left
                } else {
                    return Err(
                        self.expected(format!("closing parenthesis, found {:?}", self.token))
                    );
                }
            }

            _ => {
                if let Some(operator) = self.token.operator() {
                    // Consume the operator.
                    self.next_token();

                    let ((), right_precedence) = Self::prefix_precedence(operator)?;
                    let right = self.parse_expression_with_precedence(right_precedence)?;

                    let end = self.last_token_end;

                    ast::Expression::PrefixOperator(start..end, operator, Box::new(right))
                } else {
                    ast::Expression::Value(start..self.token_range().end, self.parse_atom()?)
                }
            }
        };

        loop {
            let operator = match self.token {
                Token::NewLine(_) | Token::EndOfFile(_) => break,

                _ => {
                    if let Some(operator) = self.token.operator() {
                        operator
                    } else {
                        break;
                    }
                }
            };

            let (left_precedence, right_precedence) = Parser::infix_precedence(operator);
            if left_precedence < precedence {
                break;
            }

            self.next_token();

            let right = self.parse_expression_with_precedence(right_precedence)?;

            let end = self.last_token_end;
            left = ast::Expression::InfixOperator(
                start..end,
                operator,
                Box::new(left),
                Box::new(right),
            );
        }

        Ok(left)
    }

    fn parse_atom(&mut self) -> Result<ast::Value, ParserError> {
        match self.token {
            Token::Literal(_, LiteralKind::Number(value)) => {
                self.next_token();
                Ok(ast::Value::Constant(value))
            }

            Token::Literal(_, LiteralKind::String(terminated)) => {
                let literal = self.token_source();
                if !terminated {
                    Err(self.expected("Unterminated string literal".to_owned()))
                } else if literal.len() != 3 {
                    Err(self.expected("Only character literals allowed.".to_owned()))
                } else {
                    // Consume the literal.
                    self.next_token();

                    let value = literal[1..literal.len() - 1].chars().next().unwrap() as i32;

                    Ok(ast::Value::Constant(value))
                }
            }

            Token::Identifier(len) => {
                let identifier = self.token_source();

                let start = self.token_start;
                let end = self.token_start + len;

                self.next_token();

                Ok(ast::Value::Label(ast::Label(
                    start..end,
                    identifier.to_owned(),
                )))
            }

            _ => Err(ParserError::OperandExpected(
                self.token_range(),
                format!("{}", FoundToken(self.token.clone(), self.token_source())),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! expr_const {
        ($span:expr, $value:literal) => {{
            ast::Expression::Value($span, ast::Value::Constant($value))
        }};
    }

    macro_rules! expr_label {
        ($span:expr, $value:literal) => {{
            ast::Expression::Value(
                $span,
                ast::Value::Label(ast::Label($span, $value.to_owned())),
            )
        }};
    }

    macro_rules! expr_prefix {
        ($span:expr, $operator:ident, $left:expr) => {{
            ast::Expression::PrefixOperator($span, ast::Operator::$operator, Box::new($left))
        }};
    }

    macro_rules! expr_infix {
        ($span:expr, $operator:ident, $left:expr, $right:expr) => {{
            ast::Expression::InfixOperator(
                $span,
                ast::Operator::$operator,
                Box::new($left),
                Box::new($right),
            )
        }};
    }

    macro_rules! parse_expression {
        ($source:literal) => {{
            Parser::new($source).parse_expression().unwrap()
        }};
    }

    #[test]
    fn spans() {
        let mut parser = Parser::new("one two three   four");
        assert_eq!(parser.token_start, 0);
        assert_eq!(parser.last_token_end, 0);

        parser.next_token();
        assert_eq!(parser.token_start, 4);
        assert_eq!(parser.last_token_end, 3);

        parser.next_token();
        assert_eq!(parser.token_start, 8);
        assert_eq!(parser.last_token_end, 7);

        parser.next_token();
        assert_eq!(parser.token_start, 16);
        assert_eq!(parser.last_token_end, 13);
    }

    fn collect_lines(source: &str) -> Vec<ast::Line> {
        let mut parser = Parser::new(source);
        let mut lines = vec![];
        while let Some(line) = parser.parse_line().unwrap() {
            lines.push(line)
        }
        lines
    }

    macro_rules! assert_parse {
        ($source:literal, $lines:expr) => {{
            assert_eq!(collect_lines($source), $lines);
        }};
    }

    macro_rules! assert_parse_err {
        ($source:literal, $err:expr) => {{
            let mut parser = Parser::new($source);
            assert_eq!(parser.parse_line(), Err($err));
        }};
    }

    #[test]
    fn blank_lines() {
        assert_parse!("", vec![]);
        assert_parse!("\n\n", vec![]);
    }

    #[test]
    fn tokens() {
        let mut parser = Parser::new("test label");
        assert_eq!(parser.token, Token::Identifier(4));
        parser.next_token();
        assert_eq!(parser.token, Token::Identifier(5));
        parser.next_token();
        assert_eq!(parser.token, Token::EndOfFile(0));
    }

    #[test]
    fn label_and_instruction() {
        assert_parse!(
            "start hlt",
            vec![
                ast::Line::Label(ast::Label(0..5, "start".to_owned())),
                ast::Line::Instruction(ast::Instruction {
                    span: 6..9,
                    operation: Operation::HLT,
                    operands: ast::Operands::None(9..9),
                })
            ]
        );
    }

    #[test]
    fn with_labels() {
        assert_parse!(
            "begin: hlt\nend: hlt\nhlt",
            vec![
                ast::Line::Label(ast::Label(0..5, "begin".to_owned())),
                ast::Line::Instruction(ast::Instruction {
                    span: 7..10,
                    operation: Operation::HLT,
                    operands: ast::Operands::None(10..10),
                }),
                ast::Line::Label(ast::Label(11..14, "end".to_owned())),
                ast::Line::Instruction(ast::Instruction {
                    span: 16..19,
                    operation: Operation::HLT,
                    operands: ast::Operands::None(19..19),
                }),
                ast::Line::Instruction(ast::Instruction {
                    span: 20..23,
                    operation: Operation::HLT,
                    operands: ast::Operands::None(23..23),
                }),
            ]
        );
    }

    #[test]
    fn repeat_prefix() {
        assert_parse!(
            "rep movsb\nrepne cmpsw",
            vec![
                ast::Line::Instruction(ast::Instruction {
                    span: 0..3,
                    operation: Operation::REP,
                    operands: ast::Operands::None(3..3),
                }),
                ast::Line::Instruction(ast::Instruction {
                    span: 4..9,
                    operation: Operation::MOVSB,
                    operands: ast::Operands::None(9..9),
                }),
                ast::Line::Instruction(ast::Instruction {
                    span: 10..15,
                    operation: Operation::REPNE,
                    operands: ast::Operands::None(15..15),
                }),
                ast::Line::Instruction(ast::Instruction {
                    span: 16..21,
                    operation: Operation::CMPSW,
                    operands: ast::Operands::None(21..21),
                }),
            ]
        );
    }

    #[test]
    fn constants() {
        assert_parse!(
            "label equ 42",
            vec![
                ast::Line::Label(ast::Label(0..5, "label".to_owned())),
                ast::Line::Constant(6..12, expr_const!(10..12, 42)),
            ]
        );

        assert_parse!(
            "first equ 10 ; first value\n\nsecond equ 20 ; second value\n\n",
            vec![
                ast::Line::Label(ast::Label(0..5, "first".to_owned())),
                ast::Line::Constant(6..12, expr_const!(10..12, 10)),
                ast::Line::Label(ast::Label(28..34, "second".to_owned())),
                ast::Line::Constant(35..41, expr_const!(39..41, 20)),
            ]
        );
    }

    #[test]
    fn data() {
        assert_parse!(
            "db 10, 20, 30",
            vec![ast::Line::Data(0..13, vec![10, 20, 30])]
        );
        assert_parse!(
            "dw 10, 20, 30",
            vec![ast::Line::Data(0..13, vec![10, 0, 20, 0, 30, 0])]
        );

        assert_parse_err!("db ", ParserError::DataDefinitionWithoutData(0..2));
        assert_parse_err!(
            "db test",
            ParserError::Expected(
                3..7,
                "literal expected for data definition".to_owned(),
                "identifier \"test\"".to_owned()
            )
        );
        assert_parse_err!(
            "db 10, test",
            ParserError::Expected(
                7..11,
                "literal expected for data definition".to_owned(),
                "identifier \"test\"".to_owned()
            )
        );
    }

    #[test]
    fn expression_with_precedence() {
        let expr = parse_expression!("2 + 3 * 4 + 5");
        assert_eq!(
            expr,
            expr_infix!(
                0..13,
                Add,
                expr_infix!(
                    0..9,
                    Add,
                    expr_const!(0..1, 2),
                    expr_infix!(4..9, Multiply, expr_const!(4..5, 3), expr_const!(8..9, 4))
                ),
                expr_const!(12..13, 5)
            )
        );
    }

    #[test]
    fn expression_with_non_constants() {
        let expr = parse_expression!("2 + label * 4 + 5");
        assert_eq!(
            expr,
            expr_infix!(
                0..17,
                Add,
                expr_infix!(
                    0..13,
                    Add,
                    expr_const!(0..1, 2),
                    expr_infix!(
                        4..13,
                        Multiply,
                        expr_label!(4..9, "label"),
                        expr_const!(12..13, 4)
                    )
                ),
                expr_const!(16..17, 5)
            )
        );
    }

    #[test]
    fn expression_with_prefix_operator() {
        let expr = parse_expression!("- 3 * 4");
        assert_eq!(
            expr,
            expr_infix!(
                0..7,
                Multiply,
                expr_prefix!(0..3, Subtract, expr_const!(2..3, 3)),
                expr_const!(6..7, 4)
            )
        );
    }

    #[test]
    fn indirect_encoding() {
        assert_parse!(
            "push word [cs: bx + si - 512]",
            vec![ast::Line::Instruction(ast::Instruction {
                span: 0..29,
                operation: Operation::PUSH,
                operands: ast::Operands::Destination(
                    5..29,
                    ast::Operand::Indirect(
                        10..29,
                        ast::IndirectEncoding::BxSi,
                        Some(expr_prefix!(23..28, Subtract, expr_const!(25..28, 512))),
                        Some(ast::DataSize::Word),
                        Some(ast::Segment::CS),
                    )
                )
            })]
        );

        assert_parse!(
            "push [si - 512]",
            vec![ast::Line::Instruction(ast::Instruction {
                span: 0..15,
                operation: Operation::PUSH,
                operands: ast::Operands::Destination(
                    5..15,
                    ast::Operand::Indirect(
                        5..15,
                        ast::IndirectEncoding::Si,
                        Some(expr_prefix!(9..14, Subtract, expr_const!(11..14, 512))),
                        None,
                        None,
                    )
                )
            })]
        );

        assert_parse!(
            "MOV AX,[SI+0x12]",
            vec![ast::Line::Instruction(ast::Instruction {
                span: 0..16,
                operation: Operation::MOV,
                operands: ast::Operands::DestinationAndSource(
                    4..16,
                    ast::Operand::Register(4..6, ast::Register::Word(ast::WordRegister::Ax)),
                    ast::Operand::Indirect(
                        7..16,
                        ast::IndirectEncoding::Si,
                        Some(expr_prefix!(10..15, Add, expr_const!(11..15, 18))),
                        None,
                        None,
                    )
                )
            })]
        );
    }

    #[test]
    fn far_operands() {
        assert_parse!(
            "JMP 0xF000:0xFFF0",
            vec![ast::Line::Instruction(ast::Instruction {
                span: 0..17,
                operation: Operation::JMP,
                operands: ast::Operands::Destination(
                    4..17,
                    ast::Operand::Far(
                        4..17,
                        ast::Expression::Value(11..17, ast::Value::Constant(0xFFF0)),
                        ast::Expression::Value(4..10, ast::Value::Constant(0xF000)),
                    )
                )
            })]
        )
    }
}
