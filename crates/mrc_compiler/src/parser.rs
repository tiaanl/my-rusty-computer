use crate::ast;
use crate::lexer::{Cursor, LiteralKind, PunctuationKind, Token};
use mrc_instruction::Operation;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq)]
pub enum ParserError<E> {
    Expected(ast::Span, String),
    IdentifierOrLabelExpected(ast::Span, String),
    InvalidPrefixOperator(ast::Span),
    DataDefinitionWithoutData(ast::Span),

    /// The LineConsumer asked that the parsing be stopped.
    Stopped(ast::Span, E),
}

impl<E> ParserError<E> {
    pub fn span(&self) -> &ast::Span {
        match self {
            ParserError::Expected(span, _)
            | ParserError::IdentifierOrLabelExpected(span, _)
            | ParserError::InvalidPrefixOperator(span)
            | ParserError::DataDefinitionWithoutData(span)
            | ParserError::Stopped(span, _) => span,
        }
    }
}

impl<E: Display> Display for ParserError<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Expected(_, expected) => write!(f, "expected {}", expected),
            ParserError::IdentifierOrLabelExpected(_, found) => {
                write!(f, "Identifier or label expected, found: {}", found)
            }
            ParserError::InvalidPrefixOperator(_) => write!(f, "Invalid prefix operator"),
            ParserError::DataDefinitionWithoutData(_) => write!(f, "Data definition without data"),
            ParserError::Stopped(_, e) => write!(f, "Parsing stopped: {}", e),
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

pub trait LineConsumer {
    type Err;

    fn consume(&mut self, line: ast::Line) -> Result<(), Self::Err>;
}

/// Allow lambdas to be passed as `LineConsumer`s
impl<'a, E, T: FnMut(ast::Line) -> Result<(), E>> LineConsumer for T {
    type Err = E;

    #[inline]
    fn consume(&mut self, line: ast::Line) -> Result<(), E> {
        self(line)
    }
}

#[inline]
pub fn parse<E>(
    source: &str,
    consumer: &mut impl LineConsumer<Err = E>,
) -> Result<(), ParserError<E>> {
    Parser::new(source).parse(consumer)
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

struct Parser<'a> {
    cursor: Cursor<'a>,

    /// The current token we are parsing.
    token: Token,

    /// Position in the cursor where the current token starts.
    token_start: usize,

    // Position in the cursor where the last meaningful token ended.
    last_token_end: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
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

    fn token_source(&self) -> &'a str {
        self.cursor.source_at(self.token_start, self.token.len())
    }

    fn parse<E>(
        &mut self,
        consumer: &mut impl LineConsumer<Err = E>,
    ) -> Result<(), ParserError<E>> {
        macro_rules! push_and_check {
            ($line:expr) => {{
                if let Err(err) = consumer.consume($line) {
                    return Err(ParserError::Stopped(
                        self.token_start..self.token_start + self.token.len(),
                        err,
                    ));
                }
            }};
        }

        loop {
            match self.token {
                Token::NewLine(_) => {
                    // If the line starts with a new line, we just skip it.
                    self.next_token();
                }

                Token::Identifier(_) => {
                    let identifier = self.token_source();
                    if let Ok(operation) = Operation::from_str(identifier) {
                        let line = self.parse_instruction(operation)?;
                        push_and_check!(line);
                    } else {
                        match identifier.to_lowercase().as_str() {
                            "equ" => {
                                let line = self.parse_equ()?;
                                push_and_check!(line);
                            }

                            "db" => {
                                let line = self.parse_data(1)?;
                                push_and_check!(line);
                            }

                            "dw" => {
                                let line = self.parse_data(2)?;
                                push_and_check!(line);
                            }

                            "dd" => {
                                let line = self.parse_data(4)?;
                                push_and_check!(line);
                            }

                            "times" => {
                                let line = self.parse_times()?;
                                push_and_check!(line);
                            }

                            _ => {
                                // If no known keyword is found, we assume the identifier is a
                                // label.
                                let line = self.parse_label(identifier)?;
                                push_and_check!(line);
                            }
                        }
                    }
                }

                Token::EndOfFile(_) => break,

                _ => {
                    return Err(ParserError::IdentifierOrLabelExpected(
                        self.token_start..self.token_start + self.token.len(),
                        format!("{}", FoundToken(self.token.clone(), self.token_source())),
                    ));
                }
            }
        }

        Ok(())
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
    fn require_new_line<E>(&mut self) -> Result<(), ParserError<E>> {
        if let Token::NewLine(_) = self.token {
            self.next_token();
            Ok(())
        } else if let Token::EndOfFile(_) = self.token {
            Ok(())
        } else {
            Err(self.expected("Expected new line".to_owned()))
        }
    }

    fn parse_label<E>(&mut self, name: &'a str) -> Result<ast::Line, ParserError<E>> {
        let start = self.token_start;

        // We only capture the label part.
        let end = self.token_start + self.token.len();

        // Consume the token that holds the label.
        self.next_token();

        // Skip the optional colon after a label.
        if matches!(self.token, Token::Punctuation(_, PunctuationKind::Colon)) {
            self.next_token();
        }

        // We can have a label on a single line, so consume any new lines as well.
        if matches!(self.token, Token::NewLine(_)) {
            self.next_token();
        }

        Ok(ast::Line::Label(start..end, name.to_owned()))
    }

    fn parse_instruction<E>(&mut self, operation: Operation) -> Result<ast::Line, ParserError<E>> {
        let start = self.token_start;

        // Consume the operation.
        self.next_token();

        let operands = self.parse_operands()?;

        let end = self.last_token_end;

        self.require_new_line()?;

        Ok(ast::Line::Instruction(
            start..end,
            ast::Instruction {
                operation,
                operands,
            },
        ))
    }

    fn parse_operands<E>(&mut self) -> Result<ast::Operands, ParserError<E>> {
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

                _ => Err(self.expected("An unexpected token was encountered.".to_owned())),
            }
        }
    }

    fn parse_operand<E>(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand, ParserError<E>> {
        let start = self.token_start;
        match self.token {
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
                    let expression = self.parse_expression()?;
                    Ok(ast::Operand::Immediate(
                        start..self.last_token_end,
                        expression,
                    ))
                }
            }

            _ => {
                let start = self.token_start;
                let expression = self.parse_expression()?;
                Ok(ast::Operand::Immediate(
                    start..self.last_token_end,
                    expression,
                ))
            }
        }
    }

    fn parse_expression<E>(&mut self) -> Result<ast::Expression, ParserError<E>> {
        self.parse_expression_with_precedence(0)
    }

    fn parse_memory_operand<E>(
        &mut self,
        data_size: Option<ast::DataSize>,
    ) -> Result<ast::Operand, ParserError<E>> {
        assert!(matches!(
            self.token,
            Token::Punctuation(_, PunctuationKind::OpenBracket)
        ));

        let start = self.token_start;

        // Consume the opening bracket.
        self.next_token();

        let mut segment_override = None;

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

                self.parse_expression()?
            }
            _ => todo!(),
        };

        if matches!(
            self.token,
            Token::Punctuation(_, PunctuationKind::CloseBracket)
        ) {
            self.next_token();
        } else {
            return Err(self.expected("closing bracket for memory address".to_owned()));
        }

        Ok(ast::Operand::Address(
            start..self.last_token_end,
            data_size,
            expression,
            segment_override,
        ))
    }

    fn parse_equ<E>(&mut self) -> Result<ast::Line, ParserError<E>> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        let start = self.token_start;

        // Consume the "equ" keyword.
        self.next_token();

        let expression = self.parse_expression()?;

        let end = self.last_token_end;

        self.require_new_line()?;

        Ok(ast::Line::Constant(start..end, expression))
    }

    fn parse_data<E>(&mut self, bytes_per_value: usize) -> Result<ast::Line, ParserError<E>> {
        debug_assert!(matches!(self.token, Token::Identifier(_)));

        let data_definition_token_span = self.token_start..self.token_start + self.token.len();

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

                Token::Literal(_, LiteralKind::String(_)) => {
                    let source = self.token_source();
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

                _ => {
                    return Err(ParserError::Expected(
                        self.token_start..self.token_start + self.token.len(),
                        "literal expected for data definition".to_owned(),
                    ))
                }
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

    fn parse_times<E>(&mut self) -> Result<ast::Line, ParserError<E>> {
        self.next_token();

        let expression = self.parse_expression()?;

        Ok(ast::Line::Times(expression))
    }

    fn expected<E>(&self, message: String) -> ParserError<E> {
        ParserError::Expected(0..0, message)
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
    fn prefix_precedence<E>(operator: ast::Operator) -> Result<((), u8), ParserError<E>> {
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

    fn parse_expression_with_precedence<E>(
        &mut self,
        precedence: u8,
    ) -> Result<ast::Expression, ParserError<E>> {
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
                    let ((), right_precedence) = Self::prefix_precedence(operator)?;
                    let right = self.parse_expression_with_precedence(right_precedence)?;
                    ast::Expression::PrefixOperator(operator, Box::new(right))
                } else {
                    ast::Expression::Term(self.parse_atom()?)
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

            left = ast::Expression::InfixOperator(operator, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_atom<E>(&mut self) -> Result<ast::Value, ParserError<E>> {
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

            Token::Identifier(_) => {
                let identifier = self.token_source();

                self.next_token();

                if let Ok(register) = ast::Register::from_str(identifier) {
                    Ok(ast::Value::Register(register))
                } else {
                    Ok(ast::Value::Label(identifier.to_owned()))
                }
            }

            _ => Err(self.expected("Constant, label or register expected.".to_owned())),
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//
//     macro_rules! expr_const {
//         ($value:literal) => {{
//             ast::Expression::Term(ast::Value::Constant($value))
//         }};
//     }
//
//     macro_rules! expr_label {
//         ($value:literal) => {{
//             ast::Expression::Term(ast::Value::Label($value.to_owned()))
//         }};
//     }
//
//     macro_rules! expr_infix {
//         ($operator:ident, $left:expr, $right:expr) => {{
//             ast::Expression::InfixOperator(
//                 ast::Operator::$operator,
//                 Box::new($left),
//                 Box::new($right),
//             )
//         }};
//     }
//
//     macro_rules! parse_expression {
//         ($source:literal) => {{
//             Parser::new($source).parse_expression().unwrap()
//         }};
//     }
//
//     #[test]
//     fn spans() {
//         let mut parser = Parser::new("one two three   four");
//         assert_eq!(parser.token_start, 0);
//         assert_eq!(parser.last_token_end, 0);
//
//         parser.next_token();
//         assert_eq!(parser.token_start, 4);
//         assert_eq!(parser.last_token_end, 3);
//
//         parser.next_token();
//         assert_eq!(parser.token_start, 8);
//         assert_eq!(parser.last_token_end, 7);
//
//         parser.next_token();
//         assert_eq!(parser.token_start, 16);
//         assert_eq!(parser.last_token_end, 13);
//     }
//
//     fn collect_lines(source: &str) -> Vec<ast::Line> {
//         let mut parser = Parser::new(source);
//         let mut lines = vec![];
//         parser
//             .parse(&mut |line| {
//                 lines.push(line);
//             })
//             .unwrap();
//         lines
//     }
//
//     macro_rules! assert_parse {
//         ($source:literal, $lines:expr) => {{
//             assert_eq!(collect_lines($source), $lines);
//         }};
//     }
//
//     macro_rules! assert_parse_err {
//         ($source:literal, $err:expr) => {{
//             let mut parser = Parser::new($source);
//             assert_eq!(parser.parse(&mut |_| {}), Err($err));
//         }};
//     }
//
//     #[test]
//     fn blank_lines() {
//         assert_parse!("", vec![]);
//         assert_parse!("\n\n", vec![]);
//     }
//
//     #[test]
//     fn tokens() {
//         let mut parser = Parser::new("test label");
//         assert_eq!(parser.token, Token::Identifier(4));
//         parser.next_token();
//         assert_eq!(parser.token, Token::Identifier(5));
//         parser.next_token();
//         assert_eq!(parser.token, Token::EndOfFile(0));
//     }
//
//     #[test]
//     fn label_and_instruction() {
//         assert_parse!(
//             "start hlt",
//             vec![
//                 ast::Line::Label(0..5, "start".to_owned()),
//                 ast::Line::Instruction(
//                     6..9,
//                     ast::Instruction {
//                         operation: Operation::HLT,
//                         operands: ast::Operands::None(9..9),
//                     }
//                 )
//             ]
//         );
//     }
//
//     #[test]
//     fn multiple_labels() {
//         assert_parse!(
//             "start begin: begin2:hlt",
//             vec![
//                 ast::Line::Label(0..5, "start".to_owned()),
//                 ast::Line::Label(6..11, "begin".to_owned()),
//                 ast::Line::Label(13..19, "begin2".to_owned()),
//                 ast::Line::Instruction(
//                     20..23,
//                     ast::Instruction {
//                         operation: Operation::HLT,
//                         operands: ast::Operands::None(23..23),
//                     },
//                 ),
//             ]
//         );
//     }
//
//     #[test]
//     fn constants() {
//         assert_parse!(
//             "label equ 42",
//             vec![
//                 ast::Line::Label(0..5, "label".to_owned()),
//                 ast::Line::Constant(6..12, expr_const!(42)),
//             ]
//         );
//
//         assert_parse!(
//             "first equ 10 ; first value\n\nsecond equ 20 ; second value\n\n",
//             vec![
//                 ast::Line::Label(0..5, "first".to_owned()),
//                 ast::Line::Constant(6..12, expr_const!(10)),
//                 ast::Line::Label(28..34, "second".to_owned()),
//                 ast::Line::Constant(35..41, expr_const!(20)),
//             ]
//         );
//     }
//
//     #[test]
//     fn data() {
//         assert_parse!(
//             "db 10, 20, 30",
//             vec![ast::Line::Data(0..13, vec![10, 20, 30])]
//         );
//         assert_parse!(
//             "dw 10, 20, 30",
//             vec![ast::Line::Data(0..13, vec![10, 0, 20, 0, 30, 0])]
//         );
//         assert_parse!(
//             "dd 10, 20, 30",
//             vec![ast::Line::Data(
//                 0..13,
//                 vec![10, 0, 0, 0, 20, 0, 0, 0, 30, 0, 0, 0]
//             ),]
//         );
//
//         assert_parse_err!("db ", ParserError::DataDefinitionWithoutData(0..2));
//         assert_parse_err!(
//             "db test",
//             ParserError::Expected(3..7, "literal expected for data definition".to_owned())
//         );
//         assert_parse_err!(
//             "db 10, test",
//             ParserError::Expected(7..11, "literal expected for data definition".to_owned())
//         );
//     }
//
//     #[test]
//     fn expression_errors() {
//         let expr = parse_expression!("2 + 3 * 4 + 5");
//         assert_eq!(
//             expr,
//             ast::Expression::InfixOperator(
//                 ast::Operator::Add,
//                 Box::new(ast::Expression::InfixOperator(
//                     ast::Operator::Add,
//                     Box::new(ast::Expression::Term(ast::Value::Constant(2))),
//                     Box::new(ast::Expression::InfixOperator(
//                         ast::Operator::Multiply,
//                         Box::new(ast::Expression::Term(ast::Value::Constant(3))),
//                         Box::new(ast::Expression::Term(ast::Value::Constant(4))),
//                     )),
//                 )),
//                 Box::new(ast::Expression::Term(ast::Value::Constant(5))),
//             )
//         );
//     }
//
//     #[test]
//     fn expression_with_precedence() {
//         let expr = parse_expression!("2 + 3 * 4 + 5");
//         assert_eq!(
//             expr,
//             expr_infix!(
//                 Add,
//                 expr_infix!(
//                     Add,
//                     expr_const!(2),
//                     expr_infix!(Multiply, expr_const!(3), expr_const!(4))
//                 ),
//                 expr_const!(5)
//             )
//         );
//     }
//
//     #[test]
//     fn expression_with_non_constants() {
//         let expr = parse_expression!("2 + label * 4 + 5");
//         assert_eq!(
//             expr,
//             expr_infix!(
//                 Add,
//                 expr_infix!(
//                     Add,
//                     expr_const!(2),
//                     expr_infix!(Multiply, expr_label!("label"), expr_const!(4))
//                 ),
//                 expr_const!(5)
//             )
//         );
//     }
//
//     #[test]
//     fn expression_with_non_constants_and_constants() {
//         let expr = parse_expression!("label + 2 * 3");
//         assert_eq!(
//             expr,
//             expr_infix!(
//                 Add,
//                 expr_label!("label"),
//                 expr_infix!(Multiply, expr_const!(2), expr_const!(3))
//             )
//         );
//     }
// }
