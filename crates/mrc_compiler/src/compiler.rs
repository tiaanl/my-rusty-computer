use crate::{ast, ParserError};
use mrc_decoder::TryFromEncoding;
use mrc_instruction as out;
use mrc_instruction::db::{Code, OperandEncoding};
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum CompileError {
    ParseError(ast::Span, Box<ParserError>),
    InvalidOperands(ast::Span, ast::Operands),
    LabelNotFound(ast::Span, String),
    ConstantValueContainsVariables(ast::Span),
    ConstantWithoutLabel(ast::Span),
}

impl CompileError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompileError::ParseError(span, _) => span,
            CompileError::InvalidOperands(span, _) => span,
            CompileError::LabelNotFound(span, _) => span,
            CompileError::ConstantValueContainsVariables(span) => span,
            CompileError::ConstantWithoutLabel(span) => span,
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ParseError(_, err) => err.fmt(f),
            CompileError::InvalidOperands(_, operands) => {
                write!(f, "Invalid operands: {}", operands)
            }
            CompileError::LabelNotFound(_, label) => write!(f, "Label \"{}\" not found.", label),
            CompileError::ConstantValueContainsVariables(_) => {
                write!(f, "Constant value contains variables.")
            }
            CompileError::ConstantWithoutLabel(_) => {
                write!(f, "Constant declared without a label.")
            }
        }
    }
}

#[derive(Debug)]
struct Output {
    size_in_bytes: u8,
    line: ast::Line,
}

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, usize>,
    constants: HashMap<String, i32>,

    current_labels: Vec<String>,
}

impl Compiler {
    pub fn compile(&mut self) -> Result<Vec<mrc_instruction::Instruction>, CompileError> {
        self.resolve_labels()?;

        Ok(vec![])
    }

    fn resolve_labels(&mut self) -> Result<(), CompileError> {
        let outputs = unsafe {
            &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len())
        };

        for output in outputs.iter_mut() {
            if let ast::LineContent::Instruction(_, instruction) = &mut output.line.content {
                if let Some(size_in_bytes) = self.size_in_bytes(instruction) {
                    output.size_in_bytes = size_in_bytes;
                }
            }
        }

        for output in outputs.iter_mut() {
            if let ast::LineContent::Instruction(_, ast::Instruction { operands, .. }) =
                &mut output.line.content
            {
                match operands {
                    ast::Operands::None(_) => {}
                    ast::Operands::Destination(_, destination) => {
                        self.evaluate_operand(destination)?;
                    }
                    ast::Operands::DestinationAndSource(_, destination, source) => {
                        self.evaluate_operand(destination)?;
                        self.evaluate_operand(source)?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl Compiler {
    pub fn size_in_bytes(&self, instruction: &ast::Instruction) -> Option<u8> {
        let id = self.find_instruction_data(instruction)?;

        let mut total = 0;
        for code in id.codes {
            match code {
                Code::Byte(_) => total += 1,
                Code::ImmediateByte => total += 1,
            }
        }

        Some(total)
    }
}

impl Compiler {
    fn evaluate_operand(&self, operand: &mut ast::Operand) -> Result<(), CompileError> {
        match operand {
            ast::Operand::Immediate(_, expr) => self.evaluate_expression(expr)?,
            ast::Operand::Address(_, _, expr, _) => self.evaluate_expression(expr)?,
            ast::Operand::Register(_, _) => {}
            ast::Operand::Segment(_, _) => {}
        }
        Ok(())
    }

    fn evaluate_expression(&self, expression: &mut ast::Expression) -> Result<(), CompileError> {
        match expression {
            ast::Expression::InfixOperator(operator, left, right) => {
                self.evaluate_expression(left)?;
                self.evaluate_expression(right)?;

                if let (
                    ast::Expression::Term(ast::Value::Constant(left_value)),
                    ast::Expression::Term(ast::Value::Constant(right_value)),
                ) = (left.as_ref(), right.as_ref())
                {
                    *expression = ast::Expression::Term(ast::Value::Constant(
                        operator.evaluate(*left_value, *right_value),
                    ));
                }

                Ok(())
            }

            ast::Expression::Term(ast::Value::Label(label)) => {
                if let Some(value) = self.constants.get(label) {
                    *expression = ast::Expression::Term(ast::Value::Constant(*value));
                } else if let Some(output_num) = self.labels.get(label) {
                    *expression = ast::Expression::Term(ast::Value::Constant(*output_num as i32));
                } else {
                    return Err(CompileError::LabelNotFound(0..0, label.clone()));
                }

                Ok(())
            }

            ast::Expression::Term(ast::Value::Constant(_)) => Ok(()),

            _ => todo!("{:?}", expression),
        }
    }
}

fn operand_matches_operand_encoding(
    operand: &ast::Operand,
    operand_encoding: &OperandEncoding,
) -> bool {
    match operand_encoding {
        OperandEncoding::Imm8 => matches!(operand, ast::Operand::Immediate(_, _)),
        OperandEncoding::Reg8 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Byte(_)))
        }
        OperandEncoding::Reg16 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Word(_)))
        }
        OperandEncoding::Seg => {
            matches!(operand, ast::Operand::Segment(_, _))
        }
        OperandEncoding::Disp8 => matches!(operand, ast::Operand::Immediate(_, _)),
        OperandEncoding::Disp16 => matches!(operand, ast::Operand::Immediate(_, _)),

        _ => false,
    }
}

fn operand_set_matches_operand_encodings(
    operand_set: &ast::Operands,
    destination: &OperandEncoding,
    source: &OperandEncoding,
) -> bool {
    match operand_set {
        ast::Operands::None(_) => {
            *destination == OperandEncoding::None && *source == OperandEncoding::None
        }

        ast::Operands::Destination(_, d) => {
            *source == OperandEncoding::None && operand_matches_operand_encoding(d, destination)
        }

        ast::Operands::DestinationAndSource(_, d, s) => {
            operand_matches_operand_encoding(d, destination)
                && operand_matches_operand_encoding(s, source)
        }
    }
}

impl Compiler {
    fn _compile_instruction(
        &self,
        _span: &ast::Span,
        instruction: &ast::Instruction,
    ) -> Result<out::Instruction, CompileError> {
        let instruction_data = match self.find_instruction_data(instruction) {
            Some(instruction_data) => instruction_data,
            None => {
                return Err(CompileError::InvalidOperands(
                    instruction.operands.span().clone(),
                    instruction.operands.clone(),
                ));
            }
        };

        match (&instruction_data.destination, &instruction_data.source) {
            (OperandEncoding::Imm8, OperandEncoding::None) => match &instruction.operands {
                ast::Operands::Destination(_, ast::Operand::Immediate(_, _)) => {
                    todo!()
                    // Ok(out::Instruction::new(
                    //     instruction.operation,
                    //     out::OperandSet::Destination(out::Operand::Immediate(
                    //         out::Immediate::Byte(0),
                    //     )),
                    // ))
                }
                _ => unreachable!(),
            },

            (OperandEncoding::Reg16, OperandEncoding::Seg) => match &instruction.operands {
                ast::Operands::DestinationAndSource(
                    _,
                    ast::Operand::Register(_, register),
                    ast::Operand::Segment(_, segment),
                ) => {
                    let reg =
                        out::RegisterEncoding::try_from_encoding(register.encoding()).unwrap();
                    let seg = out::Segment::try_from_encoding(segment.encoding()).unwrap();

                    Ok(out::Instruction::new(
                        instruction.operation,
                        out::OperandSet::DestinationAndSource(
                            out::Operand::Register(out::SizedRegisterEncoding(
                                reg,
                                out::OperandSize::Word,
                            )),
                            out::Operand::Segment(seg),
                        ),
                    ))
                }
                _ => unreachable!(),
            },

            (OperandEncoding::Seg, OperandEncoding::Reg16) => match &instruction.operands {
                ast::Operands::DestinationAndSource(
                    _,
                    ast::Operand::Segment(_, segment),
                    ast::Operand::Register(_, register),
                ) => {
                    let seg = out::Segment::try_from_encoding(segment.encoding()).unwrap();
                    let reg =
                        out::RegisterEncoding::try_from_encoding(register.encoding()).unwrap();

                    Ok(out::Instruction::new(
                        instruction.operation,
                        out::OperandSet::DestinationAndSource(
                            out::Operand::Segment(seg),
                            out::Operand::Register(out::SizedRegisterEncoding(
                                reg,
                                out::OperandSize::Word,
                            )),
                        ),
                    ))
                }
                _ => unreachable!(),
            },

            // (OperandEncoding::Disp8, OperandEncoding::None) => match &instruction.operands {
            //     ast::Operands::Destination(
            //         _,
            //         ast::Operand::Immediate(
            //             _,
            //             ast::Expression::Term(ast::Value::Constant(output_num)),
            //         ),
            //     ) => {
            //         dbg!(output_num);
            //         todo!()
            //     }
            //     _ => unreachable!(),
            // },
            _ => Err(CompileError::InvalidOperands(
                instruction.operands.span().clone(),
                instruction.operands.clone(),
            )),
        }
    }

    fn find_instruction_data(
        &self,
        instruction: &ast::Instruction,
    ) -> Option<&mrc_instruction::db::InstructionData> {
        for instruction_data in mrc_instruction::db::INSTRUCTIONS {
            if instruction.operation != instruction_data.operation {
                continue;
            }

            if operand_set_matches_operand_encodings(
                &instruction.operands,
                &instruction_data.destination,
                &instruction_data.source,
            ) {
                return Some(instruction_data);
            }
        }

        None
    }
}

impl crate::LineConsumer for Compiler {
    type Err = CompileError;

    fn consume(&mut self, mut line: ast::Line) -> Result<(), Self::Err> {
        let ast::Line { label, content } = &mut line;
        match content {
            ast::LineContent::None => {
                if let Some(label) = label {
                    self.current_labels.push(label.1.clone());
                }
            }

            ast::LineContent::Instruction(_, _)
            | ast::LineContent::Data(_, _)
            | ast::LineContent::Times(_, _, _) => {
                for label in &self.current_labels {
                    self.labels.insert(label.clone(), self.outputs.len());
                }
                self.outputs.push(Output {
                    size_in_bytes: 0,
                    line,
                });
                self.current_labels.clear();
            }

            ast::LineContent::Constant(span, expr) => {
                if label.is_none() {
                    return Err(CompileError::ConstantWithoutLabel(0..0));
                }

                self.evaluate_expression(expr)?;

                if let ast::Expression::Term(ast::Value::Constant(value)) = expr {
                    for label in &self.current_labels {
                        self.constants.insert(label.clone(), *value);
                    }
                    self.current_labels.clear();
                } else {
                    return Err(CompileError::ConstantValueContainsVariables(span.clone()));
                }
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile(source: &str) -> Result<(), CompileError> {
        let mut compiler = Compiler::default();

        let _ = crate::parse(source, &mut compiler)
            .map_err(|err| CompileError::ParseError(err.span().clone(), Box::new(err)));

        println!("{:?}", compiler.labels);
        println!("{:?}", compiler.constants);
        println!("{:?}", compiler.outputs);

        compiler.compile()?;

        Ok(())
    }

    #[test]
    fn basic() {
        compile("int_num equ 10 + 6\nmain: int int_num\njmp main").unwrap();
    }
}
