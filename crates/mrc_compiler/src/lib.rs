use mrc_instruction::db::OperandEncoding;
use mrc_instruction::{Instruction as OutInstruction, OperandSet as OutOperandSet};
use mrc_parser::ast;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum CompileError {
    ParserError(mrc_parser::ParserError),
    InvalidOperands(ast::Span, ast::Operands),
    LabelNotFound(String),
    Unknown,
}

impl CompileError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompileError::ParserError(_) => todo!(),
            CompileError::InvalidOperands(span, _) => span,
            CompileError::LabelNotFound(_) => todo!(),
            CompileError::Unknown => todo!(),
        }
    }
}

impl From<mrc_parser::ParserError> for CompileError {
    fn from(other: mrc_parser::ParserError) -> Self {
        CompileError::ParserError(other)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ParserError(err) => err.fmt(f),
            CompileError::InvalidOperands(_, operands) => {
                write!(f, "Invalid operands: {}", operands)
            }
            CompileError::LabelNotFound(label) => write!(f, "Label \"{}\" not found.", label),
            CompileError::Unknown => write!(f, "Unknown compiler error"),
        }
    }
}

#[derive(Debug)]
struct Output {
    _line: ast::Line,
}

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, usize>,
    constants: HashMap<String, i32>,

    current_labels: Vec<String>,
}

impl Compiler {
    pub fn compile(&self) -> Result<Vec<mrc_instruction::Instruction>, CompileError> {
        for output in &self.outputs {
            match &output._line {
                ast::Line::Instruction(span, instruction) => {
                    println!("{:?}", instruction);

                    self.compile_instruction(span, instruction)?;
                }

                _ => unreachable!(),
            }
        }

        Ok(vec![])
    }
}

impl Compiler {
    fn _evaluate_operand(&self, operand: &mut ast::Operand) -> Result<(), CompileError> {
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
                    return Err(CompileError::LabelNotFound(label.clone()));
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
            *source == OperandEncoding::None && operand_matches_operand_encoding(&d, destination)
        }

        ast::Operands::DestinationAndSource(_, d, s) => {
            operand_matches_operand_encoding(&d, destination)
                && operand_matches_operand_encoding(&s, source)
        }
    }
}

impl Compiler {
    fn compile_instruction(
        &self,
        _span: &ast::Span,
        instruction: &ast::Instruction,
    ) -> Result<OutInstruction, CompileError> {
        let instruction_data = match self.find_instruction_data(instruction) {
            Some(instruction_data) => instruction_data,
            None => {
                return Err(CompileError::InvalidOperands(
                    instruction.operands.span().clone(),
                    instruction.operands.clone(),
                ))
            }
        };

        match (&instruction_data.destination, &instruction_data.source) {
            (OperandEncoding::Imm8, OperandEncoding::None) => {
                return Ok(OutInstruction::new(
                    instruction.operation,
                    OutOperandSet::Destination(mrc_instruction::Operand::Immediate(
                        mrc_instruction::Immediate::Byte(0),
                    )),
                ));
            }

            _ => todo!(),
        }

        //println!("{:?}", instruction_data);

        //todo!()
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

impl mrc_parser::LineConsumer for Compiler {
    fn consume(&mut self, mut line: ast::Line) -> bool {
        match &mut line {
            ast::Line::Label(_, label) => self.current_labels.push(label.clone()),

            // ast::Line::Instruction(_, instruction) => {
            //     match &mut instruction.operands {
            //         ast::Operands::None(_) => {}
            //         ast::Operands::Destination(_, destination) => {
            //             if let Err(err) = self.evaluate_operand(destination) {
            //                 eprintln!("{}", err);
            //                 return false;
            //             }
            //         }
            //         ast::Operands::DestinationAndSource(_, destination, source) => {
            //             if let Err(err) = self.evaluate_operand(destination) {
            //                 eprintln!("{}", err);
            //                 return false;
            //             }
            //             if let Err(err) = self.evaluate_operand(source) {
            //                 eprintln!("{}", err);
            //                 return false;
            //             }
            //         }
            //     }
            //
            //     for label in &self.current_labels {
            //         self.labels.insert(label.clone(), self.outputs.len());
            //     }
            //     self.outputs.push(Output { _line: line });
            //     self.current_labels.clear();
            // }
            ast::Line::Instruction(_, _) | ast::Line::Data(_, _) => {
                for label in &self.current_labels {
                    self.labels.insert(label.clone(), self.outputs.len());
                }
                self.outputs.push(Output { _line: line });
                self.current_labels.clear();
            }

            ast::Line::Constant(_, expr) => {
                if let Err(err) = self.evaluate_expression(expr) {
                    eprintln!("{}", err);
                    return false;
                }

                if let ast::Expression::Term(ast::Value::Constant(value)) = expr {
                    for label in &self.current_labels {
                        self.constants.insert(label.clone(), *value);
                    }
                    self.current_labels.clear();
                } else {
                    println!("Constant value contains variables!");
                    return false;
                }
            }

            ast::Line::Times(_) => todo!(),
        }

        true
    }
}

pub fn compile(source: &str) -> Result<(), CompileError> {
    let mut compiler = Compiler::default();

    mrc_parser::parse(source, &mut compiler)?;

    println!("{:?}", compiler.labels);
    println!("{:?}", compiler.constants);
    println!("{:?}", compiler.outputs);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        compile("int_num equ 10 + 6\nmain: int int_num\njmp main").unwrap();
    }
}
