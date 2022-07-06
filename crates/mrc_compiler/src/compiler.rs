use crate::ast;
use mrc_decoder::TryFromEncoding;
use mrc_instruction as out;
use out::db::Code;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum CompileError {
    InvalidOperands(ast::Span, ast::Operands),
    LabelNotFound(ast::Label),
    ConstantValueContainsVariables(ast::Span),
    ConstantWithoutLabel(ast::Span),
}

impl CompileError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompileError::InvalidOperands(span, _) => span,
            CompileError::LabelNotFound(ast::Label(span, _)) => span,
            CompileError::ConstantValueContainsVariables(span) => span,
            CompileError::ConstantWithoutLabel(span) => span,
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::InvalidOperands(_, operands) => {
                write!(f, "Invalid operands: {}", operands)
            }
            CompileError::LabelNotFound(ast::Label(_, label)) => {
                write!(f, "Label \"{}\" not found.", label)
            }
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
    line: ast::Line,
    _compiled: out::Instruction,
    size_in_bytes: u8,
}

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, usize>,
    constants: HashMap<String, i32>,

    current_labels: Vec<String>,
}

impl Compiler {
    pub fn compile(&mut self) -> Result<Vec<out::Instruction>, CompileError> {
        self.resolve_labels()?;

        Ok(vec![])
    }

    fn resolve_labels(&mut self) -> Result<(), CompileError> {
        let outputs =
            unsafe { &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len()) };

        for output in outputs.iter_mut() {
            let span = output.line.content.span().clone();
            if let ast::LineContent::Instruction(_, instruction) = &mut output.line.content {
                output._compiled = self.compile_instruction(&span, instruction)?;
            }
        }

        for output in outputs.iter_mut() {
            if let ast::LineContent::Instruction(_, instruction) = &mut output.line.content {
                if let Some(size_in_bytes) = self.size_in_bytes(instruction) {
                    output.size_in_bytes = size_in_bytes;
                }
            }
        }

        for output in outputs.iter_mut() {
            if let ast::LineContent::Instruction(_, ast::Instruction { operands, .. }) = &mut output.line.content {
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
        return Some(1);
        let id = if let Some(id) = self.find_instruction_data(instruction) {
            id
        } else {
            return Some(0);
        };

        println!("id: {:?}", id);

        let mut total = 0;
        for code in id.codes {
            match code {
                Code::Byte(_) => total += 1,
                Code::ImmByte => total += 1,
                Code::ImmWord => total += 2,
                Code::ImmByteSign => total += 1,
                Code::Disp => todo!(),
                Code::DispByte => total += 1,
                Code::DispWord => total += 2,
                Code::SegOff => total += 4,
                Code::PlusReg(_) => total += 1,
                Code::ModRegRM => match &instruction.operands {
                    ast::Operands::None(_) => unreachable!(),
                    ast::Operands::Destination(_, destination) => match destination {
                        ast::Operand::Immediate(_, _) => unreachable!(),
                        ast::Operand::Address(_, _, _, _) => todo!(),
                        ast::Operand::Register(_, _) => todo!(),
                        ast::Operand::Segment(_, _) => todo!(),
                    },
                    ast::Operands::DestinationAndSource(_, destination, source) => match (destination, source) {
                        (_, _) => todo!("{:?}, {:?}", destination, source),
                    },
                },
                Code::ModRM(_) => total += 1,
            }
        }

        Some(total)
    }
}

impl Compiler {
    fn evaluate_operand(&self, operand: &mut ast::Operand) -> Result<(), CompileError> {
        match operand {
            ast::Operand::Immediate(_, expr) => self.evaluate_expression(expr)?,
            ast::Operand::Address(_, expr, _, _) => self.evaluate_expression(expr)?,
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
                    *expression =
                        ast::Expression::Term(ast::Value::Constant(operator.evaluate(*left_value, *right_value)));
                }

                Ok(())
            }

            ast::Expression::Term(ast::Value::Label(label)) => {
                if let Some(value) = self.constants.get(label.1.as_str()) {
                    *expression = ast::Expression::Term(ast::Value::Constant(*value));
                } else if let Some(output_num) = self.labels.get(label.1.as_str()) {
                    *expression = ast::Expression::Term(ast::Value::Constant(*output_num as i32));
                } else {
                    return Err(CompileError::LabelNotFound(label.clone()));
                }

                Ok(())
            }

            ast::Expression::Term(ast::Value::Constant(_)) => Ok(()),

            // _ => todo!("{:?}", expression),
            _ => Ok(()),
        }
    }
}

fn operand_matches_operand_encoding(
    operand: &ast::Operand,
    operand_encoding: &out::db::OperandEncoding,
    size_hint: &Option<ast::DataSize>,
) -> Option<bool> {
    Some(match operand_encoding {
        out::db::OperandEncoding::None => false,

        out::db::OperandEncoding::Imm => {
            if let Some(size_hint) = size_hint {
                match size_hint {
                    ast::DataSize::Byte => matches!(operand, ast::Operand::Immediate(_, _)),
                    ast::DataSize::Word => matches!(operand, ast::Operand::Immediate(_, _)),
                }
            } else {
                println!(
                    "Size could not be determined {:?} {:?} {:?}",
                    operand, operand_encoding, size_hint
                );
                return None;
            }
        }

        out::db::OperandEncoding::Imm8 => matches!(operand, ast::Operand::Immediate(_, _)),

        out::db::OperandEncoding::RegAl => matches!(
            operand,
            ast::Operand::Register(_, ast::Register::Byte(ast::ByteRegister::Al)),
        ),

        out::db::OperandEncoding::RegAx => matches!(
            operand,
            ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Ax)),
        ),

        out::db::OperandEncoding::RegCl => matches!(
            operand,
            ast::Operand::Register(_, ast::Register::Byte(ast::ByteRegister::Cl)),
        ),

        out::db::OperandEncoding::RegCx => matches!(
            operand,
            ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Cx)),
        ),

        out::db::OperandEncoding::RegDx => matches!(
            operand,
            ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Dx)),
        ),

        out::db::OperandEncoding::Reg8 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Byte(_)))
        }

        out::db::OperandEncoding::Reg16 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Word(_)))
        }

        out::db::OperandEncoding::Seg => {
            matches!(operand, ast::Operand::Segment(_, _))
        }

        out::db::OperandEncoding::Disp8 => matches!(operand, ast::Operand::Immediate(_, _)),

        out::db::OperandEncoding::Disp16 => matches!(operand, ast::Operand::Immediate(_, _)),

        out::db::OperandEncoding::Mem => {
            if let Some(size_hint) = size_hint {
                match size_hint {
                    ast::DataSize::Byte => matches!(operand, ast::Operand::Address(_, _, _, _)),
                    ast::DataSize::Word => matches!(operand, ast::Operand::Address(_, _, _, _)),
                }
            } else {
                println!("no size hint specified");
                return None;
            }
        }

        out::db::OperandEncoding::Mem8 => {
            matches!(operand, ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _))
        }

        out::db::OperandEncoding::Mem16 => {
            matches!(operand, ast::Operand::Address(_, _, Some(ast::DataSize::Word), _))
        }

        out::db::OperandEncoding::RegMem8 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Byte(_)))
                || matches!(operand, ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _))
        }

        out::db::OperandEncoding::RegMem16 => {
            matches!(operand, ast::Operand::Register(_, ast::Register::Word(_)))
                || matches!(operand, ast::Operand::Address(_, _, Some(ast::DataSize::Word), _))
        }

        out::db::OperandEncoding::SegEs => matches!(operand, ast::Operand::Segment(_, ast::Segment::ES)),
        out::db::OperandEncoding::SegCs => matches!(operand, ast::Operand::Segment(_, ast::Segment::CS)),
        out::db::OperandEncoding::SegSs => matches!(operand, ast::Operand::Segment(_, ast::Segment::SS)),
        out::db::OperandEncoding::SegDs => matches!(operand, ast::Operand::Segment(_, ast::Segment::DS)),

        out::db::OperandEncoding::SegOff => false, // TODO

        _ => {
            todo!("operand encoding: {:?}", operand_encoding);
        }
    })
}

fn operand_set_matches_operand_encodings(
    operand_set: &ast::Operands,
    destination: &out::db::OperandEncoding,
    source: &out::db::OperandEncoding,
    size_hint: &Option<ast::DataSize>,
) -> Option<bool> {
    Some(match operand_set {
        ast::Operands::None(_) => {
            *destination == out::db::OperandEncoding::None && *source == out::db::OperandEncoding::None
        }

        ast::Operands::Destination(_, d) => {
            *source == out::db::OperandEncoding::None && operand_matches_operand_encoding(d, destination, size_hint)?
        }

        ast::Operands::DestinationAndSource(_, d, s) => {
            operand_matches_operand_encoding(d, destination, size_hint)?
                && operand_matches_operand_encoding(s, source, size_hint)?
        }
    })
}

impl Compiler {
    fn compile_instruction(
        &self,
        _span: &ast::Span,
        instruction: &mut ast::Instruction,
    ) -> Result<out::Instruction, CompileError> {
        let id = match self.find_instruction_data(instruction) {
            Some(instruction_data) => instruction_data,
            None => {
                return Err(CompileError::InvalidOperands(
                    instruction.operands.span().clone(),
                    instruction.operands.clone(),
                ));
            }
        };

        Ok(match &mut instruction.operands {
            ast::Operands::None(_) => out::Instruction::new(instruction.operation, out::OperandSet::None),
            ast::Operands::Destination(_, destination) => out::Instruction::new(
                instruction.operation,
                out::OperandSet::Destination(self.compile_operand(&id.destination, destination)?),
            ),
            ast::Operands::DestinationAndSource(_, destination, source) => out::Instruction::new(
                instruction.operation,
                out::OperandSet::DestinationAndSource(
                    self.compile_operand(&id.destination, destination)?,
                    self.compile_operand(&id.source, source)?,
                ),
            ),
        })
    }

    fn compile_operand(
        &self,
        _hint: &out::db::OperandEncoding,
        input: &mut ast::Operand,
    ) -> Result<out::Operand, CompileError> {
        Ok(match input {
            ast::Operand::Immediate(span, expr) => {
                self.evaluate_expression(expr)?;
                let value = match expr {
                    ast::Expression::Term(ast::Value::Constant(value)) => *value,
                    ast::Expression::Term(ast::Value::Label(_)) => 0,
                    _ => return Err(CompileError::ConstantValueContainsVariables(span.clone())),
                };
                if value <= u8::MAX as i32 {
                    out::Operand::Immediate(out::Immediate::Byte(value as u8))
                } else if value <= u16::MAX as i32 {
                    out::Operand::Immediate(out::Immediate::Word(value as u16))
                } else {
                    todo!("create error for out of range values")
                }
            }

            ast::Operand::Address(_, _expr, _data_size, segment) => {
                let segment = segment.clone().unwrap_or(ast::Segment::DS);
                let segment = out::Segment::try_from_encoding(segment.encoding()).unwrap();
                out::Operand::Direct(segment, 0, out::OperandSize::Byte)
            }

            ast::Operand::Register(_, register) => match register {
                ast::Register::Byte(r) => out::Operand::Register(out::SizedRegisterEncoding(
                    out::RegisterEncoding::try_from_encoding(r.encoding())
                        .map_err(|_| todo!("Error for invalid encoding"))?,
                    out::OperandSize::Byte,
                )),
                ast::Register::Word(r) => out::Operand::Register(out::SizedRegisterEncoding(
                    out::RegisterEncoding::try_from_encoding(r.encoding())
                        .map_err(|_| todo!("Error for invalid encoding"))?,
                    out::OperandSize::Word,
                )),
            },
            ast::Operand::Segment(_, seg) => {
                out::Operand::Segment(out::Segment::try_from_encoding(seg.encoding()).unwrap())
            }
        })
    }

    fn find_instruction_data(&self, instruction: &ast::Instruction) -> Option<&out::db::InstructionData> {
        for instruction_data in out::db::INSTRUCTIONS {
            if instruction.operation != instruction_data.operation {
                continue;
            }

            if operand_set_matches_operand_encodings(
                &instruction.operands,
                &instruction_data.destination,
                &instruction_data.source,
                &instruction_data_size_hint(instruction_data),
            )? {
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

            ast::LineContent::Instruction(_, _) | ast::LineContent::Data(_, _) | ast::LineContent::Times(_, _, _) => {
                if let Some(label) = label {
                    self.current_labels.push(label.1.clone());
                }

                for label in &self.current_labels {
                    self.labels.insert(label.clone(), self.outputs.len());
                }

                self.outputs.push(Output {
                    line,
                    _compiled: out::Instruction::new(out::Operation::NOP, out::OperandSet::None),
                    size_in_bytes: 0,
                });

                self.current_labels.clear();
            }

            ast::LineContent::Constant(span, expr) => {
                if let Some(label) = label {
                    self.evaluate_expression(expr)?;

                    if let ast::Expression::Term(ast::Value::Constant(value)) = expr {
                        self.constants.insert(label.1.clone(), *value);
                    } else {
                        return Err(CompileError::ConstantValueContainsVariables(span.clone()));
                    }
                } else {
                    return Err(CompileError::ConstantWithoutLabel(span.clone()));
                }
            }
        }

        Ok(())
    }
}

fn instruction_data_size_hint(id: &out::db::InstructionData) -> Option<ast::DataSize> {
    match (
        operand_encoding_size_hint(&id.destination),
        operand_encoding_size_hint(&id.source),
    ) {
        (Some(d), Some(s)) if d == s => Some(d),
        (Some(size), None) => Some(size),
        (None, Some(size)) => Some(size),
        (Some(_), Some(_)) => None,
        (None, None) => None,
    }
}

fn operand_encoding_size_hint(oe: &out::db::OperandEncoding) -> Option<ast::DataSize> {
    use out::db::OperandEncoding;
    match oe {
        OperandEncoding::None => None,
        OperandEncoding::Imm => None,
        OperandEncoding::Imm8 => Some(ast::DataSize::Byte),
        OperandEncoding::Imm16 => Some(ast::DataSize::Word),
        OperandEncoding::Sbw => None,
        OperandEncoding::Sbw8 => Some(ast::DataSize::Byte),
        OperandEncoding::Sbw16 => Some(ast::DataSize::Word),
        OperandEncoding::RegAl => Some(ast::DataSize::Byte),
        OperandEncoding::RegAx => Some(ast::DataSize::Word),
        OperandEncoding::RegCl => Some(ast::DataSize::Byte),
        OperandEncoding::RegCx => Some(ast::DataSize::Word),
        OperandEncoding::RegDx => Some(ast::DataSize::Word),
        OperandEncoding::Reg8 => Some(ast::DataSize::Byte),
        OperandEncoding::Reg16 => Some(ast::DataSize::Word),
        OperandEncoding::SegEs => Some(ast::DataSize::Word),
        OperandEncoding::SegCs => Some(ast::DataSize::Word),
        OperandEncoding::SegSs => Some(ast::DataSize::Word),
        OperandEncoding::SegDs => Some(ast::DataSize::Word),
        OperandEncoding::Seg => Some(ast::DataSize::Word),
        OperandEncoding::SegOff => None,
        OperandEncoding::Mem => None,
        OperandEncoding::Mem8 => Some(ast::DataSize::Byte),
        OperandEncoding::Mem16 => Some(ast::DataSize::Word),
        OperandEncoding::RegMem8 => Some(ast::DataSize::Byte),
        OperandEncoding::RegMem16 => Some(ast::DataSize::Word),
        OperandEncoding::Disp8 => Some(ast::DataSize::Byte),
        OperandEncoding::Disp16 => Some(ast::DataSize::Word),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::LineConsumer;

    #[test]
    fn basic() {
        let mut compiler = Compiler::default();
        compiler
            .consume(ast::Line {
                label: None,
                content: ast::LineContent::None,
            })
            .unwrap();
        println!("{:?}", compiler.compile().unwrap());
    }

    #[test]
    fn basic_2() {
        let mut compiler = Compiler::default();
        compiler
            .consume(ast::Line {
                label: None,
                content: ast::LineContent::Instruction(
                    0..0,
                    ast::Instruction {
                        operation: out::Operation::MOV,
                        operands: ast::Operands::DestinationAndSource(
                            0..0,
                            ast::Operand::Address(0..0, ast::Expression::Term(ast::Value::Constant(0)), None, None),
                            ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Al)),
                        ),
                    },
                ),
            })
            .unwrap();
        println!("{:?}", compiler.compile().unwrap());
    }
}
