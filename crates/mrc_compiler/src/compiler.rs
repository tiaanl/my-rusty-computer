use crate::ast;
use mrc_instruction as out;
use std::collections::{HashMap, LinkedList};
use std::fmt::Formatter;

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum CompileError {
    InvalidOperands(
        ast::Span,
        ast::Instruction,
        Vec<&'static out::db::InstructionData>,
    ),
    LabelNotFound(ast::Label),
    ConstantValueContainsVariables(ast::Span),
    ConstantValueContainsLabel(ast::Label),
    ConstantWithoutLabel(ast::Span),
    ImmediateValueOutOfRange(ast::Span, i32),
    UnresolvedReference(ast::Label),
}

impl CompileError {
    pub fn span(&self) -> &ast::Span {
        match self {
            CompileError::InvalidOperands(span, _, _)
            | CompileError::LabelNotFound(ast::Label(span, _))
            | CompileError::ConstantValueContainsVariables(span)
            | CompileError::ConstantValueContainsLabel(ast::Label(span, _))
            | CompileError::ConstantWithoutLabel(span)
            | CompileError::ImmediateValueOutOfRange(span, _)
            | CompileError::UnresolvedReference(ast::Label(span, _)) => span,
        }
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::InvalidOperands(_, operands, possible) => {
                write!(
                    f,
                    "Invalid operands: {}\nPossible: {:?}",
                    operands,
                    possible
                        .iter()
                        .map(|e| format!("({:?}, {:?})", e.destination, e.source))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            CompileError::LabelNotFound(ast::Label(_, label)) => {
                write!(f, "Label \"{}\" not found.", label)
            }
            CompileError::ConstantValueContainsVariables(_) => {
                write!(f, "Constant value contains variables.")
            }
            CompileError::ConstantValueContainsLabel(label) => {
                write!(f, "Constant value contains a label \"{}\".", label.1)
            }
            CompileError::ConstantWithoutLabel(_) => {
                write!(f, "Constant declared without a label.")
            }
            CompileError::ImmediateValueOutOfRange(_, value) => {
                write!(f, "Immediate value out of range ({})", value)
            }
            CompileError::UnresolvedReference(label) => {
                write!(f, "Unresolved reference: {}", label.1)
            }
        }
    }
}

#[derive(Debug)]
struct Output {
    line: ast::Line,
    size: u16,
    unresolved_references: bool,
}

#[derive(Debug)]
struct LabelInfo {
    offset: Option<u16>,
    original: ast::Label,
}

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, LabelInfo>,
    constants: HashMap<String, i32>,
}

impl Compiler {
    pub fn compile(&mut self) -> Result<Vec<u8>, CompileError> {
        if self.resolve_labels()? > 0 {
            let label = self
                .labels
                .iter()
                .filter(|(_, li)| li.offset.is_none())
                .map(|(_, li)| li.original.clone())
                .next()
                .unwrap();
            return Err(CompileError::UnresolvedReference(label));
        }

        self.debug_print_outputs();

        Ok(vec![])
    }

    fn debug_print_outputs(&self) {
        let mut offset = 0;
        for output in &self.outputs {
            if matches!(&output.line, ast::Line::Label(..) | ast::Line::Constant(..)) {
                continue;
            }

            println!(
                "{:04X} {:04X} {} {}",
                offset,
                output.size,
                if output.unresolved_references {
                    "x"
                } else {
                    " "
                },
                output.line
            );
            offset += output.size;
        }
    }

    fn resolve_labels(&mut self) -> Result<usize, CompileError> {
        let mut last_offset = u16::MAX;

        let mut labels = LinkedList::new();

        loop {
            // When we start a pass, there should not be any labels left over from a previous pass.
            debug_assert!(labels.is_empty());

            let mut unresolved_references = 0;
            let mut offset = 0;

            let outputs = unsafe {
                &mut *std::ptr::slice_from_raw_parts_mut(
                    self.outputs.as_mut_ptr(),
                    self.outputs.len(),
                )
            };

            macro_rules! drain_labels {
                () => {{
                    while let Some(label) = labels.pop_back() {
                        self.set_label_offset(&label, Some(offset));
                    }
                }};
            }

            for output in outputs {
                macro_rules! line_size {
                    ($line:expr) => {{
                        match self.calculate_line_size($line) {
                            Ok(size) => {
                                output.unresolved_references = false;
                                size
                            }
                            Err(CompileError::LabelNotFound(label)) => {
                                // If the label wasn't found, then we add the label without an
                                // offset, stating that it is unresolved.
                                self.set_label_offset(&label, None);
                                unresolved_references += 1;
                                output.unresolved_references = true;
                                0
                            }
                            Err(err) => return Err(err),
                        }
                    }};
                }

                match &mut output.line {
                    ast::Line::Label(label) => {
                        labels.push_back(label.clone());
                    }

                    line @ ast::Line::Instruction(..) => {
                        drain_labels!();
                        let size = line_size!(line);
                        output.size = size;
                        offset += size;
                    }

                    ast::Line::Data(_, data) => {
                        drain_labels!();
                        let size = data.len() as u16;
                        output.size = size;
                        offset += size;
                    }

                    ast::Line::Constant(span, expr) => {
                        if let Some(label) = labels.pop_back() {
                            let value = self.evaluate_expression(expr)?;
                            self.constants.insert(label.1.clone(), value);
                        } else {
                            return Err(CompileError::ConstantWithoutLabel(span.clone()));
                        }
                    }

                    ast::Line::Times(_, times_expr, inner_line) => {
                        let times = self.evaluate_expression(times_expr)?;

                        let mut size = 0;
                        for _ in 0..times {
                            size += line_size!(inner_line.as_ref());
                        }

                        output.size = size;
                        offset += size;
                    }
                }
            }

            drain_labels!();

            // If there are no more unresolved references, then we can stop.
            if unresolved_references == 0 {
                return Ok(0);
            }

            // Now we know there are unsolved references, but if we did not get new sizes for any
            // of them, then we are done.
            if offset == last_offset {
                return Ok(unresolved_references);
            }

            last_offset = offset;
        }
    }

    fn set_label_offset(&mut self, label: &ast::Label, offset: Option<u16>) {
        if let Some(li) = self.labels.get_mut(label.1.as_str()) {
            li.offset = offset;
        } else {
            self.labels.insert(
                label.1.clone(),
                LabelInfo {
                    offset,
                    original: label.clone(),
                },
            );
        }
    }
}

impl Compiler {
    fn evaluate_expression(&self, expression: &ast::Expression) -> Result<i32, CompileError> {
        match expression {
            ast::Expression::PrefixOperator(_, operator, expr) => {
                let value = self.evaluate_expression(expr)?;
                Ok(operator.evaluate(0, value))
            }

            ast::Expression::InfixOperator(_, operator, left, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;

                Ok(operator.evaluate(left, right))
            }

            ast::Expression::Value(_, ast::Value::Label(label)) => {
                if let Some(value) = self.constants.get(label.1.as_str()) {
                    Ok(*value)
                } else if let Some(LabelInfo {
                    offset: Some(label_offset),
                    ..
                }) = self.labels.get(label.1.as_str())
                {
                    Ok(*label_offset as i32)
                } else {
                    Err(CompileError::LabelNotFound(label.clone()))
                }
            }

            ast::Expression::Value(_, ast::Value::Constant(value)) => Ok(*value),
        }
    }
}

impl Compiler {
    fn calculate_line_size(&self, line: &ast::Line) -> Result<u16, CompileError> {
        Ok(match line {
            ast::Line::Instruction(instruction) => {
                match self.calculate_instruction_size(instruction) {
                    Ok(size) => size,
                    Err(err) => return Err(err),
                }
            }

            ast::Line::Data(_, data) => data.len() as u16,

            _ => todo!(),
        })
    }

    fn calculate_instruction_size(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<u16, CompileError> {
        let idata = self.find_instruction_data(instruction)?;

        let mut total_size = 0;

        for code in idata.codes {
            match code {
                out::db::Code::ModRegRM => total_size += 1,

                out::db::Code::ModRM(_) => total_size += 1,

                out::db::Code::PlusReg(_) => total_size += 1,

                out::db::Code::Byte(_) | out::db::Code::ImmByte => total_size += 1,

                out::db::Code::ImmWord => total_size += 2,

                out::db::Code::DispByte => total_size += 1,

                out::db::Code::DispWord => total_size += 2,

                out::db::Code::ImmByteSign => {
                    // TODO: Not sure if this correct!
                    total_size += 2;
                }

                _ => todo!("{:?}", code),
            }
        }

        Ok(total_size)
    }
}

impl Compiler {
    fn find_instruction_data(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<&out::db::InstructionData, CompileError> {
        for instruction_data in out::db::INSTRUCTIONS {
            if instruction.operation != instruction_data.operation {
                continue;
            }

            if self.operand_set_matches_operand_encodings(
                &instruction.operands,
                &instruction_data.destination,
                &instruction_data.source,
                instruction_data_size_hint(instruction_data),
            )? {
                return Ok(instruction_data);
            }
        }

        let possible: Vec<&out::db::InstructionData> = out::db::INSTRUCTIONS
            .iter()
            .filter(|ie| ie.operation == instruction.operation)
            .collect();

        Err(CompileError::InvalidOperands(
            instruction.operands.span().clone(),
            instruction.clone(),
            possible,
        ))
    }

    fn operand_set_matches_operand_encodings(
        &self,
        operand_set: &ast::Operands,
        destination: &out::db::OperandEncoding,
        source: &out::db::OperandEncoding,
        size_hint: Option<ast::DataSize>,
    ) -> Result<bool, CompileError> {
        Ok(match operand_set {
            ast::Operands::None(_) => {
                *destination == out::db::OperandEncoding::None
                    && *source == out::db::OperandEncoding::None
            }

            ast::Operands::Destination(_, d) => {
                *source == out::db::OperandEncoding::None
                    && self.matches_operand_encoding(d, destination, size_hint)?
            }

            ast::Operands::DestinationAndSource(_, d, s) => {
                self.matches_operand_encoding(d, destination, size_hint.clone())?
                    && self.matches_operand_encoding(s, source, size_hint)?
            }
        })
    }

    fn matches_operand_encoding(
        &self,
        operand: &ast::Operand,
        operand_encoding: &out::db::OperandEncoding,
        size_hint: Option<ast::DataSize>,
    ) -> Result<bool, CompileError> {
        Ok(match operand_encoding {
            out::db::OperandEncoding::None => false,

            out::db::OperandEncoding::Imm => {
                if let Some(size_hint) = size_hint {
                    match size_hint {
                        ast::DataSize::Byte => matches!(operand, ast::Operand::Immediate(_, _)),
                        ast::DataSize::Word => matches!(operand, ast::Operand::Immediate(_, _)),
                    }
                } else {
                    panic!(
                        "Size could not be determined {:?} {:?} {:?}",
                        operand, operand_encoding, size_hint
                    );
                }
            }

            out::db::OperandEncoding::Imm8 => match operand {
                ast::Operand::Immediate(_, expr) => {
                    let value = self.evaluate_expression(expr)?;
                    value >= 0 && value <= u8::MAX as i32
                }

                _ => false,
            },

            out::db::OperandEncoding::Imm16 => match operand {
                ast::Operand::Immediate(_, expr) => {
                    let value = self.evaluate_expression(expr)?;
                    value >= 0 && value <= u16::MAX as i32
                }

                _ => false,
            },

            out::db::OperandEncoding::Sbw => {
                // TODO: Check the actual value here.
                false
            }

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
                        ast::DataSize::Byte => {
                            matches!(
                                operand,
                                ast::Operand::Address(..) | ast::Operand::Indirect(..)
                            )
                        }
                        ast::DataSize::Word => {
                            matches!(
                                operand,
                                ast::Operand::Address(..) | ast::Operand::Indirect(..)
                            )
                        }
                    }
                } else {
                    panic!("no size hint specified")
                }
            }

            out::db::OperandEncoding::Mem8 => {
                matches!(
                    operand,
                    ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _)
                        | ast::Operand::Indirect(_, _, _, Some(ast::DataSize::Byte), _)
                )
            }

            out::db::OperandEncoding::Mem16 => {
                matches!(
                    operand,
                    ast::Operand::Address(_, _, Some(ast::DataSize::Word), _)
                        | ast::Operand::Indirect(_, _, _, Some(ast::DataSize::Word), _)
                )
            }

            out::db::OperandEncoding::RegMem8 => {
                matches!(operand, ast::Operand::Register(_, ast::Register::Byte(_)))
                    || matches!(
                        operand,
                        ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _)
                            | ast::Operand::Indirect(_, _, _, Some(ast::DataSize::Byte), _)
                    )
            }

            out::db::OperandEncoding::RegMem16 => {
                matches!(operand, ast::Operand::Register(_, ast::Register::Word(_)))
                    || matches!(
                        operand,
                        ast::Operand::Address(_, _, Some(ast::DataSize::Word), _)
                            | ast::Operand::Indirect(_, _, _, Some(ast::DataSize::Word), _)
                    )
            }

            out::db::OperandEncoding::SegEs => {
                matches!(operand, ast::Operand::Segment(_, ast::Segment::ES))
            }
            out::db::OperandEncoding::SegCs => {
                matches!(operand, ast::Operand::Segment(_, ast::Segment::CS))
            }
            out::db::OperandEncoding::SegSs => {
                matches!(operand, ast::Operand::Segment(_, ast::Segment::SS))
            }
            out::db::OperandEncoding::SegDs => {
                matches!(operand, ast::Operand::Segment(_, ast::Segment::DS))
            }

            out::db::OperandEncoding::SegOff => false, // TODO

            _ => {
                todo!("operand encoding: {:?}", operand_encoding);
            }
        })
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

impl Compiler {
    pub fn push_line(&mut self, line: ast::Line) {
        self.outputs.push(Output {
            line,
            size: 0,
            unresolved_references: false,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut compiler = Compiler::default();
        compiler.push_line(ast::Line::Label(ast::Label(0..0, "test".to_owned())));
        assert_eq!(Vec::<u8>::new(), compiler.compile().unwrap());
    }

    #[test]
    fn basic_2() {
        let mut compiler = Compiler::default();
        compiler.push_line(ast::Line::Instruction(ast::Instruction {
            span: 0..0,
            operation: out::Operation::MOV,
            operands: ast::Operands::DestinationAndSource(
                0..0,
                ast::Operand::Address(
                    0..0,
                    ast::Expression::Value(0..0, ast::Value::Constant(0)),
                    None,
                    None,
                ),
                ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Al)),
            ),
        }));
        assert_eq!(vec![0], compiler.compile().unwrap());
    }
}
