use crate::ast;
use crate::encoder as enc;
use crate::encoder::{encode, value_is_signed_word, OperandData};
use mrc_instruction as out;
use std::collections::{HashMap, LinkedList};
use std::fmt::Formatter;

const START_OFFSET: u16 = 0x100;

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
    DataSizeNotSpecified(ast::Span),
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
            | CompileError::UnresolvedReference(ast::Label(span, _))
            | CompileError::DataSizeNotSpecified(span) => span,
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

            CompileError::DataSizeNotSpecified(_) => {
                write!(f, "Data size not specified.")
            }
        }
    }
}

#[derive(Debug)]
pub struct Output {
    line: ast::Line,
    size: u16,
    times: u16,
    unresolved_references: bool,
}

#[derive(Debug)]
pub struct LabelInfo {
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

        self._debug_print_outputs();

        let mut result = vec![];

        let mut offset = START_OFFSET;
        for output in &self.outputs {
            if matches!(
                output.line,
                ast::Line::Instruction(..) | ast::Line::Data(..)
            ) {
                // println!("line: {}", output.line);

                let bytes = self.encode_line(&output.line, output.times, offset)?;

                // At this point, an instruction with 0 size is invalid.
                assert_ne!(output.size, 0, "Line size is zero: {:?}", &output.line);

                // Does the number of bytes we emitted and the size of the instruction we calculated
                // earlier match?
                assert_eq!(
                    output.size as usize,
                    bytes.len(),
                    "Calculated size and assembled sizes does not match."
                );

                for b in bytes {
                    result.push(b);
                }
            }
            offset += output.size;
        }

        Ok(result)
    }

    fn _debug_print_outputs(&self) {
        let mut offset = START_OFFSET;
        for output in &self.outputs {
            if matches!(&output.line, ast::Line::Label(..) | ast::Line::Constant(..)) {
                continue;
            }

            println!(
                "{:04X} {:04X} {} {}{}",
                offset,
                output.size,
                if output.unresolved_references {
                    "x"
                } else {
                    " "
                },
                if output.times > 1 {
                    format!("TIMES {} ", output.times)
                } else {
                    "".to_owned()
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
                match &mut output.line {
                    ast::Line::Label(label) => {
                        labels.push_back(label.clone());
                    }

                    line @ ast::Line::Instruction(..) => {
                        drain_labels!();
                        let mut size = 0;
                        for _ in 0..output.times {
                            size += match self.calculate_line_size(line, offset) {
                                Ok(size) => {
                                    output.unresolved_references = false;
                                    size
                                }
                                Err(CompileError::LabelNotFound(label)) => {
                                    self.set_label_offset(&label, None);
                                    unresolved_references += 1;
                                    output.unresolved_references = true;
                                    0
                                }
                                Err(err) => return Err(err),
                            };
                        }
                        output.size = size;
                        offset += size;
                    }

                    ast::Line::Data(_, data) => {
                        drain_labels!();
                        let mut size = 0;
                        for _ in 0..output.times {
                            size += data.len() as u16;
                        }
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

                    ast::Line::Times(..) => {
                        // We convert ::Times lines to normal instruction lines with a times value,
                        // so encountering this should not be possible.
                        unreachable!()
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
        // if let Some(offset) = offset {
        //     println!("setting \"{}\" to {} ({:#04x})", label, offset, offset);
        // } else {
        //     println!("clearing label \"{}\"", label);
        // }

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

    fn encode_line(
        &self,
        line: &ast::Line,
        times: u16,
        offset: u16,
    ) -> Result<Vec<u8>, CompileError> {
        match line {
            ast::Line::Instruction(instruction) => {
                let mut result = vec![];
                for _ in 0..times {
                    let data = self.encode_instruction(instruction, offset)?;

                    for b in &data {
                        result.push(*b)
                    }
                }

                Ok(result)
            }

            ast::Line::Data(_, data) => {
                let mut result = vec![];

                for _ in 0..times {
                    for b in data {
                        result.push(*b)
                    }
                }

                Ok(result)
            }

            // ast::Line::Label(..) => Ok(vec![]),
            _ => todo!("{:?}", line),
        }
    }

    fn build_operand_data(&self, operand: &ast::Operand) -> Result<OperandData, CompileError> {
        Ok(match operand {
            ast::Operand::Immediate(span, expr) => {
                let value = self.evaluate_expression(expr)?;
                OperandData::immediate(span.clone(), value)
            }

            ast::Operand::Register(span, reg) => OperandData::register(
                span.clone(),
                reg.encoding(),
                match reg {
                    ast::Register::Byte(_) => enc::OperandSize::Byte,
                    ast::Register::Word(_) => enc::OperandSize::Word,
                },
            ),

            ast::Operand::Segment(span, seg) => OperandData::segment(span.clone(), seg.encoding()),

            ast::Operand::Direct(span, expr, data_size, seg) => OperandData::direct(
                span.clone(),
                self.evaluate_expression(expr)?,
                data_size,
                seg,
            ),

            ast::Operand::Indirect(span, indirect_encoding, expr, data_size, seg) => {
                let value = if let Some(expr) = expr {
                    self.evaluate_expression(expr)?
                } else {
                    0
                };
                if !value_is_signed_word(value) {
                    return Err(CompileError::ImmediateValueOutOfRange(span.clone(), value));
                }

                OperandData::indirect(
                    span.clone(),
                    indirect_encoding.encoding(),
                    value as i16,
                    data_size,
                    seg,
                )
            }

            todo => todo!("{:?}", todo),
        })
    }

    fn build_instruction_data(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<crate::encoder::InstructionData, CompileError> {
        Ok(match &instruction.operands {
            ast::Operands::None(span) => {
                crate::encoder::InstructionData::none(span.clone(), instruction.operation)
            }

            ast::Operands::Destination(span, dst) => crate::encoder::InstructionData::dst(
                span.clone(),
                instruction.operation,
                self.build_operand_data(dst)?,
            ),

            ast::Operands::DestinationAndSource(span, dst, src) => {
                crate::encoder::InstructionData::dst_and_src(
                    span.clone(),
                    instruction.operation,
                    self.build_operand_data(dst)?,
                    self.build_operand_data(src)?,
                )
            }
        })
    }

    fn encode_instruction(
        &self,
        instruction: &ast::Instruction,
        offset: u16,
    ) -> Result<Vec<u8>, CompileError> {
        let instruction_data = self.build_instruction_data(instruction)?;
        let mut bytes: Vec<u8> = vec![];
        encode(&instruction_data, offset, &mut bytes).unwrap();
        Ok(bytes)
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
    fn calculate_line_size(&self, line: &ast::Line, offset: u16) -> Result<u16, CompileError> {
        Ok(match line {
            ast::Line::Instruction(instruction) => {
                let size_in_bytes = self.calculate_instruction_size(instruction, offset)?;

                size_in_bytes as u16
            }

            ast::Line::Data(_, data) => data.len() as u16,

            _ => unreachable!(),
        })
    }

    fn calculate_instruction_size(
        &self,
        instruction: &ast::Instruction,
        offset: u16,
    ) -> Result<u8, CompileError> {
        let insn_data = self.build_instruction_data(instruction)?;
        let mut size_in_bytes = 0_u8;

        if let Err(err) = encode(&insn_data, offset, &mut size_in_bytes) {
            panic!("Encode error: {:?} {}", err, instruction);
        }

        Ok(size_in_bytes)
    }
}

impl Compiler {
    pub fn push_line(&mut self, line: ast::Line) -> Result<(), CompileError> {
        match line {
            ast::Line::Times(_, expr, line) => {
                let times = self.evaluate_expression(&expr)? as u16;
                self.outputs.push(Output {
                    line: *line,
                    size: 0,
                    times,
                    unresolved_references: false,
                })
            }

            _ => {
                self.outputs.push(Output {
                    line,
                    size: 0,
                    times: 1,
                    unresolved_references: false,
                });
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::Diagnostics;
    use crate::Parser;

    macro_rules! compile_test {
        ($source:literal, $binary:literal) => {{
            use std::path::Path;
            let source = include_str!($source);
            let expected = include_bytes!($binary);

            // let path = std::fs::canonicalize($source.to_owned())
            //     .unwrap()
            //     .into_os_string()
            //     .into_string()
            //     .unwrap();
            let path = Path::new(env!("PWD"))
                .join(file!())
                .parent()
                .unwrap()
                .join($source)
                .canonicalize()
                .unwrap()
                .into_os_string()
                .into_string()
                .unwrap();

            // let path = "mem".to_string();

            let mut diag = Diagnostics::new(source, path);

            let mut compiler = Compiler::default();
            let mut parser = Parser::new(source);

            loop {
                match parser.parse_line() {
                    Ok(Some(line)) => compiler.push_line(line).unwrap(),
                    Ok(None) => break,
                    Err(err) => {
                        diag.error(&err, err.span().clone());
                        diag.print(&mut std::io::stderr())
                            .expect("Could not write to stderr.");
                        panic!()
                    }
                }
            }

            match compiler.compile() {
                Ok(actual) => {
                    assert_eq!(expected, actual.as_slice());
                }
                Err(err) => {
                    diag.error(&err, err.span().clone());
                    diag.print(&mut std::io::stderr())
                        .expect("Could not write to stderr.");
                    panic!()
                }
            }
        }};
    }

    #[test]
    fn compile() {
        compile_test!("../tests/calljmp.asm", "../tests/calljmp.bin");
        compile_test!("../tests/ea.asm", "../tests/ea.bin");
        compile_test!("../tests/each.asm", "../tests/each.bin");
        compile_test!("../tests/group1.asm", "../tests/group1.bin");
        compile_test!("../tests/imul.asm", "../tests/imul.bin");
        compile_test!("../tests/incdec.asm", "../tests/incdec.bin");
    }
}
