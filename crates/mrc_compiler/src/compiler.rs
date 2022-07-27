use crate::ast;
use mrc_decoder::TryFromEncoding;
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
    times: u16,
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
    pub fn compile(&mut self) -> Result<Vec<out::Instruction>, CompileError> {
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

        // self.debug_print_outputs();

        let mut instructions = vec![];

        for output in &self.outputs {
            if let ast::Line::Instruction(instruction) = &output.line {
                let id = self.find_instruction_data(instruction)?;
                let size_hint = instruction_data_size_hint(id);
                instructions.push(self.compile_instruction(instruction, size_hint)?);
            }
        }

        Ok(instructions)

        // let mut result = vec![];
        //
        // let mut offset = 0;
        // for output in &self.outputs {
        //     if matches!(
        //         output.line,
        //         ast::Line::Instruction(..) | ast::Line::Data(..)
        //     ) {
        //         println!("line: {}", output.line);
        //         for b in self.encode_line(&output.line, output.times, output.size, offset)? {
        //             result.push(b);
        //         }
        //     }
        //     offset += output.size;
        // }
        //
        // Ok(result)
    }

    fn _debug_print_outputs(&self) {
        let mut offset = 0;
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
                        let mut size = 0;
                        for _ in 0..output.times {
                            size += line_size!(line);
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
        // println!("setting \"{}\" to {:?}", label, offset);

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

    fn _encode_line(
        &self,
        line: &ast::Line,
        times: u16,
        size: u16,
        offset: u16,
    ) -> Result<Vec<u8>, CompileError> {
        match line {
            ast::Line::Instruction(instruction) => {
                self._encode_instruction(instruction, size, offset)
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

            ast::Line::Label(..) => Ok(vec![]),

            _ => todo!("{:?}", line),
        }
    }

    fn _encode_instruction(
        &self,
        instruction: &ast::Instruction,
        size: u16,
        offset: u16,
    ) -> Result<Vec<u8>, CompileError> {
        let id = self.find_instruction_data(instruction)?;

        macro_rules! displacement_to {
            ($ts:ty, $tu:ty, $expr:expr, $offset:expr, $size:expr) => {{
                let target_offset = self.evaluate_expression($expr)?;
                let displacement = target_offset as i32 - ($offset as i32 + $size as i32);
                assert!(displacement >= <$ts>::MIN as i32 && displacement <= <$ts>::MAX as i32);
                displacement as $ts as $tu
            }};
        }

        let mut result = vec![];

        for code in id.codes {
            match code {
                out::db::Code::Byte(byte) => result.push(*byte),

                out::db::Code::Imm8 => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u8).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u8).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    ast::Operands::DestinationAndSource(_, ast::Operand::Immediate(_, expr), _) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u8).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    _ => todo!("{:?}", &instruction.operands),
                },

                out::db::Code::Imm16 => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u16).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u16).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    _ => todo!("{:?}", &instruction.operands),
                },

                out::db::Code::Disp8 => {
                    if let ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) =
                        &instruction.operands
                    {
                        let displacement = displacement_to!(i8, u8, expr, offset, size);
                        for b in (displacement as i8 as u8).to_le_bytes() {
                            result.push(b);
                        }
                    }
                }

                out::db::Code::Disp16 => {
                    if let ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) =
                        &instruction.operands
                    {
                        let displacement = displacement_to!(i16, u16, expr, offset, size);
                        for b in (displacement as i16 as u16).to_le_bytes() {
                            result.push(b);
                        }
                    }
                }

                out::db::Code::PlusReg(start) => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Register(_, register)) => {
                        result.push(start + register.encoding());
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, register),
                        _,
                    ) => {
                        result.push(start + register.encoding());
                    }

                    _ => todo!("{:?}", &instruction.operands),
                },

                out::db::Code::ModRegRM => match &instruction.operands {
                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, _),
                        ast::Operand::Register(_, register),
                    ) => self._emit_mrrm_address(&mut result, register.encoding(), expr)?,

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, _),
                        ast::Operand::Segment(_, segment),
                    ) => self._emit_mrrm_address(&mut result, segment.encoding(), expr)?,

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, register),
                        ast::Operand::Address(_, expr, _, _),
                    ) => {
                        self._emit_mrrm_address(&mut result, register.encoding(), expr)?;
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, dst),
                        ast::Operand::Register(_, src),
                    ) => self._emit_mrrm(&mut result, 0b11, dst.encoding(), src.encoding()),

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Segment(_, dst),
                        ast::Operand::Register(_, src),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, src),
                        ast::Operand::Segment(_, dst),
                    ) => self._emit_mrrm(&mut result, 0b11, dst.encoding(), src.encoding()),

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Indirect(_, indirect_encoding, expr, _, _),
                        ast::Operand::Register(_, reg),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, reg),
                        ast::Operand::Indirect(_, indirect_encoding, expr, _, _),
                    ) => {
                        let r = reg.encoding();

                        if let Some(expr) = expr {
                            let value = self.evaluate_expression(expr)?;
                            if value < i8::MIN as i32 || value > i8::MAX as i32 {
                                self._emit_mrrm(&mut result, 0b01, r, indirect_encoding.encoding());
                                result.push(value as i8 as u8);
                            } else {
                                self._emit_mrrm(&mut result, 0b10, r, indirect_encoding.encoding());
                                for b in (value as i16 as u16).to_le_bytes() {
                                    result.push(b);
                                }
                            }
                        } else {
                            self._emit_mrrm(&mut result, 0b11, r, indirect_encoding.encoding());
                        }
                    }

                    // ast::Operands::DestinationAndSource(_, ast::Operand::Indirect(_, indirect_encoding, expr, _, _), ast::Operand::Immediate(_, imm_expr)) => {
                    //
                    // }
                    _ => todo!("{:?}", &instruction.operands),
                },

                out::db::Code::ModRM(r) => match &instruction.operands {
                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, data_size, _),
                        ast::Operand::Immediate(_, imm),
                    ) => {
                        let mrrm = (0b00 << 6) + (0b110 << 3) + r;
                        result.push(mrrm);

                        let value = self.evaluate_expression(expr)?;
                        for b in (value as u16).to_le_bytes() {
                            result.push(b);
                        }

                        let value = self.evaluate_expression(imm)?;

                        if let Some(data_size) = data_size {
                            match data_size {
                                ast::DataSize::Byte => {
                                    for b in (value as u8).to_le_bytes() {
                                        result.push(b);
                                    }
                                }
                                ast::DataSize::Word => {
                                    for b in (value as u16).to_le_bytes() {
                                        result.push(b);
                                    }
                                }
                            }
                        } else {
                            unreachable!()
                        }
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Indirect(_, indirect_encoding, expr, data_size, _),
                        ast::Operand::Immediate(_, imm),
                    ) => {
                        self._emit_indirect_mrrm(&mut result, *r, indirect_encoding, expr)?;
                        match data_size {
                            Some(ast::DataSize::Byte) => {
                                self._emit_immediate_byte(&mut result, imm)?;
                            }
                            Some(ast::DataSize::Word) => {
                                self._emit_immediate_word(&mut result, imm)?;
                            }
                            None => unreachable!(),
                        }
                    }

                    ast::Operands::Destination(_, ast::Operand::Address(_, expr, _, _)) => {
                        self._emit_mrrm(&mut result, 0b00, *r, 0b110);
                        self._emit_immediate_word(&mut result, expr)?;
                    }

                    ast::Operands::Destination(_, ast::Operand::Register(_, register)) => {
                        self._emit_mrrm(&mut result, 0b11, *r, register.encoding());
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, register),
                        ast::Operand::Immediate(_, expr),
                    ) => {
                        match register {
                            ast::Register::Byte(register) => {
                                self._emit_mrrm(&mut result, 0b11, *r, register.encoding());
                                self._emit_immediate_byte(&mut result, expr)?;
                            }
                            ast::Register::Word(register) => {
                                self._emit_mrrm(&mut result, 0b11, *r, register.encoding());
                                self._emit_immediate_word(&mut result, expr)?;
                            }
                        };
                    }

                    _ => todo!("{:?}", &instruction.operands),
                },

                out::db::Code::SignImm8 => match &instruction.operands {
                    ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)?;
                        for b in (value as i8 as u8 as u16).to_le_bytes() {
                            result.push(b);
                        }
                    }

                    _ => todo!("{:?}", &instruction.operands),
                },

                _ => todo!("{:?}", code),
            }
        }

        Ok(result)
    }

    fn _emit_mrrm(&self, out: &mut Vec<u8>, mode: u8, reg: u8, reg_mem: u8) {
        let mrrm = (mode << 6) + (reg << 3) + reg_mem;
        out.push(mrrm);
    }

    fn _emit_immediate_byte(
        &self,
        out: &mut Vec<u8>,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        let value = self.evaluate_expression(expr)?;
        for b in (value as u8).to_le_bytes() {
            out.push(b);
        }
        Ok(())
    }

    fn _emit_immediate_word(
        &self,
        out: &mut Vec<u8>,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        let value = self.evaluate_expression(expr)?;
        for b in (value as u16).to_le_bytes() {
            out.push(b);
        }
        Ok(())
    }

    fn _emit_mrrm_address(
        &self,
        out: &mut Vec<u8>,
        reg: u8,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        self._emit_mrrm(out, 0b00, reg, 0b110);
        self._emit_immediate_word(out, expr)?;
        Ok(())
    }

    fn _emit_indirect_mrrm(
        &self,
        out: &mut Vec<u8>,
        reg: u8,
        indirect_encoding: &ast::IndirectEncoding,
        expr: &Option<ast::Expression>,
    ) -> Result<(), CompileError> {
        if let Some(expr) = expr {
            let value = self.evaluate_expression(expr)?;
            if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
                self._emit_mrrm(out, 0b01, reg, indirect_encoding.encoding());
                out.push(value as i8 as u8);
            } else {
                self._emit_mrrm(out, 0b10, reg, indirect_encoding.encoding());
                for b in (value as i16 as u16).to_le_bytes() {
                    out.push(b);
                }
            }
            Ok(())
        } else {
            self._emit_mrrm(out, 0b00, reg, indirect_encoding.encoding());
            Ok(())
        }
    }

    fn compile_instruction(
        &self,
        instruction: &ast::Instruction,
        size_hint: Option<ast::DataSize>,
    ) -> Result<out::Instruction, CompileError> {
        Ok(match &instruction.operands {
            ast::Operands::None(_) => out::Instruction {
                operation: instruction.operation,
                operands: out::OperandSet::None,
                repeat: None,
            },

            ast::Operands::Destination(_, dst) => out::Instruction {
                operation: instruction.operation,
                operands: out::OperandSet::Destination(self.compile_operand(dst, size_hint)?),
                repeat: None,
            },

            ast::Operands::DestinationAndSource(_, dst, src) => out::Instruction {
                operation: instruction.operation,
                operands: out::OperandSet::DestinationAndSource(
                    self.compile_operand(dst, size_hint)?,
                    self.compile_operand(src, size_hint)?,
                ),
                repeat: None,
            },
        })
    }

    fn compile_operand(
        &self,
        operand: &ast::Operand,
        size_hint: Option<ast::DataSize>,
    ) -> Result<out::Operand, CompileError> {
        Ok(match operand {
            ast::Operand::Immediate(_, expr) => {
                let value = self.evaluate_expression(expr)?;
                out::Operand::Immediate(out::Immediate::Word(value as u16))
            }

            ast::Operand::Register(_, register) => {
                out::Operand::Register(out::SizedRegisterEncoding(
                    out::RegisterEncoding::try_from_encoding(register.encoding()).unwrap(),
                    if matches!(register, ast::Register::Byte(_)) {
                        out::OperandSize::Byte
                    } else {
                        out::OperandSize::Word
                    },
                ))
            }

            ast::Operand::Segment(_, segment) => {
                out::Operand::Segment(out::Segment::try_from_encoding(segment.encoding()).unwrap())
            }

            ast::Operand::Address(_, expr, data_size, segment) => {
                let segment = if let Some(segment) = segment {
                    out::Segment::try_from_encoding(segment.encoding()).unwrap()
                } else {
                    out::Segment::DS
                };

                let value = self.evaluate_expression(expr)?;

                let data_size = match (data_size, &size_hint) {
                    (Some(data_size), _) => data_size,
                    (None, Some(size_hint)) => size_hint,
                    (None, None) => panic!(),
                };

                let operand_size = if let ast::DataSize::Byte = data_size {
                    out::OperandSize::Byte
                } else {
                    out::OperandSize::Word
                };

                out::Operand::Direct(segment, value as u16, operand_size)
            }

            ast::Operand::Indirect(_, indirect_encoding, expr, data_size, segment) => {
                let segment = if let Some(segment) = segment {
                    out::Segment::try_from_encoding(segment.encoding()).unwrap()
                } else {
                    out::Segment::DS
                };

                let addressing_mode =
                    out::AddressingMode::try_from_encoding(indirect_encoding.encoding()).unwrap();

                let displacement = if let Some(expr) = expr {
                    let value = self.evaluate_expression(expr)?;
                    if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
                        out::Displacement::Byte(value as i8)
                    } else {
                        out::Displacement::Word(value as i16)
                    }
                } else {
                    out::Displacement::None
                };

                let data_size = match (data_size, &size_hint) {
                    (Some(data_size), _) => data_size,
                    (None, Some(size_hint)) => size_hint,
                    (None, None) => panic!(),
                };

                let operand_size = if let ast::DataSize::Byte = data_size {
                    out::OperandSize::Byte
                } else {
                    out::OperandSize::Word
                };

                out::Operand::Indirect(segment, addressing_mode, displacement, operand_size)
            }
        })
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

                out::db::Code::Byte(_) | out::db::Code::Imm8 => total_size += 1,

                out::db::Code::Imm16 => total_size += 2,

                out::db::Code::Disp8 => total_size += 1,

                out::db::Code::Disp16 => total_size += 2,

                out::db::Code::SignImm8 => {
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
                    // panic!("no size hint specified")
                    false
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

            // out::db::OperandEncoding::SegDs => {
            //     matches!(operand, ast::Operand::Segment(_, ast::Segment::DS))
            // }
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
        match line {
            ast::Line::Times(_, expr, line) => {
                let times = self.evaluate_expression(&expr).unwrap() as u16;
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
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Parser;

    // #[test]
    // fn basic() {
    //     let mut compiler = Compiler::default();
    //     compiler.push_line(ast::Line::Label(ast::Label(0..0, "test".to_owned())));
    //     assert_eq!(Vec::<u8>::new(), compiler.compile().unwrap());
    // }

    // #[test]
    // fn basic_2() {
    //     let mut compiler = Compiler::default();
    //     compiler.push_line(ast::Line::Instruction(ast::Instruction {
    //         span: 0..0,
    //         operation: out::Operation::MOV,
    //         operands: ast::Operands::DestinationAndSource(
    //             0..0,
    //             ast::Operand::Address(
    //                 0..0,
    //                 ast::Expression::Value(0..0, ast::Value::Constant(0)),
    //                 None,
    //                 None,
    //             ),
    //             ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Al)),
    //         ),
    //     }));
    //     assert_eq!(vec![0], compiler.compile().unwrap());
    // }

    #[test]
    fn compile() {
        let mut c = Compiler::default();
        let mut p = Parser::new("mov byte [0x100], 10");
        while let Some(line) = p.parse_line().unwrap() {
            c.push_line(line);
        }
        assert_eq!(
            vec![out::Instruction {
                operation: out::Operation::MOV,
                operands: out::OperandSet::DestinationAndSource(
                    out::Operand::Direct(out::Segment::DS, 0x100, out::OperandSize::Byte),
                    out::Operand::Immediate(out::Immediate::Byte(10)),
                ),
                repeat: None
            }],
            c.compile().unwrap()
        );
    }
}
