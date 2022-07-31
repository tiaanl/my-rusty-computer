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

        // self._debug_print_outputs();

        let mut result = vec![];

        let mut offset = 0;
        for output in &self.outputs {
            if matches!(
                output.line,
                ast::Line::Instruction(..) | ast::Line::Data(..)
            ) {
                // println!("line: {}", output.line);
                for b in self.encode_line(&output.line, output.times, offset, output.size)? {
                    result.push(b);
                }
            }
            offset += output.size;
        }

        Ok(result)
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
        println!(
            "setting \"{}\" to {:?}",
            label,
            if let Some(offset) = offset {
                format!("{:#04x}", offset)
            } else {
                "NONE".to_string()
            }
        );

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
        size: u16,
    ) -> Result<Vec<u8>, CompileError> {
        match line {
            ast::Line::Instruction(instruction) => {
                self.encode_instruction(instruction, offset, size)
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

    fn encode_instruction(
        &self,
        instruction: &ast::Instruction,
        offset: u16,
        size: u16,
    ) -> Result<Vec<u8>, CompileError> {
        let template = self.find_instruction_template(instruction)?;

        println!("{instruction:?}");
        println!("{template:}");

        let mut result = vec![];

        use out::template::codes::*;

        match &instruction.operands {
            ast::Operands::Destination(
                _,
                ast::Operand::Address(_, _, _, segment)
                | ast::Operand::Indirect(_, _, _, _, segment),
            ) => self.emit_segment_override(&mut result, segment),

            ast::Operands::DestinationAndSource(
                _,
                ast::Operand::Address(_, _, _, segment)
                | ast::Operand::Indirect(_, _, _, _, segment),
                _,
            ) => self.emit_segment_override(&mut result, segment),

            ast::Operands::DestinationAndSource(
                _,
                _,
                ast::Operand::Address(_, _, _, segment)
                | ast::Operand::Indirect(_, _, _, _, segment),
            ) => self.emit_segment_override(&mut result, segment),

            // No segment override if we don't have some kind of address.
            _ => {}
        }

        let mut it = template.codes.iter();
        while let Some(&code) = it.next() {
            match code {
                C_BYTE => {
                    let byte = *it.next().unwrap();
                    result.push(byte);
                }

                C_REG_BASE => {
                    let base = (*it.next().unwrap()) as u8;

                    let reg = match &instruction.operands {
                        ast::Operands::DestinationAndSource(
                            _,
                            ast::Operand::Register(_, register),
                            _,
                        ) => register.encoding(),

                        ast::Operands::Destination(_, ast::Operand::Register(_, register)) => {
                            register.encoding()
                        }

                        _ => unreachable!("C_REG_BASE {:?}", instruction),
                    };

                    result.push((base + reg) as Code);
                }

                C_MOD_RM => {
                    let reg = *it.next().unwrap() as u8;
                    match &instruction.operands {
                        ast::Operands::Destination(_, ast::Operand::Register(_, src)) => {
                            self.emit_mrrm_register(&mut result, reg, src.encoding());
                        }

                        ast::Operands::Destination(
                            _,
                            ast::Operand::Address(_, expr, _, seg_override),
                        ) => {
                            self.emit_mrrm_address(
                                &mut result,
                                reg,
                                offset,
                                size,
                                expr,
                                seg_override.is_some(),
                            )?;
                        }

                        ast::Operands::Destination(
                            _,
                            ast::Operand::Indirect(_, encoding, expr, _, _),
                        ) => {
                            self.emit_mrrm_indirect(&mut result, reg, encoding, expr)?;
                        }

                        ast::Operands::DestinationAndSource(
                            _,
                            ast::Operand::Register(_, other),
                            _,
                        )
                        | ast::Operands::DestinationAndSource(
                            _,
                            _,
                            ast::Operand::Register(_, other),
                        ) => {
                            self.emit_mrrm_register(&mut result, reg, other.encoding());
                        }

                        ast::Operands::DestinationAndSource(
                            _,
                            ast::Operand::Address(_, expr, _, seg_override),
                            _,
                        )
                        | ast::Operands::DestinationAndSource(
                            _,
                            _,
                            ast::Operand::Address(_, expr, _, seg_override),
                        ) => {
                            self.emit_mrrm_address(
                                &mut result,
                                reg,
                                offset,
                                size,
                                expr,
                                seg_override.is_some(),
                            )?;
                        }

                        ast::Operands::DestinationAndSource(
                            _,
                            ast::Operand::Indirect(_, encoding, expr, _, _),
                            _,
                        )
                        | ast::Operands::DestinationAndSource(
                            _,
                            _,
                            ast::Operand::Indirect(_, encoding, expr, _, _),
                        ) => {
                            self.emit_mrrm_indirect(&mut result, reg, encoding, expr)?;
                        }

                        _ => unreachable!("C_MOD_RM {:?}", instruction),
                    }
                }

                C_MOD_REG_RM => match &instruction.operands {
                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, dst),
                        ast::Operand::Register(_, src),
                    ) => {
                        self.emit_mrrm_register(&mut result, dst.encoding(), src.encoding());
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, reg),
                        ast::Operand::Segment(_, seg),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Segment(_, seg),
                        ast::Operand::Register(_, reg),
                    ) => {
                        self.emit_mrrm_register(&mut result, seg.encoding(), reg.encoding());
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg_override),
                        ast::Operand::Register(_, register),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, register),
                        ast::Operand::Address(_, expr, _, seg_override),
                    ) => {
                        self.emit_mrrm_address(
                            &mut result,
                            register.encoding(),
                            offset,
                            size,
                            expr,
                            seg_override.is_some(),
                        )?;
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg_override),
                        ast::Operand::Segment(_, segment),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Segment(_, segment),
                        ast::Operand::Address(_, expr, _, seg_override),
                    ) => {
                        self.emit_mrrm_address(
                            &mut result,
                            segment.encoding(),
                            offset,
                            size,
                            expr,
                            seg_override.is_some(),
                        )?;
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Indirect(_, encoding, expr, _, _),
                        ast::Operand::Register(_, register),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, register),
                        ast::Operand::Indirect(_, encoding, expr, _, _),
                    ) => {
                        self.emit_mrrm_indirect(&mut result, register.encoding(), encoding, expr)?;
                    }

                    _ => unreachable!("C_MOD_REG_RM {:?}", instruction),
                },

                C_IMM_BYTE => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        self.emit_immediate_byte(&mut result, expr)?;
                    }

                    ast::Operands::DestinationAndSource(_, ast::Operand::Immediate(_, expr), _) => {
                        self.emit_immediate_byte(&mut result, expr)?;
                    }

                    ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr)) => {
                        self.emit_immediate_byte(&mut result, expr)?;
                    }

                    _ => unreachable!("C_IMM_BYTE {:?}", instruction),
                },

                C_IMM_WORD => match &instruction.operands {
                    ast::Operands::DestinationAndSource(_, ast::Operand::Immediate(_, expr), _)
                    | ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr))
                    | ast::Operands::Destination(_, ast::Operand::Immediate(_, expr))
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, _),
                        _,
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        _,
                        ast::Operand::Address(_, expr, _, _),
                    ) => {
                        self.emit_immediate_word(&mut result, expr)?;
                    }

                    _ => unreachable!("C_IMM_WORD {:?}", instruction),
                },

                C_IMM_SIGN_BYTE => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        self.emit_immediate_signed_byte(&mut result, expr)?;
                    }

                    ast::Operands::DestinationAndSource(_, ast::Operand::Immediate(_, expr), _) => {
                        self.emit_immediate_signed_byte(&mut result, expr)?;
                    }

                    ast::Operands::DestinationAndSource(_, _, ast::Operand::Immediate(_, expr)) => {
                        self.emit_immediate_signed_byte(&mut result, expr)?;
                    }

                    _ => unreachable!("C_IMM_BYTE_SIGN {:?}", instruction),
                },

                C_DISP_BYTE => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        // The value stored in the expression is the address of the instruction
                        // we want to go to, so calculate the displacement.
                        let value = self.evaluate_expression(expr)?;
                        let value = value - (offset as i32 + size as i32);

                        self.emit_immediate_signed_byte(
                            &mut result,
                            &ast::Expression::Value(
                                expr.span().clone(),
                                ast::Value::Constant(value),
                            ),
                        )?;
                    }

                    _ => unreachable!("C_DISP_BYTE {:?}", instruction),
                },

                C_DISP_WORD => match &instruction.operands {
                    ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) => {
                        let value = self.evaluate_expression(expr)? - offset as i32 - size as i32;

                        self.emit_immediate_word(
                            &mut result,
                            &ast::Expression::Value(
                                expr.span().clone(),
                                ast::Value::Constant(value),
                            ),
                        )?;
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg_override),
                        _,
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        _,
                        ast::Operand::Address(_, expr, _, seg_override),
                    ) => {
                        let value = self.evaluate_expression(expr)?;
                        let value = if seg_override.is_none() {
                            value - (offset as i32 + size as i32)
                        } else {
                            value - size as i32
                        };

                        self.emit_immediate_word(
                            &mut result,
                            &ast::Expression::Value(
                                expr.span().clone(),
                                ast::Value::Constant(value),
                            ),
                        )?;
                    }

                    _ => unreachable!("C_DISP_WORD {:?}", instruction),
                },

                C_SEG_OFF => todo!(),

                _ => unreachable!(),
            }
        }

        Ok(result)
    }

    fn emit_mrrm(&self, out: &mut Vec<u8>, mode: u8, reg: u8, reg_mem: u8) {
        let mrrm = (mode << 6) + (reg << 3) + reg_mem;
        out.push(mrrm);
    }

    fn emit_segment_override(&self, out: &mut Vec<u8>, segment: &Option<ast::Segment>) {
        // ES = 26 = 100110
        // CS = 2E = 101110
        // SS = 36 = 110110
        // DS = 3E = 111110

        match segment {
            None => {}
            Some(ast::Segment::ES) => out.push(0b100110),
            Some(ast::Segment::SS) => out.push(0b110110),
            Some(ast::Segment::CS) => out.push(0b101110),
            Some(ast::Segment::DS) => out.push(0b111110),
        }
    }

    fn emit_immediate_byte(
        &self,
        out: &mut Vec<u8>,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        let value = self.evaluate_expression(expr)?;

        let emit_value = if value < 0 {
            if value < i8::MIN as i32 || value > i8::MAX as i32 {
                return Err(CompileError::ImmediateValueOutOfRange(
                    expr.span().clone(),
                    value,
                ));
            }
            value as i8 as u8
        } else {
            if value > u8::MAX as i32 {
                return Err(CompileError::ImmediateValueOutOfRange(
                    expr.span().clone(),
                    value,
                ));
            }
            value as u8
        };

        out.push(emit_value);

        Ok(())
    }

    fn emit_immediate_signed_byte(
        &self,
        out: &mut Vec<u8>,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        self.emit_immediate_byte(out, expr)
    }

    fn emit_immediate_word(
        &self,
        out: &mut Vec<u8>,
        expr: &ast::Expression,
    ) -> Result<(), CompileError> {
        let value = self.evaluate_expression(expr)?;

        let emit_value = if value < 0 {
            if value < i16::MIN as i32 || value > i16::MAX as i32 {
                return Err(CompileError::ImmediateValueOutOfRange(
                    expr.span().clone(),
                    value,
                ));
            }
            value as i16 as u16
        } else {
            if value > u16::MAX as i32 {
                return Err(CompileError::ImmediateValueOutOfRange(
                    expr.span().clone(),
                    value,
                ));
            }
            value as u16
        };

        for b in emit_value.to_le_bytes() {
            out.push(b);
        }

        Ok(())
    }

    fn emit_mrrm_register(&self, out: &mut Vec<u8>, reg: u8, reg_mem: u8) {
        self.emit_mrrm(out, 0b11, reg, reg_mem);
    }

    fn emit_mrrm_address(
        &self,
        out: &mut Vec<u8>,
        reg: u8,
        offset: u16,
        size: u16,
        expr: &ast::Expression,
        has_segment: bool,
    ) -> Result<(), CompileError> {
        self.emit_mrrm(out, 0b00, reg, 0b110);

        let value = self.evaluate_expression(expr)?;
        let value = if !has_segment {
            println!("abs");
            value - offset as i32 - size as i32
        } else {
            println!("rel");
            value + size as i32
        };

        self.emit_immediate_word(
            out,
            &ast::Expression::Value(expr.span().clone(), ast::Value::Constant(value)),
        )?;

        Ok(())
    }

    fn emit_mrrm_indirect(
        &self,
        out: &mut Vec<u8>,
        reg: u8,
        indirect_encoding: &ast::IndirectEncoding,
        expr: &Option<ast::Expression>,
    ) -> Result<(), CompileError> {
        if let Some(expr) = expr {
            let value = self.evaluate_expression(expr)?;

            if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
                self.emit_mrrm(out, 0b01, reg, indirect_encoding.encoding());
                self.emit_immediate_byte(
                    out,
                    &ast::Expression::Value(expr.span().clone(), ast::Value::Constant(value)),
                )?;
            } else {
                self.emit_mrrm(out, 0b10, reg, indirect_encoding.encoding());
                self.emit_immediate_word(
                    out,
                    &ast::Expression::Value(expr.span().clone(), ast::Value::Constant(value)),
                )?;
            }
            Ok(())
        } else {
            self.emit_mrrm(out, 0b00, reg, indirect_encoding.encoding());
            Ok(())
        }
    }

    fn _compile_instruction(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<out::Instruction, CompileError> {
        Ok(match &instruction.operands {
            ast::Operands::None(_) => out::Instruction {
                operation: instruction.operation,
                operands: out::OperandSet::None,
                repeat: None,
            },

            ast::Operands::Destination(_, dst) => out::Instruction {
                operation: instruction.operation,
                operands: out::OperandSet::Destination(self._compile_operand(dst, None)?),
                repeat: None,
            },

            ast::Operands::DestinationAndSource(_, dst, src) => {
                let size_hint = match (dst.data_size(), src.data_size()) {
                    (Some(d), Some(s)) if d == s => Some(d),
                    (Some(d), None) => Some(d),
                    (None, Some(s)) => Some(s),
                    _ => unreachable!(),
                };

                out::Instruction {
                    operation: instruction.operation,
                    operands: out::OperandSet::DestinationAndSource(
                        self._compile_operand(dst, size_hint)?,
                        self._compile_operand(src, size_hint)?,
                    ),
                    repeat: None,
                }
            }
        })
    }

    fn _compile_operand(
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
                let size_in_bytes = self.calculate_instruction_size(instruction)?;

                size_in_bytes as u16
            }

            ast::Line::Data(_, data) => data.len() as u16,

            _ => todo!(),
        })
    }

    fn calculate_instruction_size(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<u16, CompileError> {
        use out::template::codes::*;

        let template = self.find_instruction_template(instruction)?;

        let mut total_size = 0;
        let mut i = template.codes.iter();
        while let Some(code) = i.next() {
            match *code {
                C_BYTE => {
                    total_size += 1;
                    let _ = i.next();
                }

                C_REG_BASE => {
                    total_size += 1;
                    let _ = i.next();
                }

                C_MOD_RM => {
                    let _encoding = i.next().unwrap();
                    match &instruction.operands {
                        ast::Operands::Destination(_, dst) => {
                            // Use a dummy source, because it is just an encoding.
                            total_size += self.calculate_mrrm_size(
                                dst,
                                &ast::Operand::Register(
                                    0..0,
                                    ast::Register::Word(ast::WordRegister::Ax),
                                ),
                            )?;
                        }

                        ast::Operands::DestinationAndSource(_, dst, src) => {
                            total_size += self.calculate_mrrm_size(dst, src)?;
                        }

                        _ => unreachable!("Invalid operands for mrrm {:?}", &instruction.operands),
                    }
                }

                C_MOD_REG_RM => {
                    if let ast::Operands::DestinationAndSource(_, dst, src) = &instruction.operands
                    {
                        total_size += self.calculate_mrrm_size(dst, src)?;
                    } else {
                        unreachable!("Invalid operands for mrrm {:?}", &instruction.operands)
                    };
                }

                C_IMM_BYTE => total_size += 1,

                C_IMM_WORD => total_size += 2,

                C_IMM_SIGN_BYTE => total_size += 1,

                C_DISP_BYTE => total_size += 1,

                C_DISP_WORD => total_size += 2,

                C_SEG_OFF => total_size += 4,

                _ => unreachable!(),
            }
        }

        // println!("{} == {}", template, total_size);

        Ok(total_size)
    }

    fn calculate_mrrm_size(
        &self,
        dst: &ast::Operand,
        src: &ast::Operand,
    ) -> Result<u16, CompileError> {
        fn segment_size(seg: &Option<ast::Segment>) -> u16 {
            match seg {
                None | Some(ast::Segment::DS) => 0,
                _ => 1,
            }
        }

        Ok(match (dst, src) {
            (
                ast::Operand::Register(_, _),
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
            )
            | (ast::Operand::Segment(_, _), ast::Operand::Register(_, _)) => 1,

            (
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
                ast::Operand::Address(_, _, _, segment),
            )
            | (
                ast::Operand::Address(_, _, _, segment),
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
            ) => 3 + segment_size(segment),

            (
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
                ast::Operand::Indirect(_, _, expr, _, segment),
            )
            | (
                ast::Operand::Indirect(_, _, expr, _, segment),
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
            ) => self.indirect_mrrm_size(expr)? + segment_size(segment),

            (
                ast::Operand::Register(_, _) | ast::Operand::Segment(_, _),
                ast::Operand::Immediate(_, _),
            ) => {
                // mrrm + direct address (immediate gets calculated by another code.
                3
            }

            (ast::Operand::Address(_, _, _, segment), ast::Operand::Immediate(_, _)) => {
                // mrrm + direct address (immediate gets calculated by another code.
                3 + segment_size(segment)
            }

            (ast::Operand::Indirect(_, _, expr, _, segment), ast::Operand::Immediate(_, _)) => {
                self.indirect_mrrm_size(expr)? + segment_size(segment)
            }

            (d, s) => todo!("{:?}, {:?}", d, s),
        })
    }

    fn indirect_mrrm_size(&self, expr: &Option<ast::Expression>) -> Result<u16, CompileError> {
        Ok(if let Some(expr) = expr {
            let value = self.evaluate_expression(expr)?;
            if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
                // Byte displacement.
                2
            } else {
                // Word displacement.
                3
            }
        } else {
            // No displacement.
            1
        })
    }
}

impl Compiler {
    fn find_instruction_template(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<&out::template::Template, CompileError> {
        use out::template::type_flags::*;

        let operands = match &instruction.operands {
            ast::Operands::None(_) => (T_NONE, T_NONE),
            ast::Operands::Destination(_, dst) => (self.operand_to_type_flags(dst)?, T_NONE),
            ast::Operands::DestinationAndSource(_, dst, src) => (
                self.operand_to_type_flags(dst)?,
                self.operand_to_type_flags(src)?,
            ),
        };

        match out::template::find(instruction.operation, operands.0, operands.1) {
            Some(template) => Ok(template),
            None => Err(CompileError::InvalidOperands(
                instruction.operands.span().clone(),
                instruction.clone(),
                vec![],
            )),
        }
    }

    fn operand_to_type_flags(
        &self,
        operand: &ast::Operand,
    ) -> Result<out::template::type_flags::TypeFlags, CompileError> {
        use out::template::type_flags::*;
        Ok(match operand {
            ast::Operand::Immediate(_, expr) => {
                let value = self.evaluate_expression(expr)?;
                if value == 1 {
                    T_IMM | T_ONE
                } else if value < 0 {
                    T_IMM | T_SIGNEX
                } else {
                    T_IMM
                }
            }

            ast::Operand::Register(_, ast::Register::Byte(register)) => {
                T_REG
                    | T_BITS_8
                    | match register {
                        ast::ByteRegister::Al => T_REG_AL_AX,
                        ast::ByteRegister::Cl => T_REG_CL_CX,
                        ast::ByteRegister::Dl => T_REG_DL_DX,
                        _ => 0,
                    }
            }

            ast::Operand::Register(_, ast::Register::Word(register)) => {
                T_REG
                    | T_BITS_16
                    | match register {
                        ast::WordRegister::Ax => T_REG_AL_AX,
                        ast::WordRegister::Cx => T_REG_CL_CX,
                        ast::WordRegister::Dx => T_REG_DL_DX,
                        _ => 0,
                    }
            }

            ast::Operand::Address(_, _, data_size, _) => {
                T_MEM_DIR
                    | match data_size {
                        None => 0,
                        Some(ast::DataSize::Byte) => T_BITS_8,
                        Some(ast::DataSize::Word) => T_BITS_16,
                    }
            }

            ast::Operand::Indirect(_, _, _, data_size, _) => {
                T_MEM_IND
                    | match data_size {
                        None => 0,
                        Some(ast::DataSize::Byte) => T_BITS_8,
                        Some(ast::DataSize::Word) => T_BITS_16,
                    }
            }

            ast::Operand::Segment(_, segment) => match segment {
                ast::Segment::ES => T_SEG | T_SEG_ES | T_BITS_16,
                ast::Segment::CS => T_SEG | T_SEG_CS | T_BITS_16,
                ast::Segment::SS => T_SEG | T_SEG_SS | T_BITS_16,
                ast::Segment::DS => T_SEG | T_SEG_DS | T_BITS_16,
            },
        })
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
                    Ok(Some(line)) => compiler.push_line(line),
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
