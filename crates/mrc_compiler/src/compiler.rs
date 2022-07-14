#![allow(dead_code)]

use crate::ast;
use mrc_decoder::TryFromEncoding;
use mrc_instruction as out;
use std::collections::{HashMap, LinkedList};
use std::fmt::Formatter;

impl ast::Expression {
    fn get_constant_value(&self) -> Result<i32, CompileError> {
        match self {
            ast::Expression::Term(_, ast::Value::Constant(value)) => Ok(*value),
            _ => Err(CompileError::ConstantValueContainsVariables(
                self.span().clone(),
            )),
        }
    }

    fn replace_label(&mut self, label: &ast::Label, value: i32) {
        match self {
            ast::Expression::Term(span, ast::Value::Label(l)) if l.1 == label.1 => {
                *self = ast::Expression::Term(span.clone(), ast::Value::Constant(value as i32));
            }
            ast::Expression::PrefixOperator(_, _, expr) => {
                expr.replace_label(label, value);
            }
            ast::Expression::InfixOperator(_, _, left, right) => {
                left.replace_label(label, value);
                right.replace_label(label, value);
            }
            _ => todo!(),
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum CompileError {
    InvalidOperands(
        ast::Span,
        ast::Operands,
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
    size_in_bytes: u16,
}

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, usize>,
    constants: HashMap<String, i32>,

    current_labels: Vec<String>,
}

struct ForwardReference {
    line_num: usize,
    target_line_num: usize,
    label: ast::Label,
}

impl Compiler {
    pub fn compile(&mut self) -> Result<Vec<out::Instruction>, CompileError> {
        let forward_references = self.calculate_size_of_each_instruction()?;

        {
            let outputs = unsafe {
                &mut *std::ptr::slice_from_raw_parts_mut(
                    self.outputs.as_mut_ptr(),
                    self.outputs.len(),
                )
            };
            for output in outputs {
                println!("    {}, {}", &output.size_in_bytes, &output.line);
            }
        }

        if !forward_references.is_empty() {
            self.resolve_forward_references(forward_references)?;
        }

        Ok(vec![])
    }

    /// Go through each output and compile instructions.  Returns a list of instructions that
    /// has references to other memory locations.
    fn calculate_size_of_each_instruction(
        &mut self,
    ) -> Result<LinkedList<ForwardReference>, CompileError> {
        let outputs = unsafe {
            &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len())
        };

        let mut forward_references = LinkedList::new();

        // Compile each line into a mrc_instruction::Instruction.

        println!("First pass - calculate instruction sizes if possible:");

        for (num, output) in outputs.iter_mut().enumerate() {
            match &mut output.line.content {
                ast::LineContent::Instruction(instruction) => {
                    match self.size_in_bytes(instruction) {
                        Ok(size_in_bytes) => {
                            output.size_in_bytes = size_in_bytes;
                        }

                        Err(err) => match err {
                            CompileError::ConstantValueContainsLabel(label) => {
                                // forward_references.push_back((num, label.clone()));
                                forward_references.push_back(ForwardReference {
                                    line_num: num,
                                    target_line_num: *self.labels.get(&label.1).unwrap(),
                                    label: label.clone(),
                                })
                            }
                            err => return Err(err),
                        },
                    };
                }

                ast::LineContent::Data(_, data) => {
                    output.size_in_bytes = data.len() as u16;
                }

                _ => {}
            }
        }

        Ok(forward_references)
    }

    fn resolve_forward_references(
        &mut self,
        forward_references: LinkedList<ForwardReference>,
    ) -> Result<(), CompileError> {
        debug_assert!(!forward_references.is_empty());

        // Take mutable ownership of the forward references in this scope.
        let mut forward_references = forward_references;

        println!(
            "Second pass - resolve {} forward references:",
            forward_references.len()
        );

        // TODO: if we sort the references by the difference in line numbers starting with smallest
        //       we will do potentially less work.

        let outputs = unsafe {
            &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len())
        };

        let mut new_forward_references = LinkedList::new();
        loop {
            let mut removed = 0;

            while let Some(reference) = forward_references.pop_front() {
                println!("    Resolving reference: {}", &reference.label);
                // println!("resolving label: {}", &reference.label);
                // println!("num: {num}, label_num: {label_num}");

                let line_num = reference.line_num;
                let target_line_num = reference.target_line_num;

                let diff = if line_num + 1 == target_line_num {
                    Some(0)
                } else {
                    let mut inner_diff = 0_i32;

                    if line_num < target_line_num {
                        // Going forward we start from the next output.
                        for output in &mut outputs[line_num + 1..target_line_num] {
                            // println!("size: {i}: {}", &outputs[i].size_in_bytes);
                            if output.size_in_bytes != 0 {
                                inner_diff += output.size_in_bytes as i32;
                            } else {
                                println!("down instruction with no size: {}", output.line);
                                inner_diff = 0;
                                break;
                            }
                        }
                    } else {
                        for output in &mut outputs[target_line_num..line_num] {
                            // println!("size: {i}: {}", &outputs[i].size_in_bytes);
                            if output.size_in_bytes != 0 {
                                inner_diff -= output.size_in_bytes as i32;
                            } else {
                                println!("up instruction with no size: {}", output.line);
                                inner_diff = 0;
                                break;
                            }
                        }
                    }

                    if inner_diff == 0 {
                        // println!("nothing found");
                        None
                    } else {
                        // println!("found diff: {inner_diff}");
                        Some(inner_diff)
                    }
                };

                if let Some(diff) = diff {
                    // println!("diff: {diff}");
                    let output = &mut outputs[line_num];

                    if let ast::LineContent::Instruction(instruction) = &mut output.line.content {
                        instruction.operands.replace_label(&reference.label, diff);

                        match self.size_in_bytes(instruction) {
                            Ok(size_in_bytes) => {
                                output.size_in_bytes = size_in_bytes;
                                removed += 1;
                            }

                            Err(_) => {
                                // If we still can't figure out the size of the instruction
                                // after replacing a label, then just add it to the list of
                                // references again so we can try another time.
                                new_forward_references.push_back(reference);
                            }
                        }
                    }
                } else {
                    // println!("not found");
                    new_forward_references.push_back(reference);
                }
            }

            if removed == 0 {
                // println!(
                //     "Some references could not be resolved: {:?}",
                //     new_forward_references
                // );

                return Err(CompileError::UnresolvedReference(
                    new_forward_references.pop_front().unwrap().label,
                ));
            }

            if new_forward_references.is_empty() {
                break;
            }

            println!("forward references: {}", new_forward_references.len());

            std::mem::swap(&mut forward_references, &mut new_forward_references);
        }

        Ok(())
    }

    fn get_address_for_label(&self, label: &ast::Label) -> Result<u16, CompileError> {
        let target_line_num = match self.labels.get(label.1.as_str()) {
            Some(num) => *num,
            None => return Err(CompileError::LabelNotFound(label.clone())),
        };

        let mut address = 0;
        for num in 0..target_line_num {
            let size_in_bytes = self.outputs[num].size_in_bytes;
            if size_in_bytes == 0 {
                todo!("size_in_bytes == 0");
            }
            address += size_in_bytes;
        }

        println!("address: {}", address);

        todo!()
    }
}

impl Compiler {
    fn evaluate_expression(&self, expression: &mut ast::Expression) -> Result<(), CompileError> {
        match expression {
            ast::Expression::InfixOperator(span, operator, left, right) => {
                self.evaluate_expression(left)?;
                self.evaluate_expression(right)?;

                if let (
                    ast::Expression::Term(_, ast::Value::Constant(left_value)),
                    ast::Expression::Term(_, ast::Value::Constant(right_value)),
                ) = (left.as_ref(), right.as_ref())
                {
                    *expression = ast::Expression::Term(
                        span.clone(),
                        ast::Value::Constant(operator.evaluate(*left_value, *right_value)),
                    );
                }

                Ok(())
            }

            ast::Expression::Term(_, ast::Value::Label(label)) => {
                if let Some(value) = self.constants.get(label.1.as_str()) {
                    *expression = ast::Expression::Term(0..0, ast::Value::Constant(*value));
                } else if self.labels.get(label.1.as_str()).is_some() {
                    let address = self.get_address_for_label(label)?;
                    println!("address: {}", address);
                    return Err(CompileError::ConstantValueContainsLabel(label.clone()));
                } else {
                    return Err(CompileError::LabelNotFound(label.clone()));
                }

                Ok(())
            }

            ast::Expression::Term(_, ast::Value::Constant(_)) => Ok(()),

            _ => Ok(()),
        }
    }
}

impl Compiler {
    fn size_in_bytes(&mut self, instruction: &mut ast::Instruction) -> Result<u16, CompileError> {
        let id = self.find_instruction_data(instruction)?;

        let mut total_size = 0;

        for code in id.codes {
            match code {
                out::db::Code::ModRegRM => match &mut instruction.operands {
                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, _),
                        ast::Operand::Register(_, _),
                    ) => total_size += 1,

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg),
                        ast::Operand::Segment(_, _),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Segment(_, _),
                        ast::Operand::Address(_, expr, _, seg),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg),
                        ast::Operand::Register(_, _),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, _),
                        ast::Operand::Address(_, expr, _, seg),
                    ) => {
                        // [address], seg
                        if let Some(seg) = seg {
                            if !matches!(seg, ast::Segment::DS) {
                                total_size += 1;
                            }
                        }
                        // if let Some(size) = self.address_size_in_bytes(expr)? {
                        //     total_size += size;
                        // }
                    }

                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Register(_, _),
                        ast::Operand::Segment(_, _),
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Segment(_, _),
                        ast::Operand::Register(_, _),
                    ) => {
                        total_size += 1;
                    }

                    _ => todo!("ModRegRM {:?}", instruction.operands),
                },

                out::db::Code::ModRM(_) => match &mut instruction.operands {
                    ast::Operands::DestinationAndSource(
                        _,
                        ast::Operand::Address(_, expr, _, seg),
                        _,
                    )
                    | ast::Operands::DestinationAndSource(
                        _,
                        _,
                        ast::Operand::Address(_, expr, _, seg),
                    )
                    | ast::Operands::Destination(_, ast::Operand::Address(_, expr, _, seg)) => {
                        if let Some(seg) = seg {
                            if !matches!(seg, ast::Segment::DS) {
                                total_size += 1;
                            }
                        }
                        // if let Some(size) = self.address_size_in_bytes(expr)? {
                        //     total_size += size;
                        // }
                    }

                    ast::Operands::DestinationAndSource(_, ast::Operand::Register(_, _), _)
                    | ast::Operands::DestinationAndSource(_, _, ast::Operand::Register(_, _)) => {
                        total_size += 1;
                    }

                    ast::Operands::Destination(_, ast::Operand::Register(_, _))
                    | ast::Operands::Destination(_, ast::Operand::Segment(_, _)) => {
                        total_size += 1;
                    }

                    _ => todo!("ModRM(?) {:?}", instruction.operands),
                },

                out::db::Code::PlusReg(_) => {
                    total_size += 1;
                }

                out::db::Code::Byte(_) | out::db::Code::ImmByte => {
                    total_size += 1;
                }

                out::db::Code::ImmWord => {
                    total_size += 2;
                }

                out::db::Code::DispByte => match (id.destination, id.source) {
                    (out::db::OperandEncoding::Disp8, out::db::OperandEncoding::None) => {
                        if let ast::Operands::Destination(_, ast::Operand::Immediate(_, expr)) =
                            &mut instruction.operands
                        {
                            match self.evaluate_expression(expr) {
                                s => todo!("{:?}", s),
                            }
                            todo!("{:?}", expr);
                        }
                    }
                    _ => todo!(),
                },

                out::db::Code::DispWord => {
                    todo!("Calculate the value of the operand and see if it is near/short/far.")
                }

                out::db::Code::ImmByteSign => {
                    // TODO: Not sure if this correct!
                    total_size += 2;
                }

                _ => todo!("{:?}", code),
            }
        }

        Ok(total_size)
    }

    fn address_size_in_bytes(
        &mut self,
        expr: &mut ast::Expression,
    ) -> Result<Option<u16>, CompileError> {
        if let Err(err) = self.evaluate_expression(expr) {
            return match err {
                CompileError::ConstantValueContainsLabel(_) => Ok(None),
                _ => Err(err),
            };
        }

        println!("expression: {}", expr);

        todo!()
    }
}

impl ast::Operand {
    fn matches_operand_encoding(
        &self,
        operand_encoding: &out::db::OperandEncoding,
        size_hint: Option<ast::DataSize>,
    ) -> bool {
        match operand_encoding {
            out::db::OperandEncoding::None => false,

            out::db::OperandEncoding::Imm => {
                if let Some(size_hint) = size_hint {
                    match size_hint {
                        ast::DataSize::Byte => matches!(self, ast::Operand::Immediate(_, _)),
                        ast::DataSize::Word => matches!(self, ast::Operand::Immediate(_, _)),
                    }
                } else {
                    panic!(
                        "Size could not be determined {:?} {:?} {:?}",
                        self, operand_encoding, size_hint
                    );
                }
            }

            out::db::OperandEncoding::Imm8 => {
                if let ast::Operand::Immediate(_, expr) = self {
                    let value = match expr.get_constant_value() {
                        Ok(value) => value,
                        Err(_) => return false,
                    };

                    value <= u8::MAX as i32
                } else {
                    false
                }
            }

            out::db::OperandEncoding::Imm16 => {
                if let ast::Operand::Immediate(_, expr) = self {
                    let value = match expr.get_constant_value() {
                        Ok(value) => value,
                        Err(_) => return false,
                    };

                    value <= u16::MAX as i32
                } else {
                    false
                }
            }

            out::db::OperandEncoding::Sbw => {
                // TODO: Check the actual value here.
                false
            }

            out::db::OperandEncoding::RegAl => matches!(
                self,
                ast::Operand::Register(_, ast::Register::Byte(ast::ByteRegister::Al)),
            ),

            out::db::OperandEncoding::RegAx => matches!(
                self,
                ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Ax)),
            ),

            out::db::OperandEncoding::RegCl => matches!(
                self,
                ast::Operand::Register(_, ast::Register::Byte(ast::ByteRegister::Cl)),
            ),

            out::db::OperandEncoding::RegCx => matches!(
                self,
                ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Cx)),
            ),

            out::db::OperandEncoding::RegDx => matches!(
                self,
                ast::Operand::Register(_, ast::Register::Word(ast::WordRegister::Dx)),
            ),

            out::db::OperandEncoding::Reg8 => {
                matches!(self, ast::Operand::Register(_, ast::Register::Byte(_)))
            }

            out::db::OperandEncoding::Reg16 => {
                matches!(self, ast::Operand::Register(_, ast::Register::Word(_)))
            }

            out::db::OperandEncoding::Seg => {
                matches!(self, ast::Operand::Segment(_, _))
            }

            out::db::OperandEncoding::Disp8 => matches!(self, ast::Operand::Immediate(_, _)),

            out::db::OperandEncoding::Disp16 => matches!(self, ast::Operand::Immediate(_, _)),

            out::db::OperandEncoding::Mem => {
                if let Some(size_hint) = size_hint {
                    match size_hint {
                        ast::DataSize::Byte => matches!(self, ast::Operand::Address(_, _, _, _)),
                        ast::DataSize::Word => matches!(self, ast::Operand::Address(_, _, _, _)),
                    }
                } else {
                    println!("no size hint specified");
                    panic!()
                }
            }

            out::db::OperandEncoding::Mem8 => {
                matches!(
                    self,
                    ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _)
                )
            }

            out::db::OperandEncoding::Mem16 => {
                matches!(
                    self,
                    ast::Operand::Address(_, _, Some(ast::DataSize::Word), _)
                )
            }

            out::db::OperandEncoding::RegMem8 => {
                matches!(self, ast::Operand::Register(_, ast::Register::Byte(_)))
                    || matches!(
                        self,
                        ast::Operand::Address(_, _, Some(ast::DataSize::Byte), _)
                    )
            }

            out::db::OperandEncoding::RegMem16 => {
                matches!(self, ast::Operand::Register(_, ast::Register::Word(_)))
                    || matches!(
                        self,
                        ast::Operand::Address(_, _, Some(ast::DataSize::Word), _)
                    )
            }

            out::db::OperandEncoding::SegEs => {
                matches!(self, ast::Operand::Segment(_, ast::Segment::ES))
            }
            out::db::OperandEncoding::SegCs => {
                matches!(self, ast::Operand::Segment(_, ast::Segment::CS))
            }
            out::db::OperandEncoding::SegSs => {
                matches!(self, ast::Operand::Segment(_, ast::Segment::SS))
            }
            out::db::OperandEncoding::SegDs => {
                matches!(self, ast::Operand::Segment(_, ast::Segment::DS))
            }

            out::db::OperandEncoding::SegOff => false, // TODO

            _ => {
                todo!("operand encoding: {:?}", operand_encoding);
            }
        }
    }

    fn replace_label(&mut self, label: &ast::Label, value: i32) {
        match self {
            ast::Operand::Immediate(_, expr) | ast::Operand::Address(_, expr, _, _) => {
                expr.replace_label(label, value);
            }
            _ => {}
        }
    }
}

impl ast::Operands {
    fn replace_label(&mut self, label: &ast::Label, value: i32) {
        match self {
            ast::Operands::None(_) => {}
            ast::Operands::Destination(_, dst) => dst.replace_label(label, value),
            ast::Operands::DestinationAndSource(_, dst, src) => {
                dst.replace_label(label, value);
                src.replace_label(label, value);
            }
        }
    }
}

fn operand_set_matches_operand_encodings(
    operand_set: &ast::Operands,
    destination: &out::db::OperandEncoding,
    source: &out::db::OperandEncoding,
    size_hint: Option<ast::DataSize>,
) -> bool {
    match operand_set {
        ast::Operands::None(_) => {
            *destination == out::db::OperandEncoding::None
                && *source == out::db::OperandEncoding::None
        }

        ast::Operands::Destination(_, d) => {
            *source == out::db::OperandEncoding::None
                && d.matches_operand_encoding(destination, size_hint)
        }

        ast::Operands::DestinationAndSource(_, d, s) => {
            d.matches_operand_encoding(destination, size_hint.clone())
                && s.matches_operand_encoding(source, size_hint)
        }
    }
}

impl Compiler {
    fn compile_instruction(
        &self,
        instruction: &mut ast::Instruction,
    ) -> Result<out::Instruction, CompileError> {
        let id = self.find_instruction_data(instruction)?;

        Ok(match &mut instruction.operands {
            ast::Operands::None(_) => {
                out::Instruction::new(instruction.operation, out::OperandSet::None)
            }
            ast::Operands::Destination(_, destination) => out::Instruction::new(
                instruction.operation,
                out::OperandSet::Destination(self.compile_operand(destination, &id.destination)?),
            ),
            ast::Operands::DestinationAndSource(_, destination, source) => out::Instruction::new(
                instruction.operation,
                out::OperandSet::DestinationAndSource(
                    self.compile_operand(destination, &id.destination)?,
                    self.compile_operand(source, &id.source)?,
                ),
            ),
        })
    }

    fn compile_operand(
        &self,
        input: &mut ast::Operand,
        hint: &out::db::OperandEncoding,
    ) -> Result<out::Operand, CompileError> {
        Ok(match input {
            ast::Operand::Immediate(_, expr) => {
                self.evaluate_expression(expr)?;

                let value = expr.get_constant_value()?;

                match hint {
                    out::db::OperandEncoding::Imm => {
                        if value <= u8::MAX as i32 {
                            out::Operand::Immediate(out::Immediate::Byte(value as u8))
                        } else if value <= u16::MAX as i32 {
                            out::Operand::Immediate(out::Immediate::Word(value as u16))
                        } else {
                            return Err(CompileError::ImmediateValueOutOfRange(
                                input.span().clone(),
                                value,
                            ));
                        }
                    }

                    out::db::OperandEncoding::Imm8 => {
                        if value <= u8::MAX as i32 {
                            out::Operand::Immediate(out::Immediate::Byte(value as u8))
                        } else {
                            return Err(CompileError::ImmediateValueOutOfRange(
                                input.span().clone(),
                                value,
                            ));
                        }
                    }

                    out::db::OperandEncoding::Imm16 => {
                        if value <= u16::MAX as i32 {
                            out::Operand::Immediate(out::Immediate::Word(value as u16))
                        } else {
                            return Err(CompileError::ImmediateValueOutOfRange(
                                input.span().clone(),
                                value,
                            ));
                        }
                    }

                    out::db::OperandEncoding::Disp8 => {
                        if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
                            out::Operand::Displacement(out::Displacement::Byte(value as i8))
                        } else {
                            return Err(CompileError::ImmediateValueOutOfRange(
                                input.span().clone(),
                                value,
                            ));
                        }
                    }

                    out::db::OperandEncoding::Disp16 => {
                        if value >= i16::MIN as i32 && value <= i16::MAX as i32 {
                            out::Operand::Displacement(out::Displacement::Word(value as i16))
                        } else {
                            return Err(CompileError::ImmediateValueOutOfRange(
                                input.span().clone(),
                                value,
                            ));
                        }
                    }

                    _ => todo!("encode immediate to operand with hint: {:?}", hint),
                }
            }

            ast::Operand::Address(_, expr, _data_size, segment) => {
                self.evaluate_expression(expr)?;
                let value = expr.get_constant_value()?;
                let segment = segment.clone().unwrap_or(ast::Segment::DS);
                let segment = out::Segment::try_from_encoding(segment.encoding()).unwrap();
                out::Operand::Direct(segment, value as u16, out::OperandSize::Byte)
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

    fn find_instruction_data(
        &self,
        instruction: &ast::Instruction,
    ) -> Result<&out::db::InstructionData, CompileError> {
        for instruction_data in out::db::INSTRUCTIONS {
            if instruction.operation != instruction_data.operation {
                continue;
            }

            if operand_set_matches_operand_encodings(
                &instruction.operands,
                &instruction_data.destination,
                &instruction_data.source,
                instruction_data_size_hint(instruction_data),
            ) {
                return Ok(instruction_data);
            }
        }

        let possible: Vec<&out::db::InstructionData> = out::db::INSTRUCTIONS
            .iter()
            .filter(|ie| ie.operation == instruction.operation)
            .collect();

        Err(CompileError::InvalidOperands(
            instruction.operands.span().clone(),
            instruction.operands.clone(),
            possible,
        ))
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

            ast::LineContent::Instruction(_)
            | ast::LineContent::Data(_, _)
            | ast::LineContent::Times(_, _, _) => {
                if let Some(label) = label {
                    self.current_labels.push(label.1.clone());
                }

                for label in &self.current_labels {
                    self.labels.insert(label.clone(), self.outputs.len());
                }

                self.outputs.push(Output {
                    line,
                    size_in_bytes: 0,
                });

                self.current_labels.clear();
            }

            ast::LineContent::Constant(span, expr) => {
                if let Some(label) = label {
                    self.evaluate_expression(expr)?;

                    match expr.get_constant_value() {
                        Ok(value) => {
                            self.constants.insert(label.1.clone(), value);
                        }
                        Err(_) => {
                            return Err(CompileError::ConstantValueContainsVariables(span.clone()))
                        }
                    }
                } else {
                    return Err(CompileError::ConstantWithoutLabel(span.clone()));
                }
            }
        }

        Ok(())
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
                content: ast::LineContent::Instruction(ast::Instruction {
                    span: 0..0,
                    operation: out::Operation::MOV,
                    operands: ast::Operands::DestinationAndSource(
                        0..0,
                        ast::Operand::Address(
                            0..0,
                            ast::Expression::Term(0..0, ast::Value::Constant(0)),
                            None,
                            None,
                        ),
                        ast::Operand::Register(0..0, ast::Register::Byte(ast::ByteRegister::Al)),
                    ),
                }),
            })
            .unwrap();
        println!("{:?}", compiler.compile().unwrap());
    }
}
