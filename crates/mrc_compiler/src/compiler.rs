use crate::ast;
use mrc_instruction as out;
use std::collections::{HashMap, LinkedList};
use std::fmt::Formatter;

impl ast::Expression {
    // fn get_constant_value(&self) -> Result<i32, CompileError> {
    //     match self {
    //         ast::Expression::Value(_, ast::Value::Constant(value)) => Ok(*value),
    //         _ => Err(CompileError::ConstantValueContainsVariables(
    //             self.span().clone(),
    //         )),
    //     }
    // }

    // fn replace_label(&mut self, label: &ast::Label, value: i32) {
    //     match self {
    //         ast::Expression::Value(span, ast::Value::Label(l)) if l.1 == label.1 => {
    //             *self = ast::Expression::Value(span.clone(), ast::Value::Constant(value as i32));
    //         }
    //         ast::Expression::PrefixOperator(_, _, expr) => {
    //             expr.replace_label(label, value);
    //         }
    //         ast::Expression::InfixOperator(_, _, left, right) => {
    //             left.replace_label(label, value);
    //             right.replace_label(label, value);
    //         }
    //         _ => todo!(),
    //     }
    // }
}

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

#[derive(Default)]
pub struct Compiler {
    outputs: Vec<Output>,
    labels: HashMap<String, u16>,
    constants: HashMap<String, i32>,
}

// struct ForwardReference {
//     line_num: usize,
//     target_line_num: usize,
//     label: ast::Label,
// }

impl Compiler {
    pub fn compile(&mut self) -> Result<Vec<out::Instruction>, CompileError> {
        let mut last_offset = u16::MAX;

        let mut labels = LinkedList::new();

        loop {
            let mut unresolved_references = 0;
            let mut offset = 0;

            let outputs = unsafe {
                &mut *std::ptr::slice_from_raw_parts_mut(
                    self.outputs.as_mut_ptr(),
                    self.outputs.len(),
                )
            };

            for output in outputs {
                macro_rules! drain_labels {
                    () => {{
                        while let Some(label) = labels.pop_back() {
                            if let Some(label_offset) = self.labels.get_mut(label.1.as_str()) {
                                *label_offset = offset;
                            } else {
                                self.labels.insert(label.1.clone(), offset);
                            }
                        }
                    }};
                }

                macro_rules! line_size {
                    ($line:expr) => {{
                        match self.calculate_line_size($line) {
                            Ok(size) => {
                                output.unresolved_references = false;
                                size
                            }
                            Err(CompileError::LabelNotFound(_)) => {
                                unresolved_references += 1;
                                output.unresolved_references = true;
                                0
                            }
                            Err(err) => return Err(err),
                        }
                    }};
                }

                // Optimization that does not recalculate data for lines that were calculated
                // before.
                if output.size > 0 && !output.unresolved_references {
                    continue;
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

            // If there are no more unresolved references, then we can stop.
            if unresolved_references == 0 {
                break;
            }

            // Now we know there are unsolved references, but if we did not get new sizes for any
            // of them, then we are done.
            if offset == last_offset {
                break;
            }

            last_offset = offset;
        }

        // if unresolved_references > 0 {
        //     println!("WARNING: unresolved references!");
        // }

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

        // let forward_references = self.calculate_size_of_each_instruction()?;
        //
        // {
        //     let outputs = unsafe {
        //         &mut *std::ptr::slice_from_raw_parts_mut(
        //             self.outputs.as_mut_ptr(),
        //             self.outputs.len(),
        //         )
        //     };
        //     for output in outputs {
        //         println!("    {}, {}", &output.size, &output.line);
        //     }
        // }
        //
        // if !forward_references.is_empty() {
        //     self.resolve_forward_references(forward_references)?;
        // }

        Ok(vec![])
    }

    // /// Go through each output and compile instructions.  Returns a list of instructions that
    // /// has references to other memory locations.
    // fn calculate_size_of_each_instruction(
    //     &mut self,
    // ) -> Result<LinkedList<ForwardReference>, CompileError> {
    //     let outputs = unsafe {
    //         &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len())
    //     };
    //
    //     let mut forward_references = LinkedList::new();
    //
    //     // Compile each line into a mrc_instruction::Instruction.
    //
    //     println!("First pass - calculate instruction sizes if possible:");
    //
    //     for (num, output) in outputs.iter_mut().enumerate() {
    //         match &mut output.line {
    //             ast::Line::Instruction(instruction) => {
    //                 match self.size_in_bytes(instruction) {
    //                     Ok(size_in_bytes) => {
    //                         output.size = size_in_bytes;
    //                     }
    //
    //                     Err(err) => match err {
    //                         CompileError::ConstantValueContainsLabel(label) => {
    //                             // forward_references.push_back((num, label.clone()));
    //                             forward_references.push_back(ForwardReference {
    //                                 line_num: num,
    //                                 target_line_num: *self.labels.get(&label.1).unwrap(),
    //                                 label: label.clone(),
    //                             })
    //                         }
    //                         err => return Err(err),
    //                     },
    //                 };
    //             }
    //
    //             ast::Line::Data(_, data) => {
    //                 output.size = data.len() as u16;
    //             }
    //
    //             _ => {}
    //         }
    //     }
    //
    //     Ok(forward_references)
    // }

    // fn resolve_forward_references(
    //     &mut self,
    //     forward_references: LinkedList<ForwardReference>,
    // ) -> Result<(), CompileError> {
    //     debug_assert!(!forward_references.is_empty());
    //
    //     // Take mutable ownership of the forward references in this scope.
    //     let mut forward_references = forward_references;
    //
    //     println!(
    //         "Second pass - resolve {} forward references:",
    //         forward_references.len()
    //     );
    //
    //     // TODO: if we sort the references by the difference in line numbers starting with smallest
    //     //       we will do potentially less work.
    //
    //     let outputs = unsafe {
    //         &mut *std::ptr::slice_from_raw_parts_mut(self.outputs.as_mut_ptr(), self.outputs.len())
    //     };
    //
    //     let mut new_forward_references = LinkedList::new();
    //     loop {
    //         let mut removed = 0;
    //
    //         while let Some(reference) = forward_references.pop_front() {
    //             println!("    Resolving reference: {}", &reference.label);
    //             // println!("resolving label: {}", &reference.label);
    //             // println!("num: {num}, label_num: {label_num}");
    //
    //             let line_num = reference.line_num;
    //             let target_line_num = reference.target_line_num;
    //
    //             let diff = if line_num + 1 == target_line_num {
    //                 Some(0)
    //             } else {
    //                 let mut inner_diff = 0_i32;
    //
    //                 if line_num < target_line_num {
    //                     // Going forward we start from the next output.
    //                     for output in &mut outputs[line_num + 1..target_line_num] {
    //                         // println!("size: {i}: {}", &outputs[i].size_in_bytes);
    //                         if output.size != 0 {
    //                             inner_diff += output.size as i32;
    //                         } else {
    //                             println!("        down instruction with no size: {}", output.line);
    //                             inner_diff = 0;
    //                             break;
    //                         }
    //                     }
    //                 } else {
    //                     for output in &mut outputs[target_line_num..line_num] {
    //                         // println!("size: {i}: {}", &outputs[i].size_in_bytes);
    //                         if output.size != 0 {
    //                             inner_diff -= output.size as i32;
    //                         } else {
    //                             println!("        up instruction with no size: {}", output.line);
    //                             inner_diff = 0;
    //                             break;
    //                         }
    //                     }
    //                 }
    //
    //                 if inner_diff == 0 {
    //                     println!("            nothing found");
    //                     None
    //                 } else {
    //                     println!("            found diff: {inner_diff}");
    //                     Some(inner_diff)
    //                 }
    //             };
    //
    //             if let Some(diff) = diff {
    //                 // println!("diff: {diff}");
    //                 let output = &mut outputs[line_num];
    //
    //                 if let ast::Line::Instruction(instruction) = &mut output.line {
    //                     instruction.operands.replace_label(&reference.label, diff);
    //
    //                     match self.size_in_bytes(instruction) {
    //                         Ok(size_in_bytes) => {
    //                             output.size = size_in_bytes;
    //                             removed += 1;
    //                         }
    //
    //                         Err(_) => {
    //                             // If we still can't figure out the size of the instruction
    //                             // after replacing a label, then just add it to the list of
    //                             // references again so we can try another time.
    //                             new_forward_references.push_back(reference);
    //                         }
    //                     }
    //                 }
    //             } else {
    //                 // println!("not found");
    //                 new_forward_references.push_back(reference);
    //             }
    //         }
    //
    //         if removed == 0 {
    //             // println!(
    //             //     "Some references could not be resolved: {:?}",
    //             //     new_forward_references
    //             // );
    //
    //             return Err(CompileError::UnresolvedReference(
    //                 new_forward_references.pop_front().unwrap().label,
    //             ));
    //         }
    //
    //         if new_forward_references.is_empty() {
    //             break;
    //         }
    //
    //         // println!("forward references: {}", new_forward_references.len());
    //
    //         std::mem::swap(&mut forward_references, &mut new_forward_references);
    //     }
    //
    //     Ok(())
    // }
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
                } else if let Some(label_offset) = self.labels.get(label.1.as_str()) {
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

    // fn _calc_mod_reg_rm_size(
    //     &self,
    //     destination: &ast::Operand,
    //     source: &ast::Operand,
    // ) -> Result<u16, CompileError> {
    //     macro_rules! address_size {
    //         ($segment:expr) => {{
    //             let mut total = 1; // mod reg r/m byte.
    //             total += 2; // direct address word.
    //
    //             // 1 byte if there is a segment override.
    //             if let Some(segment) = $segment {
    //                 if *segment != ast::Segment::DS {
    //                     total += 1;
    //                 }
    //             }
    //
    //             total
    //         }};
    //     }
    //
    //     macro_rules! indirect_size {
    //         ($expr:expr, $segment:expr) => {{
    //             let mut total = 1; // mod reg r/m byte.
    //
    //             if let Some(expr) = $expr {
    //                 let mut expr = expr.clone();
    //                 self.evaluate_expression(&mut expr)?;
    //                 let value = expr.get_constant_value()?;
    //                 if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
    //                     total += 1;
    //                 } else {
    //                     total += 2;
    //                 }
    //             }
    //
    //             // 1 byte if there is a segment override.
    //             if let Some(segment) = $segment {
    //                 if *segment != ast::Segment::DS {
    //                     total += 1;
    //                 }
    //             }
    //
    //             total
    //         }};
    //     }
    //
    //     macro_rules! immediate_size {
    //         ($expr:expr) => {{
    //             // FIXME: This needs to be calculated according to a passed in hint.
    //             1
    //         }};
    //     }
    //
    //     match (destination, source) {
    //         // Register
    //         //
    //         (ast::Operand::Register(_, _), ast::Operand::Register(_, _))
    //         | (ast::Operand::Segment(_, _), ast::Operand::Register(_, _)) => Ok(1),
    //
    //         (ast::Operand::Register(_, _), ast::Operand::Immediate(_, imm_expr)) => {
    //             Ok(immediate_size!(imm_expr))
    //         }
    //
    //         (ast::Operand::Address(_, _, _, segment), ast::Operand::Register(_, _))
    //         | (ast::Operand::Address(_, _, _, segment), ast::Operand::Segment(_, _))
    //         | (ast::Operand::Register(_, _), ast::Operand::Address(_, _, _, segment)) => {
    //             Ok(address_size!(segment))
    //         }
    //
    //         (ast::Operand::Indirect(_, _, expr, _, segment), ast::Operand::Register(_, _))
    //         | (ast::Operand::Indirect(_, _, expr, _, segment), ast::Operand::Segment(_, _))
    //         | (ast::Operand::Register(_, _), ast::Operand::Indirect(_, _, expr, _, segment)) => {
    //             Ok(indirect_size!(expr, segment))
    //         }
    //
    //         // Immediate
    //         //
    //         (ast::Operand::Address(_, _, _, segment), ast::Operand::Immediate(_, imm_expr))
    //         | (ast::Operand::Immediate(_, imm_expr), ast::Operand::Address(_, _, _, segment)) => {
    //             Ok(address_size!(segment) + immediate_size!(imm_expr))
    //         }
    //
    //         (
    //             ast::Operand::Indirect(_, _, expr, _, segment),
    //             ast::Operand::Immediate(_, imm_expr),
    //         )
    //         | (
    //             ast::Operand::Immediate(_, imm_expr),
    //             ast::Operand::Indirect(_, _, expr, _, segment),
    //         ) => Ok(indirect_size!(expr, segment) + immediate_size!(imm_expr)),
    //
    //         _ => todo!("{:?}, {:?}", destination, source),
    //     }
    // }
}

// impl ast::Operand {
//     fn replace_label(&mut self, label: &ast::Label, value: i32) {
//         match self {
//             ast::Operand::Immediate(_, expr) | ast::Operand::Address(_, expr, _, _) => {
//                 expr.replace_label(label, value);
//             }
//             _ => {}
//         }
//     }
// }

// impl ast::Operands {
//     fn replace_label(&mut self, label: &ast::Label, value: i32) {
//         match self {
//             ast::Operands::None(_) => {}
//             ast::Operands::Destination(_, dst) => dst.replace_label(label, value),
//             ast::Operands::DestinationAndSource(_, dst, src) => {
//                 dst.replace_label(label, value);
//                 src.replace_label(label, value);
//             }
//         }
//     }
// }

impl Compiler {
    // fn compile_instruction(
    //     &self,
    //     instruction: &mut ast::Instruction,
    // ) -> Result<out::Instruction, CompileError> {
    //     let id = self.find_instruction_data(instruction)?;
    //
    //     Ok(match &mut instruction.operands {
    //         ast::Operands::None(_) => {
    //             out::Instruction::new(instruction.operation, out::OperandSet::None)
    //         }
    //         ast::Operands::Destination(_, destination) => out::Instruction::new(
    //             instruction.operation,
    //             out::OperandSet::Destination(self.compile_operand(destination, &id.destination)?),
    //         ),
    //         ast::Operands::DestinationAndSource(_, destination, source) => out::Instruction::new(
    //             instruction.operation,
    //             out::OperandSet::DestinationAndSource(
    //                 self.compile_operand(destination, &id.destination)?,
    //                 self.compile_operand(source, &id.source)?,
    //             ),
    //         ),
    //     })
    // }
    //
    // fn compile_operand(
    //     &self,
    //     input: &mut ast::Operand,
    //     hint: &out::db::OperandEncoding,
    // ) -> Result<out::Operand, CompileError> {
    //     Ok(match input {
    //         ast::Operand::Immediate(_, expr) => {
    //             self.evaluate_expression(expr)?;
    //
    //             let value = expr.get_constant_value()?;
    //
    //             match hint {
    //                 out::db::OperandEncoding::Imm => {
    //                     if value <= u8::MAX as i32 {
    //                         out::Operand::Immediate(out::Immediate::Byte(value as u8))
    //                     } else if value <= u16::MAX as i32 {
    //                         out::Operand::Immediate(out::Immediate::Word(value as u16))
    //                     } else {
    //                         return Err(CompileError::ImmediateValueOutOfRange(
    //                             input.span().clone(),
    //                             value,
    //                         ));
    //                     }
    //                 }
    //
    //                 out::db::OperandEncoding::Imm8 => {
    //                     if value <= u8::MAX as i32 {
    //                         out::Operand::Immediate(out::Immediate::Byte(value as u8))
    //                     } else {
    //                         return Err(CompileError::ImmediateValueOutOfRange(
    //                             input.span().clone(),
    //                             value,
    //                         ));
    //                     }
    //                 }
    //
    //                 out::db::OperandEncoding::Imm16 => {
    //                     if value <= u16::MAX as i32 {
    //                         out::Operand::Immediate(out::Immediate::Word(value as u16))
    //                     } else {
    //                         return Err(CompileError::ImmediateValueOutOfRange(
    //                             input.span().clone(),
    //                             value,
    //                         ));
    //                     }
    //                 }
    //
    //                 out::db::OperandEncoding::Disp8 => {
    //                     if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
    //                         out::Operand::Displacement(out::Displacement::Byte(value as i8))
    //                     } else {
    //                         return Err(CompileError::ImmediateValueOutOfRange(
    //                             input.span().clone(),
    //                             value,
    //                         ));
    //                     }
    //                 }
    //
    //                 out::db::OperandEncoding::Disp16 => {
    //                     if value >= i16::MIN as i32 && value <= i16::MAX as i32 {
    //                         out::Operand::Displacement(out::Displacement::Word(value as i16))
    //                     } else {
    //                         return Err(CompileError::ImmediateValueOutOfRange(
    //                             input.span().clone(),
    //                             value,
    //                         ));
    //                     }
    //                 }
    //
    //                 _ => todo!("encode immediate to operand with hint: {:?}", hint),
    //             }
    //         }
    //
    //         ast::Operand::Address(_, expr, _data_size, segment) => {
    //             self.evaluate_expression(expr)?;
    //             let value = expr.get_constant_value()?;
    //             let segment = segment.clone().unwrap_or(ast::Segment::DS);
    //             let segment = out::Segment::try_from_encoding(segment.encoding()).unwrap();
    //             out::Operand::Direct(segment, value as u16, out::OperandSize::Byte)
    //         }
    //
    //         ast::Operand::Indirect(_, indirect_encoding, expr, _data_size, segment) => {
    //             let value = if let Some(expr) = expr {
    //                 self.evaluate_expression(expr)?;
    //                 Some(expr.get_constant_value()?)
    //             } else {
    //                 None
    //             };
    //             let segment = segment.clone().unwrap_or(ast::Segment::DS);
    //             let segment = out::Segment::try_from_encoding(segment.encoding()).unwrap();
    //             out::Operand::Indirect(
    //                 segment,
    //                 AddressingMode::try_from_encoding(indirect_encoding.encoding()).unwrap(),
    //                 if let Some(value) = value {
    //                     if value >= i8::MIN as i32 && value <= i8::MAX as i32 {
    //                         out::Displacement::Byte(value as i8)
    //                     } else {
    //                         out::Displacement::Word(value as i16)
    //                     }
    //                 } else {
    //                     out::Displacement::None
    //                 },
    //                 out::OperandSize::Byte,
    //             )
    //         }
    //
    //         ast::Operand::Register(_, register) => match register {
    //             ast::Register::Byte(r) => out::Operand::Register(out::SizedRegisterEncoding(
    //                 out::RegisterEncoding::try_from_encoding(r.encoding())
    //                     .map_err(|_| todo!("Error for invalid encoding"))?,
    //                 out::OperandSize::Byte,
    //             )),
    //             ast::Register::Word(r) => out::Operand::Register(out::SizedRegisterEncoding(
    //                 out::RegisterEncoding::try_from_encoding(r.encoding())
    //                     .map_err(|_| todo!("Error for invalid encoding"))?,
    //                 out::OperandSize::Word,
    //             )),
    //         },
    //         ast::Operand::Segment(_, seg) => {
    //             out::Operand::Segment(out::Segment::try_from_encoding(seg.encoding()).unwrap())
    //         }
    //     })
    // }

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
                    println!("no size hint specified");
                    panic!()
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
    pub fn consume(&mut self, line: ast::Line) {
        self.outputs.push(Output {
            line,
            size: 0,
            unresolved_references: false,
        });
    }

    // fn consume(&mut self, mut line: ast::Line) -> Result<(), Self::Err> {
    //     macro_rules! drain_labels {
    //         () => {{
    //             for label in &self.current_labels {
    //                 self.labels.insert(label.clone(), self.outputs.len());
    //             }
    //             self.current_labels.clear();
    //         }};
    //     }
    //
    //     match &mut line {
    //         ast::Line::Label(label) => {
    //             self.current_labels.push(label.1.clone());
    //         }
    //
    //         ast::Line::Instruction(_) | ast::Line::Data(_, _) => {
    //             drain_labels!();
    //
    //             self.outputs.push(Output {
    //                 line,
    //                 size_in_bytes: 0,
    //                 unresolved_references: false,
    //             });
    //         }
    //
    //         ast::Line::Times(_, expr, line) => {
    //             drain_labels!();
    //
    //             let mut expr = expr.clone();
    //             self.evaluate_expression(&mut expr)?;
    //             let times = expr.get_constant_value()?;
    //
    //             for _ in 0..times {
    //                 self.outputs.push(Output {
    //                     line: line.as_ref().clone(),
    //                     size_in_bytes: 0,
    //                     unresolved_references: false,
    //                 });
    //             }
    //         }
    //
    //         ast::Line::Constant(_, expr) => {
    //             if let Some(label) = self.current_labels.pop() {
    //                 self.evaluate_expression(expr)?;
    //
    //                 match expr.get_constant_value() {
    //                     Ok(value) => {
    //                         self.constants.insert(label.clone(), value);
    //                     }
    //                     Err(_) => {
    //                         return Err(CompileError::ConstantValueContainsVariables(
    //                             line.span().clone(),
    //                         ))
    //                     }
    //                 }
    //             } else {
    //                 return Err(CompileError::ConstantWithoutLabel(line.span().clone()));
    //             }
    //         }
    //     }
    //
    //     Ok(())
    // }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut compiler = Compiler::default();
        compiler.consume(ast::Line::Label(ast::Label(0..0, "test".to_owned())));
        println!("{:?}", compiler.compile().unwrap());
    }

    #[test]
    fn basic_2() {
        let mut compiler = Compiler::default();
        compiler.consume(ast::Line::Instruction(ast::Instruction {
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
        println!("{:?}", compiler.compile().unwrap());
    }
}
