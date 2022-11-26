use crate::operations::Operation;
use std::collections::LinkedList;
use std::str::FromStr;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Label(pub Span, pub String);

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.1)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum ByteRegister {
    Al = 0,
    Cl = 1,
    Dl = 2,
    Bl = 3,
    Ah = 4,
    Ch = 5,
    Dh = 6,
    Bh = 7,
}

impl std::fmt::Display for ByteRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteRegister::Al => write!(f, "AL"),
            ByteRegister::Ah => write!(f, "AH"),
            ByteRegister::Cl => write!(f, "CL"),
            ByteRegister::Ch => write!(f, "CH"),
            ByteRegister::Dl => write!(f, "DL"),
            ByteRegister::Dh => write!(f, "DH"),
            ByteRegister::Bl => write!(f, "BL"),
            ByteRegister::Bh => write!(f, "BH"),
        }
    }
}

impl FromStr for ByteRegister {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "al" => Self::Al,
            "ah" => Self::Ah,
            "cl" => Self::Cl,
            "ch" => Self::Ch,
            "dl" => Self::Dl,
            "dh" => Self::Dh,
            "bl" => Self::Bl,
            "bh" => Self::Bh,

            _ => return Err(()),
        })
    }
}

impl ByteRegister {
    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum WordRegister {
    Ax = 0,
    Cx = 1,
    Dx = 2,
    Bx = 3,
    Sp = 4,
    Bp = 5,
    Si = 6,
    Di = 7,
}

impl std::fmt::Display for WordRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WordRegister::Ax => write!(f, "AX"),
            WordRegister::Cx => write!(f, "CX"),
            WordRegister::Dx => write!(f, "DX"),
            WordRegister::Bx => write!(f, "BX"),
            WordRegister::Sp => write!(f, "SP"),
            WordRegister::Bp => write!(f, "BP"),
            WordRegister::Si => write!(f, "SI"),
            WordRegister::Di => write!(f, "DI"),
        }
    }
}

impl FromStr for WordRegister {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "ax" => Self::Ax,
            "cx" => Self::Cx,
            "dx" => Self::Dx,
            "bx" => Self::Bx,
            "sp" => Self::Sp,
            "bp" => Self::Bp,
            "si" => Self::Si,
            "di" => Self::Di,

            _ => return Err(()),
        })
    }
}

impl WordRegister {
    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Register {
    Byte(ByteRegister),
    Word(WordRegister),
}

impl Register {
    pub fn data_size(&self) -> DataSize {
        match self {
            Register::Byte(_) => DataSize::Byte,
            Register::Word(_) => DataSize::Word,
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Byte(byte) => write!(f, "{}", byte),
            Register::Word(word) => write!(f, "{}", word),
        }
    }
}

impl FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(byte_register) = ByteRegister::from_str(s) {
            Ok(Register::Byte(byte_register))
        } else if let Ok(word_register) = WordRegister::from_str(s) {
            Ok(Register::Word(word_register))
        } else {
            Err(())
        }
    }
}

impl Register {
    #[inline]
    pub fn encoding(&self) -> u8 {
        match self {
            Register::Byte(r) => r.encoding(),
            Register::Word(r) => r.encoding(),
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Segment {
    ES = 0,
    CS = 1,
    SS = 2,
    DS = 3,
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Segment::ES => write!(f, "ES"),
            Segment::CS => write!(f, "CS"),
            Segment::SS => write!(f, "SS"),
            Segment::DS => write!(f, "DS"),
        }
    }
}

impl FromStr for Segment {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "es" => Self::ES,
            "cs" => Self::CS,
            "ss" => Self::SS,
            "ds" => Self::DS,
            _ => return Err(()),
        })
    }
}

impl Segment {
    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataSize {
    Byte,
    Word,
}

impl std::fmt::Display for DataSize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataSize::Byte => write!(f, "BYTE"),
            DataSize::Word => write!(f, "WORD"),
        }
    }
}

impl FromStr for DataSize {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s.to_lowercase().as_str() {
            "byte" => Self::Byte,
            "word" => Self::Word,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum IndirectEncoding {
    BxSi = 0,
    BxDi = 1,
    BpSi = 2,
    BpDi = 3,
    Si = 4,
    Di = 5,
    Bp = 6,
    Bx = 7,
}

impl IndirectEncoding {
    #[inline]
    pub fn encoding(&self) -> u8 {
        self.clone() as u8
    }
}

impl std::fmt::Display for IndirectEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IndirectEncoding::BxSi => write!(f, "BX + SI"),
            IndirectEncoding::BxDi => write!(f, "BX + DI"),
            IndirectEncoding::BpSi => write!(f, "BP + SI"),
            IndirectEncoding::BpDi => write!(f, "BP + DI"),
            IndirectEncoding::Si => write!(f, "SI"),
            IndirectEncoding::Di => write!(f, "DI"),
            IndirectEncoding::Bp => write!(f, "BP"),
            IndirectEncoding::Bx => write!(f, "BX"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Constant(i32),
    Label(Label),
}

impl<'a> std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(value) => write!(f, "{:#04X}", *value),
            Value::Label(label) => write!(f, "{}", *label),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Subtract => write!(f, "-"),
            Operator::Multiply => write!(f, "*"),
            Operator::Divide => write!(f, "/"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    PrefixOperator(Span, Operator, Box<Expression>),
    InfixOperator(Span, Operator, Box<Expression>, Box<Expression>),

    Value(Span, Value),
}

impl Expression {
    pub fn span(&self) -> &Span {
        match self {
            Expression::PrefixOperator(span, _, _)
            | Expression::InfixOperator(span, _, _, _)
            | Expression::Value(span, _) => span,
        }
    }

    pub fn is_constant(&self) -> bool {
        matches!(self, Expression::Value(_, Value::Constant(_)))
    }

    pub fn iter_values(&self) -> IterValues<'_> {
        let mut iter = IterValues {
            stack: LinkedList::new(),
        };

        iter.push_all_left(self);

        iter
    }
}

pub struct IterValues<'a> {
    stack: LinkedList<&'a Expression>,
}

impl<'a> IterValues<'a> {
    fn push_all_left(&mut self, start: &'a Expression) {
        let mut current = start;

        loop {
            match current {
                Expression::PrefixOperator(..) => todo!(),
                Expression::InfixOperator(_, _, left, _) => {
                    self.stack.push_back(current);
                    current = left;
                }
                Expression::Value(..) => {
                    self.stack.push_back(current);
                    break;
                }
            }
        }
    }
}

impl<'a> Iterator for IterValues<'a> {
    type Item = &'a Value;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.stack.pop_back() {
            match current {
                Expression::PrefixOperator(_, _, _) => todo!(),
                Expression::InfixOperator(_, _, _, right) => {
                    self.push_all_left(right);
                    self.next()
                }
                Expression::Value(_, value) => Some(value),
            }
        } else {
            None
        }
    }
}

impl<'a> std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::PrefixOperator(_, operator, right) => {
                write!(f, "{} ({})", operator, right)
            }
            Expression::InfixOperator(_, operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Value(_, value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Immediate(Span, Expression),
    Direct(Span, Expression, Option<DataSize>, Option<Segment>),
    Indirect(
        Span,
        IndirectEncoding,
        Option<Expression>,
        Option<DataSize>,
        Option<Segment>,
    ),
    Far(Span, Expression, Expression),
    Register(Span, Register),
    Segment(Span, Segment),
}

impl Operand {
    pub fn span(&self) -> &Span {
        match self {
            Self::Immediate(span, _)
            | Self::Direct(span, _, _, _)
            | Self::Indirect(span, _, _, _, _)
            | Self::Far(span, _, _)
            | Self::Register(span, _)
            | Self::Segment(span, _) => span,
        }
    }

    pub fn data_size(&self) -> Option<DataSize> {
        match self {
            Operand::Immediate(_, _) => None,
            Operand::Direct(_, _, data_size, _) => *data_size,
            Operand::Indirect(_, _, _, data_size, _) => *data_size,
            Operand::Far(_, _, _) => None,
            Operand::Register(_, register) => Some(register.data_size()),
            Operand::Segment(_, _) => Some(DataSize::Word),
        }
    }
}

impl<'a> std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Immediate(_, expr) => expr.fmt(f),

            Operand::Direct(_, expr, data_size, segment) => {
                if let Some(data_size) = data_size {
                    write!(f, "{} ", data_size)?;
                }
                write!(f, "[")?;

                if let Some(segment) = segment {
                    write!(f, "{}:", segment)?;
                }

                expr.fmt(f)?;

                write!(f, "]")
            }

            Operand::Indirect(_, indirect_encoding, expr, data_size, segment) => {
                if let Some(data_size) = data_size {
                    write!(f, "{} ", data_size)?;
                }
                write!(f, "[")?;

                if let Some(segment) = segment {
                    write!(f, "{}:", segment)?;
                }

                indirect_encoding.fmt(f)?;

                if let Some(expr) = expr {
                    " ".fmt(f)?;
                    expr.fmt(f)?;
                }

                write!(f, "]")
            }

            Operand::Far(_, offset, segment) => {
                write!(f, "{}:{}", segment, offset)
            }

            Operand::Register(_, register) => register.fmt(f),

            Operand::Segment(_, segment) => segment.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operands {
    None(Span),
    Destination(Span, Operand),
    DestinationAndSource(Span, Operand, Operand),
}

impl<'a> Operands {
    pub fn span(&self) -> &Span {
        match self {
            Operands::None(span)
            | Operands::Destination(span, _)
            | Operands::DestinationAndSource(span, _, _) => span,
        }
    }
}

impl<'a> std::fmt::Display for Operands {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operands::None(_) => Ok(()),
            Operands::Destination(_, destination) => write!(f, "{}", destination),
            Operands::DestinationAndSource(_, destination, source) => {
                write!(f, "{}, {}", destination, source)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Instruction {
    pub span: Span,
    pub operation: Operation,
    pub operands: Operands,
}

impl<'a> std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Operands::None(_) = self.operands {
            write!(f, "{:?}", self.operation)
        } else {
            write!(f, "{:?} {}", self.operation, self.operands)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Line {
    Label(Label),
    Instruction(Instruction),
    Data(Span, Vec<u8>),
    Constant(Span, Expression),
    Times(Span, Expression, Box<Line>),
}

impl Line {
    pub fn span(&self) -> &Span {
        match self {
            Line::Label(Label(span, _))
            | Line::Instruction(Instruction { span, .. })
            | Line::Data(span, _)
            | Line::Times(span, _, _)
            | Line::Constant(span, _) => span,
        }
    }
}

impl<'a> std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Label(label) => write!(f, "{}:", label),
            Line::Instruction(instruction) => write!(f, "{}", instruction),
            Line::Data(_, data) => data
                .iter()
                .map(|b| format!("{:#04X}", b))
                .collect::<Vec<String>>()
                .join(", ")
                .fmt(f),
            Line::Constant(_, expr) => write!(f, "equ {}", expr),
            Line::Times(_, expr, content) => write!(f, "times {} {}", expr, content),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_iter_values() {
        let expr = Expression::Value(0..0, Value::Constant(10));

        assert_eq!(
            vec![&Value::Constant(10)],
            expr.iter_values().collect::<Vec<&Value>>()
        );

        let expr = Expression::InfixOperator(
            0..0,
            Operator::Add,
            Box::new(Expression::Value(0..0, Value::Constant(10))),
            Box::new(Expression::Value(0..0, Value::Constant(20))),
        );

        assert_eq!(
            vec![&Value::Constant(10), &Value::Constant(20)],
            expr.iter_values().collect::<Vec<&Value>>()
        );

        let expr = Expression::InfixOperator(
            0..0,
            Operator::Add,
            Box::new(Expression::Value(0..0, Value::Constant(10))),
            Box::new(Expression::InfixOperator(
                0..0,
                Operator::Subtract,
                Box::new(Expression::Value(0..0, Value::Constant(20))),
                Box::new(Expression::Value(0..0, Value::Constant(30))),
            )),
        );

        assert_eq!(
            vec![
                &Value::Constant(10),
                &Value::Constant(20),
                &Value::Constant(30)
            ],
            expr.iter_values().collect::<Vec<&Value>>()
        );
    }
}
