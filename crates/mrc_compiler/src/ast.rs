use mrc_instruction::Operation;
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
pub enum Value {
    Constant(i32),
    Label(Label),
    Register(Register),
}

impl<'a> std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(value) => write!(f, "{}", *value),
            Value::Label(label) => write!(f, "{}", *label),
            Value::Register(register) => write!(f, "{}", *register),
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
    PrefixOperator(Operator, Box<Expression>),
    InfixOperator(Operator, Box<Expression>, Box<Expression>),

    Term(Value),
}

impl<'a> std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::PrefixOperator(operator, right) => {
                write!(f, "{}({})", operator, right)
            }
            Expression::InfixOperator(operator, left, right) => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::Term(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Operand {
    Immediate(Span, Expression),
    Address(Span, Expression, Option<DataSize>, Option<Segment>),
    Register(Span, Register),
    Segment(Span, Segment),
}

impl<'a> std::fmt::Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Immediate(_, expr) => expr.fmt(f),

            Operand::Address(_, expr, data_size, segment) => {
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
pub enum LineContent {
    None,
    Instruction(Span, Instruction),
    Data(Span, Vec<u8>),
    Constant(Span, Expression),
    Times(Span, Expression, Box<LineContent>),
}

impl<'a> std::fmt::Display for LineContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineContent::None => Ok(()),
            LineContent::Instruction(_, instruction) => write!(f, "{}", instruction),
            LineContent::Data(_, data) => write!(f, "db {:?}", data),
            LineContent::Constant(_, value) => write!(f, "equ {}", value),
            LineContent::Times(_, expr, content) => write!(f, "times {} {}", expr, content),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Line {
    pub label: Option<Label>,
    pub content: LineContent,
}

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Line { label, content } = self;

        if let Some(label) = label {
            write!(f, "{}: {}", label, content)
        } else {
            write!(f, "{}", content)
        }
    }
}
