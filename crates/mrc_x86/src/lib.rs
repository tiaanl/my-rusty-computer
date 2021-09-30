#![warn(missing_debug_implementations, rust_2018_idioms)]

pub mod printer;

#[derive(PartialEq, Debug)]
pub enum Operation {
    Aaa,
    Aad,
    Aam,
    Aas,
    Adc,
    Add,
    And,
    Baa,
    Call,
    Cbw,
    Clc,
    Cld,
    Cli,
    Cmc,
    Cmp,
    Cmpsb,
    Cmpsw,
    Cwd,
    Daa,
    Das,
    Dec,
    Div,
    Esc,
    Hlt,
    IRet,
    Idiv,
    Imul,
    In,
    Inc,
    Int,
    Into,
    Jb,
    Jbe,
    Jcxz,
    Je,
    Jl,
    Jle,
    Jmp,
    Jnb,
    Jnbe,
    Jne,
    Jnl,
    Jnle,
    Jno,
    Jnp,
    Jns,
    Jo,
    Jp,
    Js,
    Lahf,
    Lds,
    Lea,
    Les,
    Lock,
    Lodsb,
    Lodsw,
    Loop,
    Loopnz,
    Loopz,
    Mov,
    Movsb,
    Movsw,
    Mul,
    Neg,
    Nop,
    Not,
    Or,
    Out,
    Pop,
    Popf,
    Push,
    Pushf,
    Rcl,
    Rcr,
    Ret,
    Retf,
    Rol,
    Ror,
    Sahf,
    Sar,
    Sbb,
    Scasb,
    Scasw,
    Shl,
    Shr,
    Stc,
    Std,
    Sti,
    Stos,
    Sub,
    Test,
    Wait,
    Xchg,
    Xlat,
    Xor,
}

#[derive(PartialEq, Debug)]
pub enum Register {
    AlAx,
    ClCx,
    DlDx,
    BlBx,
    AhSp,
    ChBp,
    DhSi,
    BhDi,
}

#[derive(PartialEq, Debug)]
pub enum Segment {
    Es,
    Cs,
    Ss,
    Ds,
}

#[derive(Debug, PartialEq)]
pub enum AddressingMode {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx,
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub enum OperandSize {
    Byte,
    Word,
}

#[derive(PartialEq, Debug)]
pub enum Displacement {
    None,
    Byte(i8),
    Word(i16),
}

#[derive(PartialEq, Debug)]
pub enum OperandType {
    Direct(u16),
    Indirect(AddressingMode, Displacement),
    Register(Register),
    Segment(Segment),
    Immediate(u16),
}

#[derive(PartialEq, Debug)]
pub struct Operand(pub OperandType, pub OperandSize);

#[derive(PartialEq, Debug)]
pub enum OperandSet {
    None,
    Destination(Operand),
    DestinationAndSource(Operand, Operand),
    Displacement(Displacement),
    SegmentAndOffset(u16, u16),
}

#[derive(PartialEq, Debug)]
pub enum Repeat {
    Equal,
    NotEqual,
}

#[derive(PartialEq, Debug)]
pub struct Instruction {
    pub operation: Operation,
    pub operands: OperandSet,
    pub segment_override: Option<Segment>,
    pub repeat: Option<Repeat>,
    pub lock: bool,
}

impl Instruction {
    pub fn new(operation: Operation, operands: OperandSet) -> Self {
        Self {
            operation,
            operands,
            segment_override: None,
            repeat: None,
            lock: false,
        }
    }
    pub fn with_segment_override(
        segment: Segment,
        operation: Operation,
        operands: OperandSet,
    ) -> Self {
        Self {
            operation,
            operands,
            segment_override: Some(segment),
            repeat: None,
            lock: false,
        }
    }
}
