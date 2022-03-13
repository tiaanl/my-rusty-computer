use bitflags::bitflags;
use mrc_instruction::{RegisterEncoding, Segment};
use std::fmt::{Display, Formatter};

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum ByteRegister {
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,
    AH = 4,
    CH = 5,
    DH = 6,
    BH = 7,
}

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum WordRegister {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
}

bitflags! {
    pub struct Flags : u16 {
        const CARRY = 1 << 0;
        const RESERVED_1 = 1 << 1;
        const PARITY = 1 << 2;
        const RESERVED_3 = 1 << 3;
        const AUX_CARRY = 1 << 4;
        const RESERVED_5 = 1 << 5;
        const ZERO = 1 << 6;
        const SIGN = 1 << 7;
        const TRAP = 1 << 8;
        const INTERRUPT = 1 << 9;
        const DIRECTION = 1 << 10;
        const OVERFLOW = 1 << 11;
    }
}

#[derive(Copy, Clone, Default, PartialEq)]
struct WordRegisters {
    ax: u16,
    cx: u16,
    dx: u16,
    bx: u16,

    sp: u16,
    bp: u16,
    si: u16,
    di: u16,
}

#[cfg(target_endian = "little")]
#[derive(Copy, Clone, Default, PartialEq)]
struct ByteRegisters {
    al: u8,
    ah: u8,
    cl: u8,
    ch: u8,
    dl: u8,
    dh: u8,
    bl: u8,
    bh: u8,
}

#[cfg(target_endian = "big")]
#[derive(Copy, Clone, Default, PartialEq)]
struct ByteRegisters {
    ah: u8,
    al: u8,
    ch: u8,
    cl: u8,
    dh: u8,
    dl: u8,
    bh: u8,
    bl: u8,
}

#[derive(Copy, Clone)]
union Registers {
    byte: ByteRegisters,
    word: WordRegisters,
}

pub trait RegisterAccess<T> {
    fn get_value(&self, state: &State) -> T;
    fn set_value(&self, state: &mut State, value: T);
}

impl RegisterAccess<u8> for ByteRegister {
    #[inline(always)]
    fn get_value(&self, state: &State) -> u8 {
        unsafe {
            match self {
                ByteRegister::AL => state.registers.byte.al,
                ByteRegister::CL => state.registers.byte.cl,
                ByteRegister::DL => state.registers.byte.dl,
                ByteRegister::BL => state.registers.byte.bl,
                ByteRegister::AH => state.registers.byte.ah,
                ByteRegister::CH => state.registers.byte.ch,
                ByteRegister::DH => state.registers.byte.dh,
                ByteRegister::BH => state.registers.byte.bh,
            }
        }
    }

    #[inline(always)]
    fn set_value(&self, state: &mut State, value: u8) {
        match self {
            ByteRegister::AL => state.registers.byte.al = value,
            ByteRegister::CL => state.registers.byte.cl = value,
            ByteRegister::DL => state.registers.byte.dl = value,
            ByteRegister::BL => state.registers.byte.bl = value,
            ByteRegister::AH => state.registers.byte.ah = value,
            ByteRegister::CH => state.registers.byte.ch = value,
            ByteRegister::DH => state.registers.byte.dh = value,
            ByteRegister::BH => state.registers.byte.bh = value,
        }
    }
}

impl RegisterAccess<u16> for WordRegister {
    #[inline(always)]
    fn get_value(&self, state: &State) -> u16 {
        unsafe {
            match self {
                WordRegister::AX => state.registers.word.ax,
                WordRegister::CX => state.registers.word.cx,
                WordRegister::DX => state.registers.word.dx,
                WordRegister::BX => state.registers.word.bx,
                WordRegister::SP => state.registers.word.sp,
                WordRegister::BP => state.registers.word.bp,
                WordRegister::SI => state.registers.word.si,
                WordRegister::DI => state.registers.word.di,
            }
        }
    }

    #[inline(always)]
    fn set_value(&self, state: &mut State, value: u16) {
        match self {
            WordRegister::AX => state.registers.word.ax = value,
            WordRegister::CX => state.registers.word.cx = value,
            WordRegister::DX => state.registers.word.dx = value,
            WordRegister::BX => state.registers.word.bx = value,
            WordRegister::SP => state.registers.word.sp = value,
            WordRegister::BP => state.registers.word.bp = value,
            WordRegister::SI => state.registers.word.si = value,
            WordRegister::DI => state.registers.word.di = value,
        }
    }
}

impl RegisterAccess<u8> for RegisterEncoding {
    fn get_value(&self, state: &State) -> u8 {
        match self {
            RegisterEncoding::AlAx => state.register(ByteRegister::AL),
            RegisterEncoding::ClCx => state.register(ByteRegister::CL),
            RegisterEncoding::DlDx => state.register(ByteRegister::DL),
            RegisterEncoding::BlBx => state.register(ByteRegister::BL),
            RegisterEncoding::AhSp => state.register(ByteRegister::AH),
            RegisterEncoding::ChBp => state.register(ByteRegister::CH),
            RegisterEncoding::DhSi => state.register(ByteRegister::DH),
            RegisterEncoding::BhDi => state.register(ByteRegister::BH),
        }
    }

    fn set_value(&self, state: &mut State, value: u8) {
        match self {
            RegisterEncoding::AlAx => state.set_register(ByteRegister::AL, value),
            RegisterEncoding::ClCx => state.set_register(ByteRegister::CL, value),
            RegisterEncoding::DlDx => state.set_register(ByteRegister::DL, value),
            RegisterEncoding::BlBx => state.set_register(ByteRegister::BL, value),
            RegisterEncoding::AhSp => state.set_register(ByteRegister::AH, value),
            RegisterEncoding::ChBp => state.set_register(ByteRegister::CH, value),
            RegisterEncoding::DhSi => state.set_register(ByteRegister::DH, value),
            RegisterEncoding::BhDi => state.set_register(ByteRegister::BH, value),
        }
    }
}

impl RegisterAccess<u16> for RegisterEncoding {
    fn get_value(&self, state: &State) -> u16 {
        match self {
            RegisterEncoding::AlAx => state.register(WordRegister::AX),
            RegisterEncoding::ClCx => state.register(WordRegister::CX),
            RegisterEncoding::DlDx => state.register(WordRegister::DX),
            RegisterEncoding::BlBx => state.register(WordRegister::BX),
            RegisterEncoding::AhSp => state.register(WordRegister::SP),
            RegisterEncoding::ChBp => state.register(WordRegister::BP),
            RegisterEncoding::DhSi => state.register(WordRegister::SI),
            RegisterEncoding::BhDi => state.register(WordRegister::DI),
        }
    }

    fn set_value(&self, state: &mut State, value: u16) {
        match self {
            RegisterEncoding::AlAx => state.set_register(WordRegister::AX, value),
            RegisterEncoding::ClCx => state.set_register(WordRegister::CX, value),
            RegisterEncoding::DlDx => state.set_register(WordRegister::DX, value),
            RegisterEncoding::BlBx => state.set_register(WordRegister::BX, value),
            RegisterEncoding::AhSp => state.set_register(WordRegister::SP, value),
            RegisterEncoding::ChBp => state.set_register(WordRegister::BP, value),
            RegisterEncoding::DhSi => state.set_register(WordRegister::SI, value),
            RegisterEncoding::BhDi => state.set_register(WordRegister::DI, value),
        }
    }
}

impl Default for Registers {
    fn default() -> Self {
        Self {
            word: WordRegisters::default(),
        }
    }
}

impl PartialEq for Registers {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.word == other.word }
    }
}

#[derive(Copy, Clone, Default, PartialEq)]
struct Segments {
    es: u16,
    cs: u16,
    ss: u16,
    ds: u16,
}

#[derive(Copy, Clone, PartialEq)]
pub struct State {
    registers: Registers,
    segments: Segments,

    pub ip: u16,
    pub flags: Flags,
}

impl State {
    pub fn with_flags(mut self, flags: Flags) -> Self {
        self.flags = flags;
        self
    }

    #[inline(always)]
    pub fn register<T, A: RegisterAccess<T>>(&self, register: A) -> T {
        register.get_value(self)
    }

    pub fn set_register<T, A: RegisterAccess<T>>(&mut self, register: A, value: T) {
        register.set_value(self, value);
    }

    #[inline(always)]
    pub fn segment(&self, segment: Segment) -> u16 {
        use Segment::*;

        match segment {
            ES => self.segments.es,
            CS => self.segments.cs,
            SS => self.segments.ss,
            DS => self.segments.ds,
        }
    }

    #[inline(always)]
    pub fn set_segment(&mut self, segment: Segment, value: u16) {
        use Segment::*;

        match segment {
            ES => self.segments.es = value,
            CS => self.segments.cs = value,
            SS => self.segments.ss = value,
            DS => self.segments.ds = value,
        }
    }
}

impl Default for State {
    fn default() -> Self {
        Self {
            registers: Registers::default(),
            segments: Segments::default(),
            ip: 0,
            flags: Flags::empty(),
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(f, "AX: {:04X} ", self.registers.word.ax)?;
            write!(f, "BX: {:04X} ", self.registers.word.bx)?;
            write!(f, "CX: {:04X} ", self.registers.word.cx)?;
            write!(f, "DX: {:04X} ", self.registers.word.dx)?;

            write!(f, "SP: {:04X} ", self.registers.word.sp)?;
            write!(f, "BP: {:04X} ", self.registers.word.bp)?;
            write!(f, "SI: {:04X} ", self.registers.word.si)?;
            write!(f, "DI: {:04X} ", self.registers.word.di)?;
        }

        write!(f, "ES: {:04X} ", self.segments.es)?;
        write!(f, "CS: {:04X} ", self.segments.cs)?;
        write!(f, "SS: {:04X} ", self.segments.ss)?;
        write!(f, "DS: {:04X} ", self.segments.ds)?;

        write!(f, "IP: {:04X} ", self.ip)?;

        macro_rules! print_flag {
            ($name:ident,$flag:expr) => {{
                if self.flags.contains($flag) {
                    write!(f, "{}", stringify!($name))?;
                } else {
                    write!(f, ".")?;
                }
            }};
        }

        print_flag!(O, Flags::OVERFLOW);
        print_flag!(D, Flags::DIRECTION);
        print_flag!(I, Flags::INTERRUPT);
        print_flag!(T, Flags::TRAP);
        print_flag!(S, Flags::SIGN);
        print_flag!(Z, Flags::ZERO);
        // print_flag!(R, Flags::RESERVED_5);
        print_flag!(A, Flags::AUX_CARRY);
        // print_flag!(R, Flags::RESERVED_3);
        print_flag!(P, Flags::PARITY);
        // print_flag!(R, Flags::RESERVED_1);
        print_flag!(C, Flags::CARRY);

        Ok(())
    }
}
