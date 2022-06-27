use crate::Operation;

pub enum OperandEncoding {
    None,

    Imm8,
    Imm16,

    RegAl,
    RegAx,

    Reg8,
    Reg16,

    Seg,

    Mem8,
    Mem16,
}

pub struct InstructionData {
    operation: Operation,
    destination: OperandEncoding,
    source: OperandEncoding,
}

mod private {
    use super::*;

    macro_rules! id {
        ($operation:ident, $destination:ident, $source:ident, $codes:expr) => {{
            InstructionData {
                operation: Operation::$operation,
                destination: OperandEncoding::$destination,
                source: OperandEncoding::$source,
            }
        }};
    }

    #[rustfmt::skip]
    pub const INSTRUCTIONS: &[InstructionData] = &[
        id!(NOP, None,None, &[]),

        id!(MOV, Mem16, Seg,   &[]), // [mr: 8c   /r   ]
        id!(MOV, Reg16, Seg,   &[]), // [mr: o16  8c   /r ]
        id!(MOV, Seg,   Mem16, &[]), // [rm: 8e   /r   ]
        id!(MOV, Seg,   Reg16, &[]), // [rm: 8e   /r   ]
        id!(MOV, RegAl, Mem8,  &[]), // [-i: a0   iw   ]
        id!(MOV, RegAx, Mem16, &[]), // [-i: o16  a1   iw ]
        id!(MOV, Mem8,  RegAl, &[]), // [i-: a2   iw   ]
        id!(MOV, Mem16, RegAx, &[]), // [i-: o16  a3   iw ]
        id!(MOV, Mem8,  Reg8,  &[]), // [mr: 88   /r   ]
        id!(MOV, Reg8,  Reg8,  &[]), // [mr: 88   /r   ]
        id!(MOV, Mem16, Reg16, &[]), // [mr: o16  89   /r ]
        id!(MOV, Reg16, Reg16, &[]), // [mr: o16  89   /r ]
        id!(MOV, Reg8,  Mem8,  &[]), // [rm: 8a   /r   ]
        id!(MOV, Reg8,  Reg8,  &[]), // [rm: 8a   /r   ]
        id!(MOV, Reg16, Mem16, &[]), // [rm: o16  8b   /r ]
        id!(MOV, Reg16, Reg16, &[]), // [rm: o16  8b   /r ]
        id!(MOV, Reg8,  Imm8,  &[]), // [ri: b0+r ib   ]
        id!(MOV, Reg16, Imm16, &[]), // [ri: o16  b8+r iw ]
        id!(MOV, Mem8,  Imm8,  &[]), // [mi: c6   /0   ib ]
        id!(MOV, Mem16, Imm16, &[]), // [mi: o16  c7   /0 iw ]
        id!(MOV, Mem8,  Imm8,  &[]), // [mi: c6   /0   ib ]
        id!(MOV, Mem16, Imm16, &[]), // [mi: o16  c7   /0 iw ]
    ];
}

pub use private::INSTRUCTIONS;
