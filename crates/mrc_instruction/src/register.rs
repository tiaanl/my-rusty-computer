use crate::OperandSize;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum RegisterEncoding {
    AlAx,
    ClCx,
    DlDx,
    BlBx,
    AhSp,
    ChBp,
    DhSi,
    BhDi,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SizedRegisterEncoding(pub RegisterEncoding, pub OperandSize);

impl std::fmt::Display for SizedRegisterEncoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RegisterEncoding::*;

        match self.1 {
            OperandSize::Byte => match self.0 {
                AlAx => write!(f, "al"),
                ClCx => write!(f, "cl"),
                DlDx => write!(f, "dl"),
                BlBx => write!(f, "bl"),
                AhSp => write!(f, "ah"),
                ChBp => write!(f, "ch"),
                DhSi => write!(f, "dh"),
                BhDi => write!(f, "bh"),
            },

            OperandSize::Word => match self.0 {
                AlAx => write!(f, "ax"),
                ClCx => write!(f, "cx"),
                DlDx => write!(f, "dx"),
                BlBx => write!(f, "bx"),
                AhSp => write!(f, "sp"),
                ChBp => write!(f, "bp"),
                DhSi => write!(f, "si"),
                BhDi => write!(f, "di"),
            },
        }
    }
}

impl std::str::FromStr for SizedRegisterEncoding {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use OperandSize::*;
        use RegisterEncoding::*;

        match s.to_lowercase().as_str() {
            "al" => Ok(Self(AlAx, Byte)),
            "cl" => Ok(Self(ClCx, Byte)),
            "dl" => Ok(Self(DlDx, Byte)),
            "bl" => Ok(Self(BlBx, Byte)),
            "ah" => Ok(Self(AhSp, Byte)),
            "ch" => Ok(Self(ChBp, Byte)),
            "dh" => Ok(Self(DhSi, Byte)),
            "bh" => Ok(Self(BhDi, Byte)),

            "ax" => Ok(Self(AlAx, Word)),
            "cx" => Ok(Self(ClCx, Word)),
            "dx" => Ok(Self(DlDx, Word)),
            "bx" => Ok(Self(BlBx, Word)),
            "sp" => Ok(Self(AhSp, Word)),
            "bp" => Ok(Self(ChBp, Word)),
            "si" => Ok(Self(DhSi, Word)),
            "di" => Ok(Self(BhDi, Word)),

            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Segment {
    ES,
    CS,
    SS,
    DS,
}

impl std::fmt::Display for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Segment::*;

        match self {
            ES => write!(f, "es"),
            CS => write!(f, "cs"),
            SS => write!(f, "ss"),
            DS => write!(f, "ds"),
        }
    }
}

impl std::str::FromStr for Segment {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "es" => Ok(Segment::ES),
            "cs" => Ok(Segment::CS),
            "ss" => Ok(Segment::SS),
            "ds" => Ok(Segment::DS),
            _ => Err(s.to_string()),
        }
    }
}
