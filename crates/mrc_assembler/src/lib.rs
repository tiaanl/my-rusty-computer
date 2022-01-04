pub(crate) mod parser;

use crate::parser::sources::parse_line;
use crate::parser::ParseResult;
use mrc_instruction::{Instruction, OperandSet};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    multi::many1,
    sequence::{delimited, terminated, tuple},
    IResult,
};
use parser::{
    combinators::{parse_identifier, parse_number},
    instructions::{parse_operand, parse_register},
    sources::Line,
    ParseError,
};

pub fn parse_source(input: &str) -> ParseResult<Vec<Line>> {
    delimited(multispace0, many1(parse_line), multispace0)(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source() {
        let source = r"(
            BITS            16
            ORG             0x0100
            
            start:
                    mov     ax, cs
        )";
        /*
                    mov     ds, ax
                    call    add_one
                    call    add_two

            add_one:
                    inc     byte [count]            ; increase the count
                    ret

            add_two:
                    mov     al, [count]
                    add     al, 2
                    mov     [count], al
                    ret

            exit:
                    mov     ax, 0x4C00
                    int     0x21

            count:  db      0x01                    ; stores a sample count
        */

        assert_eq!(parse_source(source), Ok(("", vec![])));
    }
}
