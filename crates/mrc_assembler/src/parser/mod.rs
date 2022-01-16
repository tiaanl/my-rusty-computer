pub(crate) mod ast;
pub(crate) mod base;
pub(crate) mod instructions;
pub(crate) mod sources;

pub(crate) type Span<'a> = nom_locate::LocatedSpan<&'a str>;

#[macro_export]
macro_rules! span_at {
    ($offset:expr,$line:expr,$fragment:expr) => {{
        unsafe { Span::new_from_raw_offset($offset, $line, $fragment, ()) }
    }};
}

pub type ParseResult<'s, T> = nom::IResult<Span<'s>, T>;

#[cfg(test)]
mod tests {
    #[macro_export]
    macro_rules! test_span {
        ($fn:ident,$input:expr,$offset:expr,$line:expr,$rest:expr,$output:expr) => {{
            use crate::span_at;
            assert_eq!(
                $fn(Span::new($input)),
                Ok((span_at!($offset, $line, $rest), $output))
            );
        }};
    }
}
