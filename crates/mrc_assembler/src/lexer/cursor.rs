use std::str::Chars;

pub(crate) const EOF_CHAR: char = '\0';

pub(crate) struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Self {
            initial_len: input.len(),
            chars: input.chars(),
        }
    }

    // Peeks the nth character in the input.
    pub(crate) fn nth_char(&self, n: usize) -> char {
        self.chars().nth(n).unwrap_or(EOF_CHAR)
    }

    /// Peeks the first character in the input.
    pub(crate) fn first(&self) -> char {
        self.nth_char(0)
    }

    /// True if we are at the end of the input.
    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    /// Moves to the next character.
    pub(crate) fn consume(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Returns the amount of characters already consumed.
    pub(crate) fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_consume() {
        let mut c = Cursor::new("abcd");
        assert_eq!(Some('a'), c.consume());
        assert_eq!(Some('b'), c.consume());
        assert_eq!(Some('c'), c.consume());
        assert_eq!(Some('d'), c.consume());
    }

    #[test]
    fn test_len_consumed() {
        let mut c = Cursor::new("abcd");
        assert_eq!(0, c.len_consumed());
        let _ = c.consume().unwrap();
        assert_eq!(1, c.len_consumed());
        let _ = c.consume().unwrap();
        assert_eq!(2, c.len_consumed());
    }
}
