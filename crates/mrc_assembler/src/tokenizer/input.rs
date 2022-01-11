#![allow(unused)]

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Location {
    line: usize,
    col: usize,
}

impl Location {
    pub(crate) fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }

    fn push(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

#[derive(Debug, PartialEq)]
enum InputError {
    /// No characters matched the given predicate.
    NoMatch,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Input<'s> {
    pub(crate) location: Location,
    pub(crate) slice: &'s str,
}

impl<'s> Input<'s> {
    pub(crate) fn new(slice: &'s str) -> Self {
        Self {
            location: Location::default(),
            slice,
        }
    }

    fn with_location(slice: &'s str, location: Location) -> Self {
        Self { location, slice }
    }

    fn take_while(
        &self,
        mut predicate: impl FnMut(char) -> bool,
    ) -> Result<(Input, Input), InputError> {
        let mut location = self.location;
        let mut count = 0usize;

        for c in self.slice.chars() {
            if !predicate(c) {
                break;
            }
            location.push(c);
            count += 1;
        }

        if count == 0 {
            return Err(InputError::NoMatch);
        }

        let prefix =
            Input::with_location(unsafe { self.slice.get_unchecked(..count) }, self.location);
        let suffix = Input::with_location(unsafe { self.slice.get_unchecked(count..) }, location);

        Ok((suffix, prefix))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn take_while() {
        let i = Input::new("some stuff");
        assert_eq!(
            i.take_while(|c| c.is_alphabetic()),
            Ok((
                Input::with_location(" stuff", Location::new(1, 5)),
                Input::with_location("some", Location::new(1, 1)),
            ))
        );

        let i = Input::new("some stuff");
        assert_eq!(
            i.take_while(|c| c.is_whitespace()),
            Err(InputError::NoMatch)
        );
    }
}
