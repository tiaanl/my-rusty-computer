#![allow(unused)]

use nom::{
    error::{ErrorKind, ParseError},
    Compare, CompareResult, Err, IResult, InputIter, InputLength, InputTake, InputTakeAtPosition,
    Needed, Offset, Slice,
};
use std::{
    ops::{Range, RangeFrom, RangeTo},
    str::Chars,
};

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
}

impl<'s> std::fmt::Display for Input<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}:{}) {}",
            self.location.line,
            self.location.col,
            &self.slice[..10],
        )
    }
}

impl<'i> Slice<Range<usize>> for Input<'i> {
    fn slice(&self, range: Range<usize>) -> Self {
        let mut location = self.location;
        for c in self.slice.chars().take(range.start) {
            location.push(c);
        }

        Self {
            location,
            slice: &self.slice[range],
        }
    }
}

impl<'i> Slice<RangeFrom<usize>> for Input<'i> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        let mut location = self.location;
        for c in self.slice.chars().take(range.start) {
            location.push(c);
        }

        Self {
            location,
            slice: &self.slice[range],
        }
    }
}

impl<'i> Slice<RangeTo<usize>> for Input<'i> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        Self {
            location: self.location,
            slice: &self.slice[range],
        }
    }
}

pub(crate) struct InputChar {
    c: char,
    location: Location,
}

#[derive(Debug)]
pub(crate) struct InputCharIndices {}

impl Iterator for InputCharIndices {
    type Item = (usize, InputChar);

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

#[derive(Debug)]
pub(crate) struct InputChars {}

impl Iterator for InputChars {
    type Item = InputChar;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

impl<'i> InputIter for Input<'i> {
    type Item = InputChar;
    type Iter = InputCharIndices;
    type IterElem = InputChars;

    fn iter_indices(&self) -> Self::Iter {
        todo!()
    }

    fn iter_elements(&self) -> Self::IterElem {
        todo!()
    }

    fn position<P>(&self, predicate: P) -> Option<usize> where P: Fn(Self::Item) -> bool {
        todo!()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        todo!()
    }
}

impl<'i> InputLength for Input<'i> {
    fn input_len(&self) -> usize {
        self.slice.input_len()
    }
}

impl<'i, 'a> Compare<&'a str> for Input<'i> {
    fn compare(&self, t: &'a str) -> CompareResult {
        self.slice.compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> CompareResult {
        self.slice.compare_no_case(t)
    }
}

impl<'i> InputTake for Input<'i> {
    fn take(&self, count: usize) -> Self {
        Self::new(self.slice.take(count))
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.slice.take_split(count);

        (Self::new(prefix), Self::new(suffix))
    }
}

impl<'i> InputTakeAtPosition for Input<'i> {
    type Item = char;

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    Input::new(self.slice.get_unchecked(i..)),
                    Input::new(self.slice.get_unchecked(..i)),
                ))
            },
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(Input::new(self.slice), e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    Input::new(self.slice.get_unchecked(i..)),
                    Input::new(self.slice.get_unchecked(..i)),
                ))
            },
            None => Err(Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    Input::new(self.slice.get_unchecked(i..)),
                    Input::new(self.slice.get_unchecked(..i)),
                ))
            },
            // the end of slice is a char boundary
            None => unsafe {
                Ok((
                    Input::new(self.slice.get_unchecked(self.slice.len()..)),
                    Input::new(self.slice.get_unchecked(..self.slice.len())),
                ))
            },
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.slice.find(predicate) {
            Some(0) => Err(Err::Error(E::from_error_kind(Input::new(self.slice), e))),
            // find() returns a byte index that is already in the slice at a char boundary
            Some(i) => unsafe {
                Ok((
                    Input::new(self.slice.get_unchecked(i..)),
                    Input::new(self.slice.get_unchecked(..i)),
                ))
            },
            None => {
                if self.slice.is_empty() {
                    Err(Err::Error(E::from_error_kind(Input::new(self.slice), e)))
                } else {
                    // the end of slice is a char boundary
                    unsafe {
                        Ok((
                            Input::new(self.slice.get_unchecked(self.slice.len()..)),
                            Input::new(self.slice.get_unchecked(..self.slice.len())),
                        ))
                    }
                }
            }
        }
    }
}

impl<'i> Offset for Input<'i> {
    fn offset(&self, second: &Self) -> usize {
        self.slice.offset(second.slice)
    }
}

impl<'i> ParseError<Input<'i>> for Input<'i> {
    fn from_error_kind(input: Input<'i>, kind: ErrorKind) -> Self {
        todo!()
    }

    fn append(input: Input<'i>, kind: ErrorKind, other: Self) -> Self {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use nom::character::complete::alphanumeric1;
    use super::*;

    #[test]
    fn test_range() {
        assert_eq!(
            Input::new("some stuff").slice(2..4),
            Input::with_location("me", Location::new(1, 3))
        );
    }

    #[test]
    fn test_range_from() {
        assert_eq!(
            Input::new("some stuff").slice(3..),
            Input::with_location("e stuff", Location::new(1, 4))
        );
    }

    #[test]
    fn test_range_to() {
        assert_eq!(
            Input::new("some stuff").slice(..6),
            Input::with_location("some s", Location::new(1, 1))
        );
    }

    #[test]
    fn test_input_iter() {
        let input = Input::new("some stuff");
        let i = input.iter_indices();

        println!("{:?}", i);
    }
}
