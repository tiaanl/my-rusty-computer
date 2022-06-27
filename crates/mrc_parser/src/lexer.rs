#[repr(u8)]
#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Base {
    Binary = 2,
    Octal = 8,
    Decimal = 10,
    Hexadecimal = 16,
}

impl Base {
    #[inline]
    fn try_from_char(c: char) -> Result<Self, ()> {
        Ok(match c {
            'b' | 'B' => Self::Binary,
            'o' | 'O' => Self::Octal,
            'd' | 'D' => Self::Decimal,
            'h' | 'H' => Self::Hexadecimal,
            _ => return Err(()),
        })
    }

    fn detect_highest_for(s: &str) -> Self {
        let mut base = Self::Binary;

        for c in s.chars() {
            if c > '1' {
                base = Self::Octal;
            } else if c > '8' {
                base = Self::Decimal
            } else if c > '9' {
                base = Self::Hexadecimal
            }
        }

        base
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LiteralKind {
    Number(i32),
    String(bool),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PunctuationKind {
    CloseBracket,
    CloseParenthesis,
    Colon,
    Comma,
    Dot,
    ForwardSlash,
    Minus,
    OpenBracket,
    OpenParenthesis,
    Plus,
    Star,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    Whitespace(usize),
    Identifier(usize),
    Literal(usize, LiteralKind),
    Punctuation(usize, PunctuationKind),
    Comment(usize),
    NewLine(usize),
    EndOfFile(usize),
    Invalid(usize, char),
}

impl Token {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        match self {
            Token::Whitespace(len)
            | Token::Identifier(len)
            | Token::Literal(len, _)
            | Token::Punctuation(len, _)
            | Token::Comment(len)
            | Token::NewLine(len)
            | Token::EndOfFile(len)
            | Token::Invalid(len, _) => *len,
        }
    }
}

pub struct Cursor<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    #[inline]
    pub fn pos(&self) -> usize {
        self.pos
    }

    #[inline]
    pub fn next_token(&mut self) -> Token {
        let token = Lexer::new(&self.source[self.pos..]).next_token();
        self.pos += token.len();
        token
    }

    pub fn source_at(&self, start: usize, len: usize) -> &'a str {
        if start + len >= self.source.len() {
            &self.source[start..]
        } else {
            &self.source[start..start + len]
        }
    }
}

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
}

macro_rules! first_not_of {
    ($self:expr, $predicate:ident) => {{
        first_not_of!($self, $predicate, 0)
    }};

    ($self:expr, $predicate:ident, $offset:expr) => {{
        match $self.source[$offset..].find(|c| !$predicate(c)) {
            Some(found) => found,
            None => $self.source.len() - $offset,
        }
    }};
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source }
    }

    fn char_at(&self, index: usize) -> Option<char> {
        if index < self.source.len() {
            self.source[index..].chars().next()
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Token {
        let first = match self.char_at(0) {
            Some(c) => c,
            None => return Token::EndOfFile(0),
        };

        match first {
            ';' => {
                match self.source.find(|c| c == '\n') {
                    Some(found) => Token::Comment(found),
                    None => {
                        // If a new line was not found, we have to take the rest of the source.
                        Token::Comment(self.source.len())
                    }
                }
            }

            c if is_whitespace(c) => Token::Whitespace(first_not_of!(self, is_whitespace)),

            c if is_decimal_digit(c) => self.number(c),

            c if is_identifier_first(c) => Token::Identifier(first_not_of!(self, is_identifier)),

            '\'' => self.string_literal(),

            '\n' => Token::NewLine(1),

            ':' => Token::Punctuation(1, PunctuationKind::Colon),
            ',' => Token::Punctuation(1, PunctuationKind::Comma),
            '.' => Token::Punctuation(1, PunctuationKind::Dot),
            '[' => Token::Punctuation(1, PunctuationKind::OpenBracket),
            ']' => Token::Punctuation(1, PunctuationKind::CloseBracket),
            '(' => Token::Punctuation(1, PunctuationKind::OpenParenthesis),
            ')' => Token::Punctuation(1, PunctuationKind::CloseParenthesis),
            '+' => Token::Punctuation(1, PunctuationKind::Plus),
            '-' => Token::Punctuation(1, PunctuationKind::Minus),
            '*' => Token::Punctuation(1, PunctuationKind::Star),
            '/' => Token::Punctuation(1, PunctuationKind::ForwardSlash),

            c => Token::Invalid(1, c),
        }
    }

    fn number(&mut self, first_char: char) -> Token {
        debug_assert!(is_decimal_digit(first_char));

        if first_char == '0' {
            self.number_literal_with_prefix()
        } else {
            self.number_literal_with_suffix()
        }
    }

    fn number_literal_with_prefix(&mut self) -> Token {
        macro_rules! number_with_base {
            ($prefix:expr, $is_digit:ident, $base:expr) => {{
                debug_assert!(self.char_at(0).unwrap() == '0');
                debug_assert!(self.char_at(1).unwrap() == $prefix);

                let mut end = 1;

                let found = first_not_of!(self, $is_digit, 2);

                // If no additional characters were found, then we return the '0' character as the
                // literal only.
                if found == 0 {
                    return Token::Literal(1, LiteralKind::Number(0));
                }

                // Consume the prefix.
                end += 1;
                let decode_pos = end;

                // We have additional characters, so we can consume the prefix character and the found
                // digits.
                end += found;

                // We can unwrap here, because we already made sure we only have valid characters.
                #[allow(clippy::from_str_radix_10)]
                let value = i32::from_str_radix(&self.source[decode_pos..end], $base).unwrap();
                Token::Literal(end, LiteralKind::Number(value))
            }};
        }

        if let Some(second_char) = self.char_at(1) {
            // If we have a prefix at this point, then we can handle it immediately.
            match second_char {
                c @ 'b' => return number_with_base!(c, is_binary_digit, 2),
                c @ 'o' => return number_with_base!(c, is_octal_digit, 8),
                c @ 'd' => return number_with_base!(c, is_decimal_digit, 10),
                c @ 'x' => return number_with_base!(c, is_hexadecimal_digit, 16),

                // [second_char] is not a valid prefix, so fall through to handle other formats.
                _ => self.number_literal_with_suffix(),
            }
        } else {
            // We reached the end of the source and we only have the '0' character.
            Token::Literal(1, LiteralKind::Number(0))
        }
    }

    fn number_literal_with_suffix(&mut self) -> Token {
        debug_assert!(is_decimal_digit(self.char_at(0).unwrap()));

        // Consume as many of the highest base (hexadecimal) characters as we can.
        let mut end = first_not_of!(self, is_hexadecimal_digit);

        let mut s = &self.source[..end];

        // We can unwrap here, because we are guaranteed to have at least a single numeric
        // character.
        let suffix = s.chars().last().unwrap();

        let base = match Base::try_from_char(suffix) {
            Ok(base) => {
                // If we found a suffix included in the string already, we have to cut it off.
                s = &s[..s.len() - 1];

                base
            }
            Err(_) => {
                // Consume another character to see if there is a suffix that is not a valid
                // hexadecimal character as well.
                match self.char_at(end) {
                    Some(c) => match Base::try_from_char(c) {
                        Ok(base) => {
                            // Consume the suffix.
                            end += 1;

                            base
                        }
                        Err(_) => Base::Decimal,
                    },
                    None => Base::Decimal,
                }
            }
        };

        let highest_base = Base::detect_highest_for(s);

        if highest_base > base {
            todo!("The requested base in lower than the detected base")
        }

        // We can unwrap here, because we already checked that all the characters are valid for the
        // base.
        let value = i32::from_str_radix(s, base as u32).unwrap();

        Token::Literal(end, LiteralKind::Number(value))
    }

    fn string_literal(&mut self) -> Token {
        // Consume the opening character.
        let mut end = 1;

        let first_terminator = self.source[end..].find('\'');
        let first_new_line = self.source[end..].find('\n');

        match (first_terminator, first_new_line) {
            (None, None) => {
                end = self.source.len();
                Token::Literal(end, LiteralKind::String(false))
            }

            (None, Some(new_line)) => {
                // Consume until the '\n' character.
                end += new_line;
                Token::Literal(end, LiteralKind::String(false))
            }

            (Some(terminator), None) => {
                // Consume the text and the terminator.
                end += terminator + 1;
                Token::Literal(end, LiteralKind::String(true))
            }

            (Some(terminator), Some(new_line)) => {
                if new_line < terminator {
                    end += new_line;

                    Token::Literal(end, LiteralKind::String(false))
                } else {
                    end += terminator + 1;

                    Token::Literal(end, LiteralKind::String(true))
                }
            }
        }
    }

    // pub fn _source_line_current(&self, path: Option<&str>) -> String {
    //     self._source_line_at_pos(self.pos, path)
    // }
    //
    // pub fn _source_line_at_pos(&self, pos: usize, path: Option<&str>) -> String {
    //     let prev_new_line = if let Some(found) = self.source[..pos].rfind('\n') {
    //         found + 1
    //     } else {
    //         0
    //     };
    //
    //     let next_new_line = if let Some(found) = self.source[pos..].find('\n') {
    //         pos + found
    //     } else {
    //         self.source.len()
    //     };
    //
    //     let fragment = &self.source[prev_new_line..next_new_line];
    //
    //     let line = self.source[0..pos].matches('\n').count() + 1;
    //     let column = pos - prev_new_line;
    //
    //     let mut result = String::new();
    //
    //     if let Some(path) = path {
    //         result += format!("{}:{}:{}\n", path, line, column + 1).as_str();
    //     }
    //
    //     result += fragment;
    //     result += "\n";
    //     for _ in 0..column {
    //         result += " ";
    //     }
    //     result += "^";
    //
    //     result
    // }
}

#[inline]
fn is_identifier_first(c: char) -> bool {
    ('a'..='z').contains(&c) | ('A'..='Z').contains(&c) || c == '_'
}

#[inline]
fn is_identifier(c: char) -> bool {
    is_identifier_first(c) || is_decimal_digit(c)
}

#[inline]
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

#[inline]
fn is_binary_digit(c: char) -> bool {
    c == '0' || c == '1'
}

#[inline]
fn is_octal_digit(c: char) -> bool {
    ('0'..='7').contains(&c)
}

#[inline]
fn is_decimal_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[inline]
fn is_hexadecimal_digit(c: char) -> bool {
    is_decimal_digit(c) || ('a'..='f').contains(&c) || ('A'..='F').contains(&c)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_next_token {
        ($source:literal, $token:expr, $match:literal) => {
            let source = $source;
            let token = Lexer::new(source).next_token();
            assert_eq!(token, $token);
            assert_eq!($match, &source[..token.len()]);
        };
    }

    macro_rules! assert_sequence {
        ($source:literal, $tokens:expr) => {{
            let mut cursor = Cursor::new($source);
            let mut lines = vec![];
            loop {
                let token = cursor.next_token();
                lines.push(token.clone());
                if matches!(token, Token::EndOfFile(_)) {
                    break;
                }
            }
        }};
    }

    // #[test]
    // fn snake() {
    //     let source = include_str!("../samples/snake.asm");
    //     let mut lexer = Lexer::new(source);
    //
    //     loop {
    //         let token = lexer.next_token();
    //         match token.kind {
    //             Token::EndOfFile => break,
    //             token => println!("{:?}", token),
    //         };
    //     }
    // }

    #[test]
    fn end_of_file() {
        assert_next_token!("", Token::EndOfFile(0), "");

        assert_sequence!("test", vec![Token::Identifier(4), Token::EndOfFile(_)]);
    }

    #[test]
    fn whitespace() {
        assert_next_token!("  \r \t", Token::Whitespace(5), "  \r \t");
        assert_next_token!("  \r \ttest", Token::Whitespace(5), "  \r \t");
    }

    #[test]
    fn comments() {
        assert_next_token!(
            "; this is a comment",
            Token::Comment(19),
            "; this is a comment"
        );
        assert_next_token!(
            "; this is a comment\nid",
            Token::Comment(19),
            "; this is a comment"
        );
    }

    #[test]
    fn string_literals() {
        assert_next_token!(
            "'a string literal ;; 123'",
            Token::Literal(25, LiteralKind::String(true)),
            "'a string literal ;; 123'"
        );

        assert_next_token!(
            "'a string literal ;; 123'test",
            Token::Literal(25, LiteralKind::String(true)),
            "'a string literal ;; 123'"
        );

        assert_next_token!(
            "'a string literal  ",
            Token::Literal(19, LiteralKind::String(false)),
            "'a string literal  "
        );
        assert_next_token!(
            "'a string literal\n'",
            Token::Literal(17, LiteralKind::String(false)),
            "'a string literal"
        );
    }

    #[test]
    fn number_literals() {
        // binary
        assert_next_token!(
            "11001000b",
            Token::Literal(9, LiteralKind::Number(200)),
            "11001000b"
        );
        assert_next_token!(
            "0b11001000",
            Token::Literal(10, LiteralKind::Number(200)),
            "0b11001000"
        );

        // octal
        assert_next_token!("310o", Token::Literal(4, LiteralKind::Number(200)), "310o");
        assert_next_token!(
            "0o310",
            Token::Literal(5, LiteralKind::Number(200)),
            "0o310"
        );

        // decimal
        assert_next_token!("200", Token::Literal(3, LiteralKind::Number(200)), "200");
        assert_next_token!("0200", Token::Literal(4, LiteralKind::Number(200)), "0200");
        assert_next_token!("200d", Token::Literal(4, LiteralKind::Number(200)), "200d");
        assert_next_token!(
            "0200d",
            Token::Literal(5, LiteralKind::Number(200)),
            "0200d"
        );
        assert_next_token!(
            "0d200",
            Token::Literal(5, LiteralKind::Number(200)),
            "0d200"
        );

        // hex
        assert_next_token!("0c8h", Token::Literal(4, LiteralKind::Number(200)), "0c8h");
        assert_next_token!("0xc8", Token::Literal(4, LiteralKind::Number(200)), "0xc8");
    }

    #[test]
    fn identifier() {
        assert_next_token!("test", Token::Identifier(4), "test");
        assert_next_token!("test\n", Token::Identifier(4), "test");
        assert_next_token!("_te_st", Token::Identifier(6), "_te_st");
        assert_next_token!("_te_st\n", Token::Identifier(6), "_te_st");
        assert_next_token!("test123", Token::Identifier(7), "test123");
        assert_next_token!("test123\n", Token::Identifier(7), "test123");

        assert_next_token!("1tst", Token::Literal(1, LiteralKind::Number(1)), "1");
    }
}
