mod cursor;

use crate::lexer::cursor::Cursor;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    /// A token not expected by the lexer.
    Unknown,

    /// A comment starting with a `;` and ending with a new-line sequence.
    LineComment,

    /// A sequence of non-printable characters. spaces, tabs, new-lines, etc.
    Whitespace,

    /// A piece of text.
    Identifier,

    /// A hard-coded value.
    Literal(LiteralKind),

    // Single character tokens.
    Comma,
    Dot,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Colon,
    Minus,
    Plus,
    Star,
    ForwardSlash,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    Binary,
    Octal,
    Hexadecimal,
    Decimal,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    Int(Base),
    Char,
    Byte,
    String,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

/// Parses the first token from the provided input string.
pub fn first_token(input: &str) -> Token {
    debug_assert!(!input.is_empty());
    Cursor::new(input).advance_token()
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    std::iter::from_fn(move || {
        if input.is_empty() {
            return None;
        }
        let token = first_token(input);
        input = &input[token.len..];
        Some(token)
    })
}

impl Cursor<'_> {
    fn advance_token(&mut self) -> Token {
        let first_char = self.consume().unwrap();

        let token_kind = match first_char {
            ';' => {
                self.consume_while(|c| c != '\n');
                TokenKind::LineComment
            }

            c if is_whitespace(c) => {
                self.consume_while(is_whitespace);
                TokenKind::Whitespace
            }

            c if is_identifier_start(c) => {
                self.consume_while(is_identifier);
                TokenKind::Identifier
            }

            c @ '0'..='9' => {
                let kind = self.number(c);
                TokenKind::Literal(kind)
            }

            // Single character tokens.
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            ':' => TokenKind::Colon,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            '*' => TokenKind::Star,
            '/' => TokenKind::ForwardSlash,

            _ => TokenKind::Unknown,
        };

        Token::new(token_kind, self.len_consumed())
    }

    fn number(&mut self, c: char) -> LiteralKind {
        let mut base = Base::Decimal;

        if c == '0' {
            // Attempt to parse encoding base.
            match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.consume();
                    self.consume_while(is_decimal_digit);
                }
                'o' => {
                    base = Base::Octal;
                    self.consume();
                    self.consume_while(is_decimal_digit);
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.consume();
                    self.consume_while(is_decimal_digit);
                }
                // Not a base prefix.
                c if is_decimal_digit(c) => {
                    self.consume_while(is_decimal_digit);
                }
                // Just a 0.
                _ => {
                    return LiteralKind::Int(base);
                }
            };
        } else {
            // No base prefix, parse number in the usual way.
            self.consume_while(is_decimal_digit);
        }

        LiteralKind::Int(base)
    }

    fn consume_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.consume();
        }
    }

    // fn consume_identifier(&mut self) {
    //     if !is_identifier_start(self.first()) {
    //         return;
    //     }
    //     self.consume();
    //     self.consume_while(is_identifier);
    // }
}

/// True if `c` is a non-printable whitespace character.
pub(crate) fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is the first character of a valid identifier.
pub(crate) fn is_identifier_start(c: char) -> bool {
    // TODO: Handle unicode
    c == '_' || ('a'..='z').contains(&c) || ('A'..='Z').contains(&c)
}

/// True if `c` is a valid character for an identifier.
pub(crate) fn is_identifier(c: char) -> bool {
    // TODO: Handle unicode
    is_identifier_start(c) || is_decimal_digit(c)
}

pub(crate) fn is_decimal_digit(c: char) -> bool {
    ('0'..='9').contains(&c)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_first_token() {
        let token = first_token("hello");
        assert_eq!(TokenKind::Identifier, token.kind);
        assert_eq!(5, token.len);
    }

    #[test]
    fn test_tokenize() {
        let mut it = tokenize("hello rust");

        let token = it.next().unwrap();
        assert_eq!(TokenKind::Identifier, token.kind);
        assert_eq!(5, token.len);

        let token = it.next().unwrap();
        assert_eq!(TokenKind::Whitespace, token.kind);
        assert_eq!(1, token.len);

        let token = it.next().unwrap();
        assert_eq!(TokenKind::Identifier, token.kind);
        assert_eq!(4, token.len);
    }

    #[test]
    fn test_whitespace() {
        let token = first_token("\t test");
        assert_eq!(TokenKind::Whitespace, token.kind);
        assert_eq!(2, token.len);
    }

    #[test]
    fn test_identifier() {
        let token = first_token("test");
        assert_eq!(TokenKind::Identifier, token.kind);
        assert_eq!(4, token.len);
    }

    #[test]
    fn test_literal() {
        let token = first_token("10");
        assert!(matches!(
            token.kind,
            TokenKind::Literal(LiteralKind::Int(Base::Decimal)),
        ));
        assert_eq!(2, token.len);

        let token = first_token("0x10");
        assert!(matches!(
            token.kind,
            TokenKind::Literal(LiteralKind::Int(Base::Hexadecimal)),
        ));
        assert_eq!(4, token.len);

        let token = first_token("0b10");
        assert!(matches!(
            token.kind,
            TokenKind::Literal(LiteralKind::Int(Base::Binary))
        ));
        assert_eq!(4, token.len);

        let token = first_token("0o10");
        assert!(matches!(
            token.kind,
            TokenKind::Literal(LiteralKind::Int(Base::Octal)),
        ));
        assert_eq!(4, token.len);
    }

    #[test]
    fn single_character_tokens() {
        use TokenKind::*;

        let mut tokens = tokenize(",.()[]:-+*/");
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: Comma,
                len: 1
            })
        ));
        assert!(matches!(tokens.next(), Some(Token { kind: Dot, len: 1 })));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: OpenParen,
                len: 1
            })
        ));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: CloseParen,
                len: 1
            })
        ));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: OpenBracket,
                len: 1
            })
        ));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: CloseBracket,
                len: 1
            })
        ));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: Colon,
                len: 1
            })
        ));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: Minus,
                len: 1
            })
        ));
        assert!(matches!(tokens.next(), Some(Token { kind: Plus, len: 1 })));
        assert!(matches!(tokens.next(), Some(Token { kind: Star, len: 1 })));
        assert!(matches!(
            tokens.next(),
            Some(Token {
                kind: ForwardSlash,
                len: 1
            })
        ));
    }

    #[test]
    fn parse_source() {
        let source = r#"
        mov     ax, 10
        inc     ax
        mov     ax, word [bp + 0x02]
        "#;

        let tokens = tokenize(source)
            .into_iter()
            .filter(|t| t.kind != TokenKind::Whitespace);
        for token in tokens {
            println!("{:?}", token);
        }
    }
}
