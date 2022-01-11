mod input;

use input::Location;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum PunctuationKind {
    Colon,
    Comma,
    LeftBracket,
    Plus,
    RightBracket,
    SemiColon,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LiteralKind<'t> {
    Number(i32),
    String(&'t str),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum TokenKind<'t> {
    NewLine,
    Identifier(&'t str),
    Punctuation(PunctuationKind),
    Literal(LiteralKind<'t>),
    Comment(&'t str),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'t> {
    location: Location,
    token: TokenKind<'t>,
}

impl<'t> Token<'t> {
    pub(crate) fn new(token: TokenKind<'t>) -> Self {
        Self {
            location: Location::default(),
            token,
        }
    }

    pub(crate) fn with_location(token: TokenKind<'t>, location: Location) -> Self {
        Self { location, token }
    }
}
