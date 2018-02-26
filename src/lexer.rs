use std::collections::HashSet;
use std::iter::FromIterator;

use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Keyword(&'a str),
    Operator(&'a str),
    Identifier(&'a str),
    NumberLiteral(&'a str),
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    /// Any whitespace before the token
    pub whitespace: Option<&'a str>,

    /// Details about the Token itself
    pub kind: TokenKind<'a>,

    // TODO: The line/column that the token is on?
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError<'a> {
    UnknownSequence(&'a str),
}

lazy_static! {
    static ref KEYWORDS: HashSet<&'static str> = HashSet::from_iter(vec![
        "false", "true", "nil",
        "local",
    ]);

    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^([_a-zA-Z][_a-zA-Z0-9]*)").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^([0-9]+)").unwrap();
    static ref PATTERN_OPERATOR: Regex = Regex::new(r"^(=|\+|,)").unwrap();
    static ref PATTERN_OPEN_PAREN: Regex = Regex::new(r"^(\()").unwrap();
    static ref PATTERN_CLOSE_PAREN: Regex = Regex::new(r"^(\))").unwrap();

    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn try_advance<'a, F>(source: &'a str, pattern: &Regex, tokenizer: F) -> Option<(&'a str, TokenKind<'a>)>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    if let Some(captures) = pattern.captures(source) {
        // All patterns should have a capture, since some patterns (keywords)
        // have noncapturing groups that need to be ignored!
        let capture = captures.get(1).unwrap();
        let contents = capture.as_str();
        Some((&source[capture.end()..], tokenizer(contents)))
    } else {
        None
    }
}

fn eat<'a>(source: &'a str, pattern: &Regex) -> (&'a str, Option<&'a str>) {
    if let Some(range) = pattern.find(source) {
        let contents = &source[range.start()..range.end()];

        (&source[range.end()..], Some(contents))
    } else {
        (source, None)
    }
}

// TODO: Change to returning iterator?
pub fn lex<'a>(source: &'a str) -> Result<Vec<Token<'a>>, LexError<'a>> {
    let mut tokens = Vec::new();
    let mut current = source;

    loop {
        let (next_current, whitespace) = eat(current, &PATTERN_WHITESPACE);
        current = next_current;

        let result = try_advance(current, &PATTERN_IDENTIFIER, |s| {
                if KEYWORDS.contains(s) {
                    TokenKind::Keyword(s)
                } else {
                    TokenKind::Identifier(s)
                }
            })
            .or_else(|| try_advance(current, &PATTERN_OPERATOR, |s| TokenKind::Operator(s)))
            .or_else(|| try_advance(current, &PATTERN_NUMBER_LITERAL, |s| TokenKind::NumberLiteral(s)))
            .or_else(|| try_advance(current, &PATTERN_OPEN_PAREN, |_| TokenKind::OpenParen))
            .or_else(|| try_advance(current, &PATTERN_CLOSE_PAREN, |_| TokenKind::CloseParen));

        match result {
            Some((next_current, token_kind)) => {
                current = next_current;

                tokens.push(Token {
                    whitespace,
                    kind: token_kind,
                });
            }
            None => break,
        }
    }

    if current.is_empty() {
        Ok(tokens)
    } else {
        Err(LexError::UnknownSequence(current))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_vs_identifier() {
        fn test_eq(input: &'static str, expected: Vec<TokenKind<'static>>) {
            let kinds = lex(input).unwrap().iter().map(|v| v.kind.clone()).collect::<Vec<_>>();

            assert_eq!(kinds, expected);
        }

        test_eq("local", vec![TokenKind::Keyword("local")]);
        test_eq("local_", vec![TokenKind::Identifier("local_")]);
        test_eq("locale", vec![TokenKind::Identifier("locale")]);
        test_eq("_local", vec![TokenKind::Identifier("_local")]);
        test_eq("local _", vec![TokenKind::Keyword("local"), TokenKind::Identifier("_")]);
    }
}
