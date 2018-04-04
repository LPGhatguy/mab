use std::collections::HashSet;
use std::iter::FromIterator;

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Keyword(&'a str),
    Operator(&'a str),
    Identifier(&'a str),
    NumberLiteral(&'a str),
    OpenParen,
    CloseParen,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    /// Any whitespace before the token
    pub whitespace: &'a str,

    /// Details about the Token itself
    pub kind: TokenKind<'a>,

    pub line: usize,
    pub column: usize,
    // TODO: A slice from the source indicating what the token came from
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizeError<'a> {
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
    static ref PATTERN_CHARS_AFTER_NEWLINE: Regex = Regex::new(r"\n([^\n]+)$").unwrap();
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn try_advance<'a, F>(source: &'a str, pattern: &Regex, tokenizer: F) -> Option<(&'a str, &'a str, TokenKind<'a>)>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    if let Some(captures) = pattern.captures(source) {
        // All patterns should have a capture, since some patterns (keywords)
        // have noncapturing groups that need to be ignored!
        let capture = captures.get(1).unwrap();
        let contents = capture.as_str();
        Some((&source[capture.end()..], contents, tokenizer(contents)))
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

fn get_new_position<'a>(eaten_str: &'a str, current_line: usize, current_column: usize) -> (usize, usize) {
    let lines_eaten = eaten_str.matches("\n").count();

    // If there was a newline we're on a totally different column
    let column = if lines_eaten > 0 {
        // If there's some characters after the newline, count them!
        if let Some(captures) = PATTERN_CHARS_AFTER_NEWLINE.captures(eaten_str) {
            // Add 1 so we start at a column of 1
            captures.get(1).unwrap().as_str().len() + 1
        }
        // Otherwise, just restart at 1.
        else {
            1
        }
    }
    // Otherwise we can just increment the current column by the length of the eaten chars
    else {
        current_column + eaten_str.len()
    };

    // We return the new line count, not the delta line count
    (current_line + lines_eaten, column)
}

// TODO: Change to returning iterator?
pub fn tokenize<'a>(source: &'a str) -> Result<Vec<Token<'a>>, TokenizeError<'a>> {
    let mut tokens = Vec::new();
    let mut current = source;
    let mut current_line: usize = 1;
    let mut current_column: usize = 1;

    loop {
        let (next_current, matched_whitespace) = eat(current, &PATTERN_WHITESPACE);
        let whitespace = matched_whitespace.unwrap_or("");

        current = next_current;

        let (new_line, new_column) = get_new_position(whitespace, current_line, current_column);
        current_line = new_line;
        current_column = new_column;

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
            Some((next_current, eaten_str, token_kind)) => {
                current = next_current;

                tokens.push(Token {
                    whitespace,
                    kind: token_kind,
                    line: current_line,
                    column: current_column,
                });

                let (new_line, new_column) = get_new_position(eaten_str, current_line, current_column);
                current_line = new_line;
                current_column = new_column;
            }
            None => break,
        }
    }

    if current.is_empty() {
        Ok(tokens)
    } else {
        Err(TokenizeError::UnknownSequence(current))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_vs_identifier() {
        fn test_eq(input: &'static str, expected: Vec<TokenKind<'static>>) {
            let kinds = tokenize(input).unwrap().iter().map(|v| v.kind.clone()).collect::<Vec<_>>();

            assert_eq!(kinds, expected);
        }

        test_eq("local", vec![TokenKind::Keyword("local")]);
        test_eq("local_", vec![TokenKind::Identifier("local_")]);
        test_eq("locale", vec![TokenKind::Identifier("locale")]);
        test_eq("_local", vec![TokenKind::Identifier("_local")]);
        test_eq("local _", vec![TokenKind::Keyword("local"), TokenKind::Identifier("_")]);
    }

    #[test]
    fn whitespace() {
        let input = "  local";
        // This should always tokenize successfully
        let tokenized = tokenize(input).unwrap();
        let first_token = tokenized[0];

        assert_eq!(first_token.whitespace, "  ");
    }

    #[test]
    fn whitespace_when_none_present() {
        let input = "local";
        let tokenized = tokenize(input).unwrap();
        let first_token = tokenized[0];

        assert_eq!(first_token.whitespace, "");
    }

    #[test]
    fn get_new_line_info() {
        let (new_line, new_column) = get_new_position("test", 1, 1);
        assert_eq!(new_line, 1);
        assert_eq!(new_column, 5);

        let (new_line, new_column) = get_new_position("testy\ntest", 1, 1);
        assert_eq!(new_line, 2);
        assert_eq!(new_column, 5);
    }

    #[test]
    fn source_tracking() {
        let input = "local
                    test foo
                    bar";
        let tokenized = tokenize(input).unwrap();
        assert_eq!(tokenized, vec![
            Token {
                kind: TokenKind::Keyword("local"),
                whitespace: "",
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("test"),
                whitespace: "\n                    ",
                line: 2,
                column: 21,
            },
            Token {
                kind: TokenKind::Identifier("foo"),
                whitespace: " ",
                line: 2,
                column: 26,
            },
            Token {
                kind: TokenKind::Identifier("bar"),
                whitespace: "\n                    ",
                line: 3,
                column: 21,
            }
        ]);
    }
}
