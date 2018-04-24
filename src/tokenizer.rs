//! The tokenizer is the first stage of the parsing process. It converts raw
//! character input into a list of tokens, which are then used by the parser
//! to construct an AST.

use std::borrow::Cow;
use std::collections::HashMap;

use regex::{self, Regex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Symbol {
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    Hash,
    Equal,
    Comma,
    Semicolon,
    Ellipse,
}

impl Symbol {
    pub fn to_str(&self) -> &'static str {
        match *self {
            Symbol::LeftBrace => "{",
            Symbol::RightBrace => "}",
            Symbol::LeftBracket => "[",
            Symbol::RightBracket => "]",
            Symbol::LeftParen => "(",
            Symbol::RightParen => ")",
            Symbol::Plus => "+",
            Symbol::Minus => "-",
            Symbol::Star => "*",
            Symbol::Slash => "/",
            Symbol::Caret => "^",
            Symbol::Hash => "#",
            Symbol::Equal => "=",
            Symbol::Comma => ",",
            Symbol::Semicolon => ";",
            Symbol::Ellipse => "...",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Keyword {
    Local,
    Function,
    If,
    While,
    Repeat,
    Until,
    For,
    Then,
    Do,
    Else,
    ElseIf,
    End,
    True,
    False,
    Nil,
    Not,
    And,
    Or,
}

impl Keyword {
    pub fn to_str(&self) -> &'static str {
        match *self {
            Keyword::Local => "local",
            Keyword::Function => "function",
            Keyword::If => "if",
            Keyword::While => "while",
            Keyword::Repeat => "repeat",
            Keyword::Until => "until",
            Keyword::For => "for",
            Keyword::Then => "then",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::ElseIf => "elseif",
            Keyword::End => "end",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Nil => "nil",
            Keyword::Not => "not",
            Keyword::And => "and",
            Keyword::Or => "or",
        }
    }
}

/// Represents a token kind.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenKind<'a> {
    /// A reserved word of some form.
    Keyword(Keyword),

    /// An operator, like `+`, `-`, or `,`.
    Symbol(Symbol),

    #[serde(borrow)]
    /// An identifier that is not a keyword.
    Identifier(Cow<'a, str>),

    /// A number literal.
    /// The original value of the number, as it appeared in the source, is
    /// contained in the `&str` value.
    NumberLiteral(Cow<'a, str>),
}

/// A token in the source.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token<'a> {
    #[serde(borrow)]

    /// The kind of token this token is.
    pub kind: TokenKind<'a>,

    /// Any whitespace before the token.
    pub whitespace: Cow<'a, str>,

    /// The line in the source that the token came from.
    /// This starts at 1, not 0.
    pub line: usize,

    /// The column in the source that the token came from.
    /// This starts at 1, not 0.
    pub column: usize,

    // TODO: A slice from the source indicating what the token came from
}

/// An error with information about why tokenization failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizeError<'a> {
    /// The tokenizer encountered an unknown sequence in the source that it
    /// could not parse.
    UnknownSequence {
        /// The remaining source, starting at the unknown sequence.
        remainder: &'a str,
        /// The line in the source that the unknown sequence started at.
        line: usize,
        /// The column in the source that the unknown sequence started at.
        column: usize
    },
}

struct TryAdvanceResult<'a> {
    new_source: &'a str,
    eaten_str: &'a str,
    matched_kind: TokenKind<'a>,
}

lazy_static! {
    static ref KEYWORDS: Vec<Keyword> = vec![
        Keyword::Local, Keyword::Function,
        Keyword::If, Keyword::While, Keyword::Repeat, Keyword::Until, Keyword::For,
        Keyword::Then, Keyword::Do, Keyword::Else, Keyword::ElseIf, Keyword::End,
        Keyword::True, Keyword::False, Keyword::Nil,
        Keyword::Not, Keyword::And, Keyword::Or,
    ];

    static ref OPERATORS: Vec<Symbol> = vec![
        Symbol::LeftBrace, Symbol::RightBrace,
        Symbol::LeftBracket, Symbol::RightBracket,
        Symbol::LeftParen, Symbol::RightParen,

        Symbol::Plus, Symbol::Minus, Symbol::Star, Symbol::Slash, Symbol::Caret,
        Symbol::Hash,
        Symbol::Equal,
        Symbol::Comma, Symbol::Semicolon,
        Symbol::Ellipse,
    ];

    static ref STR_TO_KEYWORD: HashMap<&'static str, Keyword> = {
        let mut map = HashMap::new();

        for &keyword in KEYWORDS.iter() {
            map.insert(keyword.to_str(), keyword);
        }

        map
    };

    static ref STR_TO_OPERATOR: HashMap<&'static str, Symbol> = {
        let mut map = HashMap::new();

        for &operator in OPERATORS.iter() {
            map.insert(operator.to_str(), operator);
        }

        map
    };

    static ref PATTERN_OPERATOR: Regex = {
        let source = OPERATORS
            .iter()
            .map(|v| regex::escape(v.to_str()))
            .collect::<Vec<_>>()
            .join("|");

        Regex::new(&format!("^({})", source)).unwrap()
    };

    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^([_a-zA-Z][_a-zA-Z0-9]*)").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^((-?0x[A-Fa-f\d]+)|(-?(?:(?:\d*\.\d+)|(\d+))(?:[eE]-?\d+)?))").unwrap();

    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();
    static ref PATTERN_CHARS_AFTER_NEWLINE: Regex = Regex::new(r"\n([^\n]+)$").unwrap();
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn try_advance<'a, F>(source: &'a str, pattern: &Regex, tokenizer: F) -> Option<TryAdvanceResult<'a>>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    if let Some(captures) = pattern.captures(source) {
        // All patterns should have a capture, since some patterns (keywords)
        // have noncapturing groups that need to be ignored!
        let capture = captures.get(1).unwrap();
        let contents = capture.as_str();

        Some(TryAdvanceResult {
            new_source: &source[capture.end()..],
            eaten_str: contents,
            matched_kind: tokenizer(contents),
        })
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

    let column = if lines_eaten > 0 {
        // If there was a newline we're on a totally different column

        if let Some(captures) = PATTERN_CHARS_AFTER_NEWLINE.captures(eaten_str) {
            // If there's some characters after the newline, count them!
            // Add 1 so we start at a column of 1
            captures.get(1).unwrap().as_str().len() + 1
        }
        else {
            // Otherwise, just restart at 1.
            1
        }
    }
    else {
        // Otherwise we can just increment the current column by the length of the eaten chars
        current_column + eaten_str.len()
    };

    // We return the new line count, not the delta line count
    (current_line + lines_eaten, column)
}

/// Tokenizes a source string completely and returns a [Vec][Vec] of [Tokens][Token].
///
/// # Errors
/// Will return an [UnknownSequence][TokenizeError::UnknownSequence] if it
/// encounters a sequence of characters that it cannot parse.
// TODO: Change to returning iterator?
pub fn tokenize<'a>(source: &'a str) -> Result<Vec<Token<'a>>, TokenizeError<'a>> {
    let mut tokens = Vec::new();
    let mut current = source;
    let mut current_line = 1;
    let mut current_column = 1;

    loop {
        let (next_current, matched_whitespace) = eat(current, &PATTERN_WHITESPACE);
        let whitespace = matched_whitespace.unwrap_or("");

        current = next_current;

        let (new_line, new_column) = get_new_position(whitespace, current_line, current_column);
        current_line = new_line;
        current_column = new_column;

        let result = try_advance(current, &PATTERN_IDENTIFIER, |s| {
                if let Some(&keyword) = STR_TO_KEYWORD.get(s) {
                    TokenKind::Keyword(keyword)
                } else {
                    TokenKind::Identifier(s.into())
                }
            })
            .or_else(|| try_advance(current, &PATTERN_NUMBER_LITERAL, |s| TokenKind::NumberLiteral(s.into())))
            .or_else(|| try_advance(current, &PATTERN_OPERATOR, |s| TokenKind::Symbol(*STR_TO_OPERATOR.get(s).unwrap())));

        match result {
            Some(result) => {
                current = result.new_source;

                tokens.push(Token {
                    whitespace: whitespace.into(),
                    kind: result.matched_kind,
                    line: current_line,
                    column: current_column,
                });

                let (new_line, new_column) = get_new_position(result.eaten_str, current_line, current_column);
                current_line = new_line;
                current_column = new_column;
            }
            None => break,
        }
    }

    if current.is_empty() {
        Ok(tokens)
    } else {
        Err(TokenizeError::UnknownSequence {
            remainder: current,
            line: current_line,
            column: current_column,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_kinds_eq(input: &'static str, expected: Vec<TokenKind<'static>>) {
        let kinds = tokenize(input).unwrap().iter().map(|v| v.kind.clone()).collect::<Vec<_>>();
        assert_eq!(kinds, expected);
    }

    #[test]
    fn keyword_vs_identifier() {
        test_kinds_eq("local", vec![TokenKind::Keyword(Keyword::Local)]);
        test_kinds_eq("local_", vec![TokenKind::Identifier("local_".into())]);
        test_kinds_eq("locale", vec![TokenKind::Identifier("locale".into())]);
        test_kinds_eq("_local", vec![TokenKind::Identifier("_local".into())]);
        test_kinds_eq("local _", vec![TokenKind::Keyword(Keyword::Local), TokenKind::Identifier("_".into())]);
    }

    #[test]
    fn number_literals() {
        test_kinds_eq("6", vec![TokenKind::NumberLiteral("6".into())]);
        test_kinds_eq("0.231e-6", vec![TokenKind::NumberLiteral("0.231e-6".into())]);
        test_kinds_eq("-123.7", vec![TokenKind::NumberLiteral("-123.7".into())]);
        test_kinds_eq("0x12AfEE", vec![TokenKind::NumberLiteral("0x12AfEE".into())]);
        test_kinds_eq("-0x123FFe", vec![TokenKind::NumberLiteral("-0x123FFe".into())]);
        test_kinds_eq("1023.47e126", vec![TokenKind::NumberLiteral("1023.47e126".into())]);
    }

    #[test]
    fn whitespace() {
        let input = "  local";
        // This should always tokenize successfully
        let tokenized = tokenize(input).unwrap();
        let first_token = &tokenized[0];

        assert_eq!(first_token.whitespace, "  ");
    }

    #[test]
    fn whitespace_when_none_present() {
        let input = "local";
        let tokenized = tokenize(input).unwrap();
        let first_token = &tokenized[0];

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
                kind: TokenKind::Keyword(Keyword::Local),
                whitespace: "".into(),
                line: 1,
                column: 1,
            },
            Token {
                kind: TokenKind::Identifier("test".into()),
                whitespace: "\n                    ".into(),
                line: 2,
                column: 21,
            },
            Token {
                kind: TokenKind::Identifier("foo".into()),
                whitespace: " ".into(),
                line: 2,
                column: 26,
            },
            Token {
                kind: TokenKind::Identifier("bar".into()),
                whitespace: "\n                    ".into(),
                line: 3,
                column: 21,
            }
        ]);
    }
}
