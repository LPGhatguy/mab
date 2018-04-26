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
    TwoDots,
    Equal,
    Comma,
    Semicolon,
    Ellipse,
    And,
    Or,
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
            Symbol::TwoDots => "..",
            Symbol::Equal => "=",
            Symbol::Comma => ",",
            Symbol::Semicolon => ";",
            Symbol::Ellipse => "...",
            Symbol::And => "and",
            Symbol::Or => "or",
            Symbol::Not => "not",
            Symbol::Local => "local",
            Symbol::Function => "function",
            Symbol::If => "if",
            Symbol::While => "while",
            Symbol::Repeat => "repeat",
            Symbol::Until => "until",
            Symbol::For => "for",
            Symbol::Then => "then",
            Symbol::Do => "do",
            Symbol::Else => "else",
            Symbol::ElseIf => "elseif",
            Symbol::End => "end",
            Symbol::True => "true",
            Symbol::False => "false",
            Symbol::Nil => "nil",
        }
    }
}

/// Represents a position in the source text.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourcePosition {
    /// The number of bytes into the source, starting at 0.
    pub bytes: usize,

    /// The line , starting at 1.
    pub line: usize,

    /// The column in the source, starting at 1.
    pub column: usize,
}

impl SourcePosition {
    /// Calculate the source position after stepping over the given string.
    pub fn next_position(&self, consumed: &str) -> SourcePosition {
        let lines_consumed = consumed.matches("\n").count();

        let column = if lines_consumed > 0 {
            // If there was a newline we're on a totally different column
            if let Some(range) = PATTERN_CHARS_AFTER_NEWLINE.find(consumed) {
                range.end() - range.start()
            } else {
                0
            }
        } else {
            // Otherwise we can just increment the current column by the length of the eaten chars
            self.column + consumed.len()
        };

        SourcePosition {
            bytes: self.bytes + consumed.len(),
            line: self.line + lines_consumed,
            column,
        }
    }
}

/// Represents a token kind.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenKind<'a> {
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

    /// The start of the token, not including whitespace, inclusive.
    pub start_position: SourcePosition,

    /// The end of the token, not including whitespace, exclusive.
    pub end_position: SourcePosition,
}

/// An error with information about why tokenization failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenizeError {
    /// The tokenizer encountered an unknown sequence in the source that it
    /// could not parse.
    UnknownSequence {
        /// The location in the source where the unknown sequence began.
        position: SourcePosition,
    },
}

lazy_static! {
    static ref SYMBOLS: Vec<Symbol> = vec![
        Symbol::LeftBrace, Symbol::RightBrace,
        Symbol::LeftBracket, Symbol::RightBracket,
        Symbol::LeftParen, Symbol::RightParen,

        Symbol::Plus, Symbol::Minus, Symbol::Star, Symbol::Slash, Symbol::Caret, Symbol::TwoDots,
        Symbol::And, Symbol::Or,
        Symbol::Hash,
        Symbol::Equal,
        Symbol::Comma, Symbol::Semicolon,
        Symbol::Ellipse,

        Symbol::Local, Symbol::Function,
        Symbol::If, Symbol::While, Symbol::Repeat, Symbol::Until, Symbol::For,
        Symbol::Then, Symbol::Do, Symbol::Else, Symbol::ElseIf, Symbol::End,
        Symbol::True, Symbol::False, Symbol::Nil,
        Symbol::Not,
    ];

    static ref STR_TO_SYMBOL: HashMap<&'static str, Symbol> = {
        let mut map = HashMap::new();

        for &operator in SYMBOLS.iter() {
            map.insert(operator.to_str(), operator);
        }

        map
    };

    static ref PATTERN_SYMBOL: Regex = {
        let source = SYMBOLS
            .iter()
            .map(|v| regex::escape(v.to_str()))
            .collect::<Vec<_>>()
            .join("|");

        Regex::new(&format!("^{}", source)).unwrap()
    };

    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^[_a-zA-Z][_a-zA-Z0-9]*").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^((-?0x[A-Fa-f\d]+)|(-?((\d*\.\d+)|(\d+))([eE]-?\d+)?))").unwrap();
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();

    static ref PATTERN_CHARS_AFTER_NEWLINE: Regex = Regex::new(r"\n[^\n]+$").unwrap();
}

struct AdvanceResult<'a> {
    rest: &'a str,
    contents: &'a str,
    new_position: SourcePosition,
}

/// Use a pattern to step forward in the source.
fn advance<'a>(source: &'a str, position: &SourcePosition, pattern: &Regex) -> Option<AdvanceResult<'a>> {
    if let Some(range) = pattern.find(source) {
        let contents = &source[range.start()..range.end()];
        let rest = &source[range.end()..];
        let new_position = position.next_position(contents);

        Some(AdvanceResult {
            rest,
            contents,
            new_position,
        })
    } else {
        None
    }
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn advance_token<'a, F>(source: &'a str, position: &SourcePosition, pattern: &Regex, tokenizer: F) -> Option<(AdvanceResult<'a>, TokenKind<'a>)>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    let result = advance(source, position, pattern)?;
    let token_kind = tokenizer(result.contents);
    Some((result, token_kind))
}

/// Tokenizes a source string completely and returns a [Vec][Vec] of [Tokens][Token].
///
/// # Errors
/// Will return an [UnknownSequence][TokenizeError::UnknownSequence] if it
/// encounters a sequence of characters that it cannot parse.
// TODO: Change to returning iterator?
pub fn tokenize<'a>(source: &'a str) -> Result<Vec<Token<'a>>, TokenizeError> {
    let mut tokens = Vec::new();
    let mut current = source;
    let mut current_position = SourcePosition {
        line: 1,
        column: 1,
        bytes: 0,
    };

    loop {
        let whitespace = match advance(current, &current_position, &PATTERN_WHITESPACE) {
            Some(result) => {
                current = result.rest;
                current_position = result.new_position;
                result.contents
            },
            None => "",
        };

        let advancement = advance_token(current, &current_position, &PATTERN_IDENTIFIER, |s| {
                if let Some(&symbol) = STR_TO_SYMBOL.get(s) {
                    TokenKind::Symbol(symbol)
                } else {
                    TokenKind::Identifier(s.into())
                }
            })
            .or_else(|| advance_token(current, &current_position, &PATTERN_NUMBER_LITERAL, |s| TokenKind::NumberLiteral(s.into())))
            .or_else(|| advance_token(current, &current_position, &PATTERN_SYMBOL, |s| TokenKind::Symbol(*STR_TO_SYMBOL.get(s).unwrap())));

        match advancement {
            Some((result, token_kind)) => {
                tokens.push(Token {
                    whitespace: whitespace.into(),
                    kind: token_kind,
                    start_position: current_position.clone(),
                    end_position: result.new_position.clone(),
                });

                current = result.rest;
                current_position = result.new_position;
            },
            None => break,
        }
    }

    if current.is_empty() {
        Ok(tokens)
    } else {
        Err(TokenizeError::UnknownSequence {
            position: current_position,
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
        test_kinds_eq("local", vec![TokenKind::Symbol(Symbol::Local)]);
        test_kinds_eq("local_", vec![TokenKind::Identifier("local_".into())]);
        test_kinds_eq("locale", vec![TokenKind::Identifier("locale".into())]);
        test_kinds_eq("_local", vec![TokenKind::Identifier("_local".into())]);
        test_kinds_eq("local _", vec![TokenKind::Symbol(Symbol::Local), TokenKind::Identifier("_".into())]);
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
        let position = SourcePosition {
            bytes: 0,
            line: 1,
            column: 1,
        };

        let new_position = position.next_position("test");
        assert_eq!(new_position.bytes, 4);
        assert_eq!(new_position.line, 1);
        assert_eq!(new_position.column, 5);

        let new_position = position.next_position("testy\ntest");
        assert_eq!(new_position.bytes, 10);
        assert_eq!(new_position.line, 2);
        assert_eq!(new_position.column, 5);
    }

    #[test]
    fn source_tracking() {
        let input = "local
                    test foo
                    bar";
        let tokenized = tokenize(input).unwrap();
        assert_eq!(tokenized, vec![
            Token {
                kind: TokenKind::Symbol(Symbol::Local),
                whitespace: "".into(),
                start_position: SourcePosition {
                    bytes: 0,
                    line: 1,
                    column: 1,
                },
                end_position: SourcePosition {
                    bytes: 5,
                    line: 1,
                    column: 6,
                },
            },
            Token {
                kind: TokenKind::Identifier("test".into()),
                whitespace: "\n                    ".into(),
                start_position: SourcePosition {
                    bytes: 26,
                    line: 2,
                    column: 21,
                },
                end_position: SourcePosition {
                    bytes: 30,
                    line: 2,
                    column: 25,
                },
            },
            Token {
                kind: TokenKind::Identifier("foo".into()),
                whitespace: " ".into(),
                start_position: SourcePosition {
                    bytes: 31,
                    line: 2,
                    column: 26,
                },
                end_position: SourcePosition {
                    bytes: 34,
                    line: 2,
                    column: 29,
                },
            },
            Token {
                kind: TokenKind::Identifier("bar".into()),
                whitespace: "\n                    ".into(),
                start_position: SourcePosition {
                    bytes: 55,
                    line: 3,
                    column: 21,
                },
                end_position: SourcePosition {
                    bytes: 58,
                    line: 3,
                    column: 24,
                },
            }
        ]);
    }
}