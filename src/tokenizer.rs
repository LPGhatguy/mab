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
    In,
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
            Symbol::In => "in",
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum StringLiteral<'a> {
    DoubleQuote {
        raw_content: Cow<'a, str>,
    },
    SingleQuote {
        raw_content: Cow<'a, str>,
    },
    LongForm {
        raw_content: Cow<'a, str>,
        depth: u32,
    },
}

/// Represents a token kind.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenKind<'a> {
    /// An operator like `+` or `,` or a keyword like `not`.
    Symbol(Symbol),

    /// An identifier that is not a keyword.
    #[serde(borrow)]
    Identifier(Cow<'a, str>),

    /// A number literal as it appeared in the source.
    NumberLiteral(Cow<'a, str>),

    StringLiteral(StringLiteral<'a>),

    EndOfFile,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Comment<'a> {
    SingleLine {
        content: Cow<'a, str>,
    },
    MultiLine {
        content: Cow<'a, str>,
    },
}

/// An item that appears before tokens, like comments and whitespace.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TokenPrefix<'a> {
    #[serde(borrow)]
    Whitespace(Cow<'a, str>),

    Comment(Comment<'a>),
}

/// A token in the source.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token<'a> {
    /// The kind of token this token is.
    #[serde(borrow)]
    pub kind: TokenKind<'a>,

    /// Any whitespace and comments before the token.
    pub prefix: Vec<TokenPrefix<'a>>,

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

    /// A string was begun that never finished.
    UnclosedString {
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
        Symbol::In,
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

        Regex::new(&format!("^(?:{})", source)).unwrap()
    };

    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^[_a-zA-Z][_a-zA-Z0-9]*").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^((-?0x[A-Fa-f\d]+)|(-?((\d*\.\d+)|(\d+))([eE]-?\d+)?))").unwrap();
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();
    static ref PATTERN_SINGLE_LINE_COMMENT: Regex = Regex::new(r"^--(.*)").unwrap();
    static ref PATTERN_BLOCK_COMMENT: Regex = Regex::new(r"(?ms)^--\[\[(.*?)--\]\]").unwrap();

    static ref PATTERN_CHARS_AFTER_NEWLINE: Regex = Regex::new(r"\n[^\n]+$").unwrap();
}

struct AdvanceResult<'a> {
    rest: &'a str,
    contents: &'a str,
    new_position: SourcePosition,
}

enum AdvanceError {
    NoMatch,
    Error(TokenizeError),
}

/// Use a pattern to step forward in the source.
fn advance<'a>(source: &'a str, position: &SourcePosition, pattern: &Regex) -> Result<AdvanceResult<'a>, AdvanceError> {
    if let Some(range) = pattern.find(source) {
        let contents = &source[range.start()..range.end()];
        let rest = &source[range.end()..];
        let new_position = position.next_position(contents);

        Ok(AdvanceResult {
            rest,
            contents,
            new_position,
        })
    } else {
        Err(AdvanceError::NoMatch)
    }
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn advance_token<'a, F>(source: &'a str, position: &SourcePosition, pattern: &Regex, tokenizer: F) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    let result = advance(source, position, pattern)?;
    let token_kind = tokenizer(result.contents);
    Ok((result, token_kind))
}

// Similar to try! or the ? operator, but handles cases differently.
macro_rules! try_advance {
    ( $rule: expr ) => (
        match $rule {
            Ok(token_kind) => return Ok(token_kind),
            Err(AdvanceError::Error(err)) => return Err(AdvanceError::Error(err)),
            Err(_) => {},
        }
    )
}

fn parse_identifier<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    advance_token(current, &current_position, &PATTERN_IDENTIFIER, |s| {
        if let Some(&symbol) = STR_TO_SYMBOL.get(s) {
            TokenKind::Symbol(symbol)
        } else {
            TokenKind::Identifier(s.into())
        }
    })
}

fn parse_number_literal<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    advance_token(current, &current_position, &PATTERN_NUMBER_LITERAL, |s| TokenKind::NumberLiteral(s.into()))
}

fn parse_symbol<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    advance_token(current, &current_position, &PATTERN_SYMBOL, |s| TokenKind::Symbol(*STR_TO_SYMBOL.get(s).unwrap()))
}

fn parse_string_literal<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    let quote_character = if current.starts_with("\"") {
        '"'
    } else if current.starts_with("'") {
        '\''
    } else {
        return Err(AdvanceError::NoMatch);
    };

    let mut literal_end = None;
    let mut last_was_escape = false;

    for (index, character) in current.char_indices().skip(1) {
        if character == '\\' {
            last_was_escape = !last_was_escape;
        } else if character == quote_character {
            if last_was_escape {
                last_was_escape = false;
            } else {
                literal_end = Some(index);
                break;
            }
        } else if character == '\r' || character == '\n' {
            return Err(AdvanceError::Error(TokenizeError::UnclosedString {
                position: *current_position,
            }));
        } else {
            last_was_escape = false;
        }
    }

    let literal_end = match literal_end {
        Some(v) => v,
        None => {
            return Err(AdvanceError::Error(TokenizeError::UnclosedString {
                position: *current_position,
            }));
        },
    };

    let literal = match quote_character {
        '"' => StringLiteral::DoubleQuote {
            raw_content: Cow::from(&current[1..literal_end])
        },
        '\'' => StringLiteral::SingleQuote {
            raw_content: Cow::from(&current[1..literal_end])
        },
        _ => unreachable!(),
    };

    let advance_result = AdvanceResult {
        rest: &current[literal_end + 1..],
        contents: "",
        new_position: current_position.next_position(&current[..literal_end + 1]),
    };

    Ok((advance_result, TokenKind::StringLiteral(literal)))
}

fn parse_long_string_literal<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    Err(AdvanceError::NoMatch)
}

/// Attempts to advance one token into the stream.
fn tokenize_step<'a>(current: &'a str, current_position: &SourcePosition) -> Result<(AdvanceResult<'a>, TokenKind<'a>), AdvanceError> {
    try_advance!(parse_identifier(current, current_position));
    try_advance!(parse_number_literal(current, current_position));
    try_advance!(parse_symbol(current, current_position));
    try_advance!(parse_string_literal(current, current_position));
    try_advance!(parse_long_string_literal(current, current_position));

    Err(AdvanceError::NoMatch)
}

fn parse_whitespace<'a>(current: &'a str, position: &SourcePosition) -> Result<AdvanceResult<'a>, AdvanceError> {
    advance(current, position, &PATTERN_WHITESPACE)
}

fn parse_block_comment<'a>(current: &'a str, position: &SourcePosition) -> Result<(AdvanceResult<'a>, Comment<'a>), AdvanceError> {
    println!("Block comment? {:?}", current);
    if let Some(captures) = PATTERN_BLOCK_COMMENT.captures(current) {
        println!("Block comment!");
        let full_capture = captures.get(0).unwrap();
        let contents = full_capture.as_str();
        let rest = &current[full_capture.end()..];
        let new_position = position.next_position(contents);

        let comment = Comment::MultiLine {
            content: contents[2..].into(),
        };

        Ok((AdvanceResult {
            rest,
            contents,
            new_position,
        }, comment))
    } else {
        Err(AdvanceError::NoMatch)
    }
}

fn parse_comment<'a>(current: &'a str, position: &SourcePosition) -> Result<(AdvanceResult<'a>, Comment<'a>), AdvanceError> {
    if let Some(captures) = PATTERN_SINGLE_LINE_COMMENT.captures(current) {
        let full_capture = captures.get(0).unwrap();
        let contents = full_capture.as_str();
        let rest = &current[full_capture.end()..];
        let new_position = position.next_position(contents);

        let comment = Comment::SingleLine {
            content: contents[2..].into(),
        };

        Ok((AdvanceResult {
            rest,
            contents,
            new_position,
        }, comment))
    } else {
        Err(AdvanceError::NoMatch)
    }
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
        let mut prefix = Vec::new();

        loop {
            if let Ok(result) = parse_whitespace(current, &current_position) {
                current = result.rest;
                current_position = result.new_position;

                prefix.push(TokenPrefix::Whitespace(result.contents.into()));
            } else if let Ok((result, comment)) = parse_block_comment(current, &current_position) {
                current = result.rest;
                current_position = result.new_position;

                prefix.push(TokenPrefix::Comment(comment));
            } else if let Ok((result, comment)) = parse_comment(current, &current_position) {
                current = result.rest;
                current_position = result.new_position;

                prefix.push(TokenPrefix::Comment(comment));
            } else {
                break;
            }
        }

        if current.is_empty() {
            if !prefix.is_empty() {
                tokens.push(Token {
                    prefix,
                    kind: TokenKind::EndOfFile,
                    start_position: current_position.clone(),
                    end_position: current_position.clone(),
                });
            }

            break;
        }

        match tokenize_step(current, &current_position) {
            Ok((result, token_kind)) => {
                tokens.push(Token {
                    prefix,
                    kind: token_kind,
                    start_position: current_position.clone(),
                    end_position: result.new_position.clone(),
                });

                current = result.rest;
                current_position = result.new_position;
            },
            Err(AdvanceError::Error(e)) => return Err(e),
            Err(AdvanceError::NoMatch) => {
                if !current.is_empty() {
                    return Err(TokenizeError::UnknownSequence {
                        position: current_position,
                    });
                }
            },
        }
    }

    Ok(tokens)
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
    fn string_literals() {
        test_kinds_eq("\"\"", vec![TokenKind::StringLiteral(StringLiteral::DoubleQuote { raw_content: "".into() })]);
        test_kinds_eq("\"hello\"", vec![TokenKind::StringLiteral(StringLiteral::DoubleQuote { raw_content: "hello".into() })]);
        test_kinds_eq("\"he\\\"llo\"", vec![TokenKind::StringLiteral(StringLiteral::DoubleQuote { raw_content: "he\\\"llo".into() })]);
        test_kinds_eq("\"he\\nllo\"", vec![TokenKind::StringLiteral(StringLiteral::DoubleQuote { raw_content: "he\\nllo".into() })]);

        test_kinds_eq("''", vec![TokenKind::StringLiteral(StringLiteral::SingleQuote { raw_content: "".into() })]);
        test_kinds_eq("'hello'", vec![TokenKind::StringLiteral(StringLiteral::SingleQuote { raw_content: "hello".into() })]);
        test_kinds_eq("'he\\'llo'", vec![TokenKind::StringLiteral(StringLiteral::SingleQuote { raw_content: "he\\'llo".into() })]);
        test_kinds_eq("'he\\nllo'", vec![TokenKind::StringLiteral(StringLiteral::SingleQuote { raw_content: "he\\nllo".into() })]);

        assert_eq!(tokenize("\""), Err(TokenizeError::UnclosedString {
            position: SourcePosition {
                bytes: 0,
                line: 1,
                column: 1,
            },
        }));

        assert_eq!(tokenize("\"\n"), Err(TokenizeError::UnclosedString {
            position: SourcePosition {
                bytes: 0,
                line: 1,
                column: 1,
            },
        }));

        assert_eq!(tokenize("\"hello"), Err(TokenizeError::UnclosedString {
            position: SourcePosition {
                bytes: 0,
                line: 1,
                column: 1,
            },
        }));
    }

    #[test]
    fn whitespace() {
        let input = "  local";
        // This should always tokenize successfully
        let tokenized = tokenize(input).unwrap();
        let first_token = &tokenized[0];

        assert_eq!(first_token.prefix, &[TokenPrefix::Whitespace("  ".into())]);
    }

    #[test]
    fn whitespace_when_none_present() {
        let input = "local";
        let tokenized = tokenize(input).unwrap();
        let first_token = &tokenized[0];

        assert_eq!(first_token.prefix, &[]);
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
        let input = "local\n   test foo\n     bar";
        let tokenized = tokenize(input).unwrap();
        assert_eq!(tokenized, vec![
            Token {
                kind: TokenKind::Symbol(Symbol::Local),
                prefix: Vec::new(),
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
                prefix: vec![
                    TokenPrefix::Whitespace("\n   ".into()),
                ],
                start_position: SourcePosition {
                    bytes: 9,
                    line: 2,
                    column: 4,
                },
                end_position: SourcePosition {
                    bytes: 13,
                    line: 2,
                    column: 8,
                },
            },
            Token {
                kind: TokenKind::Identifier("foo".into()),
                prefix: vec![
                    TokenPrefix::Whitespace(" ".into()),
                ],
                start_position: SourcePosition {
                    bytes: 14,
                    line: 2,
                    column: 9,
                },
                end_position: SourcePosition {
                    bytes: 17,
                    line: 2,
                    column: 12,
                },
            },
            Token {
                kind: TokenKind::Identifier("bar".into()),
                prefix: vec![
                    TokenPrefix::Whitespace("\n     ".into()),
                ],
                start_position: SourcePosition {
                    bytes: 23,
                    line: 3,
                    column: 6,
                },
                end_position: SourcePosition {
                    bytes: 26,
                    line: 3,
                    column: 9,
                },
            }
        ]);
    }
}