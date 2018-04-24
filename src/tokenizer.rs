//! The tokenizer is the first stage of the parsing process. It converts raw
//! character input into a list of tokens, which are then used by the parser
//! to construct an AST.

use std::borrow::Cow;
use std::collections::HashMap;

use regex::{self, Regex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
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
                if let Some(&symbol) = STR_TO_SYMBOL.get(s) {
                    TokenKind::Symbol(symbol)
                } else {
                    TokenKind::Identifier(s.into())
                }
            })
            .or_else(|| try_advance(current, &PATTERN_NUMBER_LITERAL, |s| TokenKind::NumberLiteral(s.into())))
            .or_else(|| try_advance(current, &PATTERN_SYMBOL, |s| TokenKind::Symbol(*STR_TO_SYMBOL.get(s).unwrap())));

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
