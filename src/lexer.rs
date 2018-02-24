use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
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
    // Any whitespace before the token
    whitespace: Option<&'a str>,
    kind: TokenKind<'a>,
}

lazy_static! {
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();

    static ref PATTERN_KEYWORD: Regex = Regex::new(r"^(local)").unwrap();
    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^[a-zA-Z]\w*").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref PATTERN_OPERATOR: Regex = Regex::new(r"^(=|\+|,)").unwrap();
    static ref PATTERN_OPEN_PAREN: Regex = Regex::new(r"^\(").unwrap();
    static ref PATTERN_CLOSE_PAREN: Regex = Regex::new(r"^\)").unwrap();
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn try_advance<'a, F>(source: &'a str, pattern: &Regex, tokenizer: F) -> Option<(&'a str, TokenKind<'a>)>
where
    F: Fn(&'a str) -> TokenKind<'a>,
{
    if let Some(range) = pattern.find(source) {
        let contents = &source[range.start()..range.end()];
        Some((&source[range.end()..], tokenizer(contents)))
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

// TODO: Change to iterator!
pub fn lex<'a>(source: &'a str) -> Vec<Token<'a>> {
    let mut tokens = Vec::new();
    let mut current = source;

    loop {
        let (next_current, whitespace) = eat(current, &PATTERN_WHITESPACE);
        current = next_current;

        let result = try_advance(current, &PATTERN_KEYWORD, |s| TokenKind::Keyword(s))
            .or_else(|| try_advance(current, &PATTERN_IDENTIFIER, |s| TokenKind::Identifier(s)))
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

    if !current.is_empty() {
        eprintln!("Unknown garbage at {:?}", current);
    }

    tokens
}
