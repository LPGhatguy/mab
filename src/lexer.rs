use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    Keyword(&'a str),
    Whitespace(&'a str),
    Operator(&'a str),
    Identifier(&'a str),
    NumberLiteral(&'a str),
    OpenParen,
    CloseParen,
}

lazy_static! {
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();
    static ref PATTERN_KEYWORD: Regex = Regex::new(r"^(local)").unwrap();
    static ref PATTERN_IDENTIFIER: Regex = Regex::new(r"^[a-zA-Z]\w*").unwrap();
    static ref PATTERN_NUMBER_LITERAL: Regex = Regex::new(r"^[0-9]+").unwrap();
    static ref PATTERN_OPERATOR: Regex = Regex::new(r"^(=|\+)").unwrap();

    static ref PATTERN_OPEN_PAREN: Regex = Regex::new(r"^\(").unwrap();
    static ref PATTERN_CLOSE_PAREN: Regex = Regex::new(r"^\)").unwrap();
}

/// Tries to matches the given pattern against the string slice.
/// If it does, the 'tokenizer' fn is invokved to turn the result into a token.
fn try_advance<'a, F>(source: &'a str, pattern: &Regex, tokenizer: F) -> Option<(&'a str, Token<'a>)>
where
    F: Fn(&'a str) -> Token<'a>,
{
    if let Some(range) = pattern.find(source) {
        let contents = &source[range.start()..range.end()];
        Some((&source[range.end()..], tokenizer(contents)))
    } else {
        None
    }
}

// TODO: Change to iterator!
pub fn lex<'a>(source: &'a str) -> Vec<Token<'a>> {
    let mut tokens = Vec::new();
    let mut current = source;

    loop {
        let result = try_advance(current, &PATTERN_WHITESPACE, |s| Token::Whitespace(s))
            .or_else(|| try_advance(current, &PATTERN_KEYWORD, |s| Token::Keyword(s)))
            .or_else(|| try_advance(current, &PATTERN_IDENTIFIER, |s| Token::Identifier(s)))
            .or_else(|| try_advance(current, &PATTERN_OPERATOR, |s| Token::Operator(s)))
            .or_else(|| try_advance(current, &PATTERN_NUMBER_LITERAL, |s| Token::NumberLiteral(s)))
            .or_else(|| try_advance(current, &PATTERN_OPEN_PAREN, |_| Token::OpenParen))
            .or_else(|| try_advance(current, &PATTERN_CLOSE_PAREN, |_| Token::CloseParen));

        match result {
            Some((next_current, token)) => {
                current = next_current;

                tokens.push(token);
            }
            None => break,
        }
    }

    if !current.is_empty() {
        eprintln!("Unknown garbage at {:?}", current);
    }

    tokens
}
