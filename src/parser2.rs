use tokenizer::{Token, TokenKind, Symbol};
use ast::Chunk;
use parser_core::ParseState;

pub enum ParseError {
    UnexpectedEof(usize),
    UnexpectedToken(usize),
}

type ParseResult<'state, T> = Result<(ParseState<'state>, T), ParseError>;

pub fn parse_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Result<Chunk<'a>, ParseError> {
    let state = ParseState::new(tokens);

    let (state, chunk) = parse_chunk(state)?;

    match state.peek() {
        Some(Token { kind: TokenKind::EndOfFile, .. }) | None => {},
        Some(token) => return Err(ParseError::UnexpectedToken(state.position)),
    }

    Ok(chunk)
}

fn parse_chunk<'state>(state: ParseState<'state>) -> ParseResult<'state, Chunk<'state>> {
    unimplemented!()
}