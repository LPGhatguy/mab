use std::borrow::Cow;

use tokenizer::{Token, TokenKind};
use ast::*;

type ParseResult<'a, T> = Result<(ParseState<'a>, T), ParseError<'a>>;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<'a> {
    NoMatch,
    Err(Cow<'a, str>),
}

#[derive(Debug, Clone, Copy)]
struct ParseState<'a> {
    tokens: &'a [Token<'a>],
    position: usize,
}

impl<'a> ParseState<'a> {
    pub fn new(tokens: &'a [Token]) -> ParseState<'a> {
        ParseState {
            tokens,
            position: 0,
        }
    }

    pub fn peek(&self) -> Option<&'a Token<'a>> {
        self.tokens.get(self.position)
    }

    pub fn advance(&self, amount: usize) -> ParseState<'a> {
        ParseState {
            tokens: self.tokens,
            position: self.position + amount,
        }
    }
}

fn eat_simple<'a>(state: ParseState<'a>, eat_token: TokenKind) -> ParseResult<'a, &'a Token<'a>> {
    match state.peek() {
        Some(token) => {
            if token.kind == eat_token {
                Ok((state.advance(1), token))
            } else {
                Err(ParseError::NoMatch)
            }
        },
        None => Err(ParseError::NoMatch),
    }
}

fn parse_number<'a>(state: ParseState<'a>) -> ParseResult<'a, &'a str> {
    match state.peek() {
        Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => Ok((state.advance(1), value)),
        _ => Err(ParseError::NoMatch),
    }
}

fn parse_identifier<'a>(state: ParseState<'a>) -> ParseResult<'a, &'a str> {
    match state.peek() {
        Some(&Token { kind: TokenKind::Identifier(name), .. }) => Ok((state.advance(1), name)),
        _ => Err(ParseError::NoMatch),
    }
}

pub fn parse_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Option<Chunk<'a>> {
    let state = ParseState::new(tokens);

    let (state, chunk) = match parse_chunk(state) {
        Ok(result) => result,
        Err(_) => return None,
    };

    match state.peek() {
        Some(_) => return None,
        None => {},
    }

    Some(chunk)
}

// chunk ::= {stat [`;´]} [laststat [`;´]]
fn parse_chunk<'a>(state: ParseState<'a>) -> ParseResult<'a, Chunk<'a>> {
    let mut statements = Vec::new();
    let mut state = state;

    loop {
        state = match parse_statement(state) {
            Ok((next_state, statement)) => {
                statements.push(statement);
                next_state
            },
            Err(_) => break,
        };
    }

    let chunk = Chunk {
        statements,
    };

    Ok((state, chunk))
}

// stat ::= varlist `=´ explist |
//     functioncall |
//     do chunk end |
//     while exp do chunk end |
//     repeat chunk until exp |
//     if exp then chunk {elseif exp then chunk} [else chunk] end |
//     for Name `=´ exp `,´ exp [`,´ exp] do chunk end |
//     for namelist in explist do chunk end |
//     function funcname funcbody |
//     local function Name funcbody |
//     local namelist [`=´ explist]
fn parse_statement<'a>(state: ParseState<'a>) -> ParseResult<'a, Statement<'a>> {
    match parse_local_assignment(state) {
        Ok((state, assignment)) => return Ok((state, Statement::LocalAssignment(assignment))),
        _ => {},
    }

    match parse_function_call(state) {
        Ok((state, call)) => return Ok((state, Statement::FunctionCall(call))),
        _ => {},
    }

    Err(ParseError::NoMatch)
}

// local namelist [`=´ explist]
// right now:
// local Name `=` exp
fn parse_local_assignment<'a>(state: ParseState<'a>) -> ParseResult<'a, LocalAssignment<'a>> {
    let (state, _) = eat_simple(state, TokenKind::Keyword("local"))?;
    let (state, name) = parse_identifier(state)?;
    let (state, _) = eat_simple(state, TokenKind::Operator("="))?;
    let (state, expression) = parse_expression(state)?;

    Ok((state, LocalAssignment {
        name,
        value: expression,
    }))
}

// functioncall ::= prefixexp args | prefixexp `:´ Name args
// right now:
// functioncall ::= Name `(` explist `)`
fn parse_function_call<'a>(state: ParseState<'a>) -> ParseResult<'a, FunctionCall<'a>> {
    let (state, name) = parse_identifier(state)?;
    let (state, _) = eat_simple(state, TokenKind::OpenParen)?;
    let (state, expressions) = parse_expression_list(state);
    let (state, _) = eat_simple(state, TokenKind::CloseParen)?;

    Ok((state, FunctionCall {
        name_expression: Box::new(Expression::Name(name)),
        arguments: expressions,
    }))
}

// exp ::= unop exp | value [binop exp]
fn parse_expression<'a>(state: ParseState<'a>) -> ParseResult<'a, Expression<'a>> {
    match parse_number(state) {
        Ok((state, value)) => return Ok((state, Expression::Number(value))),
        Err(_) => {},
    }

    match parse_function_call(state) {
        Ok((state, call)) => return Ok((state, Expression::FunctionCall(call))),
        Err(_) => {},
    }

    Err(ParseError::NoMatch)
}

// explist ::= {exp `,´} exp
fn parse_expression_list<'a>(mut state: ParseState<'a>) -> (ParseState<'a>, Vec<Expression<'a>>) {
    let mut expressions = Vec::new();

    loop {
        match parse_expression(state) {
            Ok((next_state, expression)) => {
                expressions.push(expression);
                state = next_state;
            },
            Err(_) => break,
        }

        match eat_simple(state, TokenKind::Operator(",")) {
            Ok((next_state, _)) => {
                state = next_state;
            },
            Err(_) => break,
        }
    }

    (state, expressions)
}