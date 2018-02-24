use lexer::{Token, TokenKind};
use ast::*;

type ParseResult<'a, T> = Result<(ParseState<'a>, T), ParseState<'a>>;

#[derive(Debug, Clone)]
pub struct ParseState<'a> {
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

    pub fn eat_simple(self, eat_token: TokenKind) -> ParseResult<'a, &'a Token<'a>> {
        match self.peek() {
            Some(token) => {
                if token.kind == eat_token {
                    Ok((self.advance(1), token))
                } else {
                    Err(self)
                }
            },
            None => Err(self),
        }
    }
}

fn parse_number_literal<'a>(state: ParseState<'a>) -> ParseResult<'a, NumberLiteral<'a>> {
    let (state, value) = {
        match state.peek() {
            Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => (state.advance(1), value),
            _ => return Err(state),
        }
    };

    Ok((state, NumberLiteral {
        value,
    }))
}

fn parse_identifier<'a>(state: ParseState<'a>) -> ParseResult<'a, &'a str> {
    match state.peek() {
        Some(&Token { kind: TokenKind::Identifier(name), .. }) => Ok((state.advance(1), name)),
        _ => Err(state),
    }
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Option<Chunk<'a>> {
    let state = ParseState::new(tokens);

    let chunk = match parse_chunk(state) {
        Ok((_, chunk)) => chunk,
        Err(_) => return None,
    };

    Some(chunk)
}

fn parse_chunk<'a>(state: ParseState<'a>) -> ParseResult<'a, Chunk<'a>> {
    let mut statements = Vec::new();
    let mut state = state;

    loop {
        state = match parse_statement(state) {
            Ok((next_state, statement)) => {
                statements.push(statement);
                next_state
            },
            Err(next_state) => {
                state = next_state;
                break;
            },
        };
    }

    let chunk = Chunk {
        statements,
    };

    Ok((state, chunk))
}

fn parse_statement<'a>(mut state: ParseState<'a>) -> ParseResult<'a, Statement<'a>> {
    match parse_local_assignment(state) {
        Ok((next_state, assignment)) => {
            return Ok((next_state, Statement::LocalAssignment(assignment)));
        },
        Err(next_state) => {
            state = next_state;
        },
    }

    match parse_function_call(state) {
        Ok((next_state, call)) => {
            return Ok((next_state, Statement::FunctionCall(call)));
        },
        Err(next_state) => {
            state = next_state;
        },
    }

    Err(state)
}

fn parse_local_assignment<'a>(state: ParseState<'a>) -> ParseResult<'a, LocalAssignment<'a>> {
    let (state, _) = state.eat_simple(TokenKind::Keyword("local"))?;

    let (state, name) = parse_identifier(state)?;

    let (state, _) = state.eat_simple(TokenKind::Operator("="))?;

    let (state, expression) = parse_expression(state)?;

    Ok((state, LocalAssignment {
        name,
        value: expression,
    }))
}

fn parse_function_call<'a>(state: ParseState<'a>) -> ParseResult<'a, FunctionCall<'a>> {
    let (state, name) = parse_identifier(state)?;

    let (state, _) = state.eat_simple(TokenKind::OpenParen)?;

    let (state, expressions) = parse_expression_list(state);

    let (state, _) = state.eat_simple(TokenKind::CloseParen)?;

    Ok((state, FunctionCall {
        name,
        arguments: expressions,
    }))
}

fn parse_expression<'a>(mut state: ParseState<'a>) -> ParseResult<'a, Expression<'a>> {
    match parse_number_literal(state) {
        Ok((next_state, literal)) => {
            return Ok((next_state, Expression::NumberLiteral(literal)));
        },
        Err(next_state) => {
            state = next_state;
        },
    }

    match parse_function_call(state) {
        Ok((next_state, call)) => {
            return Ok((next_state, Expression::FunctionCall(call)));
        },
        Err(next_state) => {
            state = next_state;
        },
    }

    Err(state)
}

fn parse_expression_list<'a>(mut state: ParseState<'a>) -> (ParseState<'a>, Vec<Expression<'a>>) {
    let mut expressions = Vec::new();

    loop {
        match parse_expression(state) {
            Ok((next_state, expression)) => {
                expressions.push(expression);
                state = next_state;
            },
            Err(next_state) => {
                state = next_state;
                break;
            },
        }

        match state.eat_simple(TokenKind::Operator(",")) {
            Ok((next_state, _)) => {
                state = next_state;
            },
            Err(next_state) => {
                state = next_state;
                break;
            },
        }
    }

    (state, expressions)
}