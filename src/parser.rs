use tokenizer::{Token, TokenKind};
use ast::*;

type ParseResult<'a, T> = Result<(ParseState<'a>, T), ParseState<'a>>;

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
                Err(state)
            }
        },
        None => Err(state),
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
    parse_local_assignment(state)
        .and_then(|(state, assignment)| Ok((state, Statement::LocalAssignment(assignment))))
        .or_else(|state| parse_function_call(state)
            .and_then(|(state, call)| Ok((state, Statement::FunctionCall(call))))
        )
}

// local namelist [`=´ explist]
// right now:
// local Name `=` exp
fn parse_local_assignment<'a>(state: ParseState<'a>) -> ParseResult<'a, LocalAssignment<'a>> {
    let old_state = state;

    let (state, _) = eat_simple(state, TokenKind::Keyword("local"))
        .map_err(|_| old_state)?;

    let (state, name) = parse_identifier(state)
        .map_err(|_| old_state)?;

    let (state, _) = eat_simple(state, TokenKind::Operator("="))
        .map_err(|_| old_state)?;

    let (state, expression) = parse_expression(state)
        .map_err(|_| old_state)?;

    Ok((state, LocalAssignment {
        name,
        value: expression,
    }))
}

// functioncall ::= prefixexp args | prefixexp `:´ Name args
// right now:
// functioncall ::= Name `(` explist `)`
fn parse_function_call<'a>(state: ParseState<'a>) -> ParseResult<'a, FunctionCall<'a>> {
    let old_state = state;

    let (state, name) = parse_identifier(state)
        .map_err(|_| old_state)?;

    let (state, _) = eat_simple(state, TokenKind::OpenParen)
        .map_err(|_| old_state)?;

    let (state, expressions) = parse_expression_list(state);

    let (state, _) = eat_simple(state, TokenKind::CloseParen)
        .map_err(|_| old_state)?;

    Ok((state, FunctionCall {
        name_expression: Box::new(Expression::Name(name)),
        arguments: expressions,
    }))
}

// exp ::= nil | false | true | Number | String | `...´ | function |
//     prefixexp | tableconstructor | exp binop exp | unop exp
// prefixexp ::= var | functioncall | `(´ exp `)´
// var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name
fn parse_expression<'a>(state: ParseState<'a>) -> ParseResult<'a, Expression<'a>> {
    parse_number_literal(state)
        .and_then(|(state, literal)| Ok((state, Expression::NumberLiteral(literal))))
        .or_else(|state| parse_function_call(state)
            .and_then(|(state, call)| Ok((state, Expression::FunctionCall(call))))
        )
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
            Err(next_state) => {
                state = next_state;
                break;
            },
        }

        match eat_simple(state, TokenKind::Operator(",")) {
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