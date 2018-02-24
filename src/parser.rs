use lexer::{Token, TokenKind};

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

/*
#[derive(Debug, Clone)]
pub struct NumberLiteral<'a> {
    value: &'a str,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator<'a> {
    left: Expression<'a>,
    right: Expression<'a>,
    operator: &'a str,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    NumberLiteral(NumberLiteral<'a>),
    BinaryOperator(Box<BinaryOperator<'a>>),
}

#[derive(Debug, Clone)]
pub struct LocalAssignment<'a> {
    name: &'a str,
    value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub struct FunctionCall<'a> {
    name: &'a str,
    arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    LocalAssignment(LocalAssignment<'a>),
}

#[derive(Debug, Clone)]
pub struct Chunk<'a> {
    statements: Vec<Statement<'a>>,
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Option<Chunk<'a>> {
    let state = ParseState::new(tokens);

    let chunk = match parse_chunk(state) {
        Ok((_, chunk)) => chunk,
        Err(_) => return None,
    };

    Some(chunk)
}

fn eat_whitespace<'a>(state: ParseState<'a>) -> ParseState<'a> {
    match state.peek() {
        Some(&Token::Whitespace(_)) => state.advance(1),
        _ => state,
    }
}

fn parse_identifier<'a>(state: ParseState<'a>) -> ParseResult<'a, &'a str> {
    match state.peek() {
        Some(&Token::Identifier(name)) => (state.advance(1), name),
        _ => return Err(state),
    }
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

fn parse_statement<'a>(state: ParseState<'a>) -> ParseResult<'a, Statement<'a>> {
    let (state, assignment) = parse_local_assignment(state)?;

    Ok((state, Statement::LocalAssignment(assignment)))
}

fn parse_local_assignment<'a>(state: ParseState<'a>) -> ParseResult<'a, LocalAssignment<'a>> {
    let (state, _) = state.eat_simple(Token::Keyword("local"))?;
    let state = eat_whitespace(state);

    let (state, name) = parse_identifier(state)?;
    let state = eat_whitespace(state);

    let (state, _) = state.eat_simple(Token::Operator("="))?;
    let state = eat_whitespace(state);

    let (state, expression) = parse_expression(state)?;

    Ok((state, LocalAssignment {
        name,
        value: expression,
    }))
}

fn parse_number_literal<'a>(state: ParseState<'a>) -> ParseResult<'a, NumberLiteral<'a>> {
    let (state, value) = {
        match state.peek() {
            Some(&Token::NumberLiteral(value)) => (state.advance(1), value),
            _ => return Err(state),
        }
    };
    let state = eat_whitespace(state);

    Ok((state, NumberLiteral {
        value,
    }))
}

fn parse_function_call<'a>(state: ParseState<'a>) -> ParseResult<'a, FunctionCall<'a>> {
    let (state, name) = parse_identifier(state)?;
    let state = eat_whitespace(state);

    let (state, _) = state.eat_simple(Token::OpenParen)?;
    let state = eat_whitespace(state);

    let (state, expressions) = parse_expression_list(state);
    let state = eat_whitespace(state);

    let (state, _) = state.eat_simple(Token::CloseParen)?;
    let state = eat_whitespace(state);

    Ok((state, FunctionCall {
        name,
        arguments: expressions,
    }))
}

fn parse_expression<'a>(state: ParseState<'a>) -> ParseResult<'a, Expression<'a>> {
    let (state, literal) = parse_number_literal(state)?;

    Ok((state, Expression::NumberLiteral(literal)))
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
    }

    (state, expressions)
}
*/