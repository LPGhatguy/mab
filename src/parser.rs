use std::borrow::Cow;

use tokenizer::{Token, TokenKind};
use ast::*;

pub fn parse_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Option<Chunk<'a>> {
    let state = ParseState::new(tokens);

    let (state, chunk) = match ParseChunk.parse(state) {
        Some(result) => result,
        None => return None,
    };

    match state.peek() {
        Some(_) => return None,
        None => {},
    }

    Some(chunk)
}

macro_rules! parse_first_of {
    ($state: ident, { $( $parser: path => $constructor: path ),* $(,)* }) => (
        {
            $(
                match $parser.parse($state) {
                    Some((state, value)) => return Some((state, $constructor(value))),
                    None => {},
                }
            )*

            None
        }
    );
}

enum ParseAbort<'a> {
    /// Indicates that the parser was unable to match the input, but that it was
    /// not necessarily an error.
    NoMatch,

    /// Indicates that the parser was unable to match the input and hit the
    /// error described by the returned string.
    Error(Cow<'a, str>)
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

trait Parser<'a, T: 'a> {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, T)>;
}

struct EatToken<'a> {
    pub kind: TokenKind<'a>,
}

impl<'a> Parser<'a, &'a Token<'a>> for EatToken<'a> {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, &'a Token<'a>)> {
        match state.peek() {
            Some(token) => {
                if token.kind == self.kind {
                    Some((state.advance(1), token))
                } else {
                    None
                }
            },
            None => None,
        }
    }
}

struct ParseNumber;

impl<'a> Parser<'a, &'a str> for ParseNumber {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, &'a str)> {
        match state.peek() {
            Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => Some((state.advance(1), value)),
            _ => None,
        }
    }
}

struct ParseIdentifier;

impl<'a> Parser<'a, &'a str> for ParseIdentifier {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, &'a str)> {
        match state.peek() {
            Some(&Token { kind: TokenKind::Identifier(name), .. }) => Some((state.advance(1), name)),
            _ => None,
        }
    }
}

// chunk ::= {stat [`;´]} [laststat [`;´]]
struct ParseChunk;

impl<'a> Parser<'a, Chunk<'a>> for ParseChunk {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, Chunk<'a>)> {
        let mut statements = Vec::new();
        let mut state = state;

        loop {
            state = match ParseStatement.parse(state) {
                Some((next_state, statement)) => {
                    statements.push(statement);
                    next_state
                },
                None => break,
            };
        }

        let chunk = Chunk {
            statements,
        };

        Some((state, chunk))
    }
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
struct ParseStatement;

impl<'a> Parser<'a, Statement<'a>> for ParseStatement {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, Statement<'a>)> {
        parse_first_of!(state, {
            ParseLocalAssignment => Statement::LocalAssignment,
            ParseFunctionCall => Statement::FunctionCall,
        })
    }
}

// local namelist [`=´ explist]
// right now:
// local Name `=` exp
struct ParseLocalAssignment;

impl<'a> Parser<'a, LocalAssignment<'a>> for ParseLocalAssignment {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, LocalAssignment<'a>)> {
        let (state, _) = EatToken { kind: TokenKind::Keyword("local") }.parse(state)?;
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::Operator("=") }.parse(state)?;
        let (state, expression) = ParseExpression.parse(state)?;

        Some((state, LocalAssignment {
            name,
            value: expression,
        }))
    }
}

// functioncall ::= prefixexp args | prefixexp `:´ Name args
// right now:
// functioncall ::= Name `(` explist `)`
struct ParseFunctionCall;

impl<'a> Parser<'a, FunctionCall<'a>> for ParseFunctionCall {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, FunctionCall<'a>)> {
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::OpenParen }.parse(state)?;
        let (state, expressions) = ParseExpressionList.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::CloseParen }.parse(state)?;

        Some((state, FunctionCall {
            name_expression: Box::new(Expression::Name(name)),
            arguments: expressions,
        }))
    }
}

// exp ::= unop exp | value [binop exp]
struct ParseExpression;

impl<'a> Parser<'a, Expression<'a>> for ParseExpression {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, Expression<'a>)> {
        parse_first_of!(state, {
            ParseNumber => Expression::Number,
            ParseFunctionCall => Expression::FunctionCall,
        })
    }
}

// explist ::= {exp `,´} exp
struct ParseExpressionList;

impl<'a> Parser<'a, Vec<Expression<'a>>> for ParseExpressionList {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, Vec<Expression<'a>>)> {
        let mut state = state;
        let mut expressions = Vec::new();

        loop {
            match ParseExpression.parse(state) {
                Some((next_state, expression)) => {
                    expressions.push(expression);
                    state = next_state;
                },
                None => break,
            }

            match (EatToken { kind: TokenKind::Operator(",") }.parse(state)) {
                Some((next_state, _)) => {
                    state = next_state;
                },
                None => break,
            }
        }

        Some((state, expressions))
    }
}