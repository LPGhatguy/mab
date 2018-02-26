use std::mem;

use tokenizer::{Token, TokenKind};
use ast::*;

type ParseResult<'a, T> = Result<(ParseState<'a>, T), ParseState<'a>>;

#[derive(Debug, Clone)]
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

    pub fn advance(&mut self, amount: usize) {
        self.position += amount;
    }
}

fn eat_simple<'a>(state: &mut ParseState<'a>, eat_token: TokenKind) -> bool {
    match state.peek() {
        Some(token) => {
            if token.kind == eat_token {
                state.advance(1);
                true
            } else {
                false
            }
        },
        None => false,
    }
}

fn parse_number_literal<'a>(state: &mut ParseState<'a>) -> Option<NumberLiteral<'a>> {
    match state.peek() {
        Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => {
            state.advance(1);
            Some(NumberLiteral {
                value,
            })
        },
        _ => None,
    }
}

fn parse_identifier<'a>(state: &mut ParseState<'a>) -> Option<&'a str> {
    match state.peek() {
        Some(&Token { kind: TokenKind::Identifier(name), .. }) => {
            state.advance(1);
            Some(name)
        },
        _ => None,
    }
}

pub fn parse_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Option<Chunk<'a>> {
    let mut state = ParseState::new(tokens);

    let chunk = match parse_chunk(&mut state) {
        Some(chunk) => chunk,
        None => return None,
    };

    match state.peek() {
        Some(_) => return None,
        None => {},
    }

    Some(chunk)
}

// chunk ::= {stat [`;´]} [laststat [`;´]]
fn parse_chunk<'a>(state: &mut ParseState<'a>) -> Option<Chunk<'a>> {
    let mut statements = Vec::new();

    loop {
        match parse_statement(state) {
            Some(statement) => statements.push(statement),
            None => break,
        }
    }

    let chunk = Chunk {
        statements,
    };

    Some(chunk)
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
fn parse_statement<'a>(state: &mut ParseState<'a>) -> Option<Statement<'a>> {
    {
        let mut next_state = state.clone();

        match parse_local_assignment(&mut next_state) {
            Some(assignment) => {
                mem::replace(state, next_state);
                return Some(Statement::LocalAssignment(assignment));
            },
            None => {},
        }
    }

    // TODO: function call

    None
}

// local namelist [`=´ explist]
// right now:
// local Name `=` exp
fn parse_local_assignment<'a>(state: &mut ParseState<'a>) -> Option<LocalAssignment<'a>> {
    let old_state = state;
    let mut state = old_state.clone();

    if !eat_simple(&mut state, TokenKind::Keyword("local")) {
        return None;
    }

    let name = match parse_identifier(&mut state) {
        Some(name) => name,
        None => return None,
    };

    if !eat_simple(&mut state, TokenKind::Operator("=")) {
        return None;
    }

    let expression = match parse_expression(&mut state) {
        Some(expression) => expression,
        None => return None,
    };

    mem::replace(old_state, state);

    Some(LocalAssignment {
        name,
        value: expression,
    })
}

// // functioncall ::= prefixexp args | prefixexp `:´ Name args
// // right now:
// // functioncall ::= Name `(` explist `)`
// fn parse_function_call<'a>(state: ParseState<'a>) -> ParseResult<'a, FunctionCall<'a>> {
//     let (state, name) = parse_identifier(state)?;

//     let (state, _) = eat_simple(state, TokenKind::OpenParen)?;

//     let (state, expressions) = parse_expression_list(state);

//     let (state, _) = eat_simple(state, TokenKind::CloseParen)?;

//     Ok((state, FunctionCall {
//         name_expression: Box::new(Expression::Name(name)),
//         arguments: expressions,
//     }))
// }

// exp ::= nil | false | true | Number | String | `...´ | function |
//     prefixexp | tableconstructor | exp binop exp | unop exp
// prefixexp ::= var | functioncall | `(´ exp `)´
// var ::=  Name | prefixexp `[´ exp `]´ | prefixexp `.´ Name
fn parse_expression<'a>(state: &mut ParseState<'a>) -> Option<Expression<'a>> {
    {
        let mut next_state = state.clone();

        match parse_number_literal(&mut next_state) {
            Some(literal) => {
                mem::replace(state, next_state);
                return Some(Expression::NumberLiteral(literal));
            },
            None => {},
        }
    }

    // TODO: function call

    None
}

// // explist ::= {exp `,´} exp
// fn parse_expression_list<'a>(mut state: ParseState<'a>) -> (ParseState<'a>, Vec<Expression<'a>>) {
//     let mut expressions = Vec::new();

//     loop {
//         match parse_expression(state) {
//             Ok((next_state, expression)) => {
//                 expressions.push(expression);
//                 state = next_state;
//             },
//             Err(next_state) => {
//                 state = next_state;
//                 break;
//             },
//         }

//         match eat_simple(state, TokenKind::Operator(",")) {
//             Ok((next_state, _)) => {
//                 state = next_state;
//             },
//             Err(next_state) => {
//                 state = next_state;
//                 break;
//             },
//         }
//     }

//     (state, expressions)
// }