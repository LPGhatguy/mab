use std::borrow::Cow;
use std::fmt;

use tokenizer::{Token, TokenKind};
use ast::*;
use parser_core::*;

pub fn parse_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Result<Chunk<'a>, String> {
    let state = ParseState::new(tokens);

    let (state, chunk) = match ParseChunk.parse(state) {
        Ok(result) => result,
        Err(ParseAbort::NoMatch) => return Err("No error reported".to_string()),
        Err(ParseAbort::Error(message)) => return Err(message),
    };

    match state.peek() {
        Some(token) => return Err(format!("A token was left at the end of the stream: {:?}", token)),
        None => {},
    }

    Ok(chunk)
}

struct ParseToken<'a> {
    pub kind: TokenKind<'a>,
}

define_parser!(ParseToken<'state>, &'state Token<'state>, |this: &ParseToken<'state>, state: ParseState<'state>| {
    match state.peek() {
        Some(token) => {
            if token.kind == this.kind {
                Ok((state.advance(1), token))
            } else {
                Err(ParseAbort::NoMatch)
            }
        },
        None => Err(ParseAbort::NoMatch),
    }
});

struct ParseNumber;
define_parser!(ParseNumber, &'state str, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => Ok((state.advance(1), value)),
        _ => Err(ParseAbort::NoMatch),
    }
});

struct ParseIdentifier;
define_parser!(ParseIdentifier, &'state str, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::Identifier(name), .. }) => Ok((state.advance(1), name)),
        _ => Err(ParseAbort::NoMatch),
    }
});

// chunk ::= {stat [`;´]} [laststat [`;´]]
struct ParseChunk;
define_parser!(ParseChunk, Chunk<'state>, |_, state| {
    let (state, statements) = ZeroOrMore(ParseStatement).parse(state)?;

    Ok((state, Chunk {
        statements,
    }))
});

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
define_parser!(ParseStatement, Statement<'state>, |_, state| {
    parse_first_of!(state, {
        ParseLocalAssignment => Statement::LocalAssignment,
        ParseFunctionCall => Statement::FunctionCall,
        ParseNumericFor => Statement::NumericFor,
    })
});

// local namelist [`=´ explist]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment<'state>, |_, state| {
    let (state, _) = ParseToken { kind: TokenKind::Keyword("local") }.parse(state)?;

    let (state, names) = DelimitedOneOrMore {
        item_parser: ParseIdentifier,
        delimiter_parser: ParseToken { kind: TokenKind::Operator(",") },
    }.parse(state)?;

    let (state, expressions) = match (ParseToken { kind: TokenKind::Operator("=") }.parse(state)) {
        Ok((state, _)) => {
            DelimitedOneOrMore {
                item_parser: ParseExpression,
                delimiter_parser: ParseToken { kind: TokenKind::Operator(",") },
            }.parse(state)?
        },
        Err(_) => (state, Vec::new()),
    };

    Ok((state, LocalAssignment {
        names: names,
        values: expressions,
    }))
});

// functioncall ::= prefixexp args | prefixexp `:´ Name args
// right now:
// functioncall ::= Name `(` explist `)`
struct ParseFunctionCall;
define_parser!(ParseFunctionCall, FunctionCall<'state>, |_, state| {
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseToken { kind: TokenKind::OpenParen }.parse(state)?;
    let (state, expressions) = DelimitedZeroOrMore {
        item_parser: ParseExpression,
        delimiter_parser: ParseToken { kind: TokenKind::Operator(",") },
    }.parse(state)?;
    let (state, _) = ParseToken { kind: TokenKind::CloseParen }.parse(state)?;

    Ok((state, FunctionCall {
        name_expression: Box::new(Expression::Name(name)),
        arguments: expressions,
    }))
});

// exp ::= unop exp | value [binop exp]
struct ParseExpression;
define_parser!(ParseExpression, Expression<'state>, |_, state| {
    parse_first_of!(state, {
        ParseNumber => Expression::Number,
        ParseFunctionCall => Expression::FunctionCall,
        ParseIdentifier => Expression::Name,
    })
});

struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'state>, |_, state| {
    let (state, _) = ParseToken { kind: TokenKind::Keyword("for") }.parse(state)?;
    let (state, var) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseToken { kind: TokenKind::Operator("=") }.parse(state)?;
    let (state, start) = ParseExpression.parse(state)?;
    let (state, _) = ParseToken { kind: TokenKind::Operator(",") }.parse(state)?;
    let (state, end) = ParseExpression.parse(state)?;
    let mut step = None;
    let mut state = state;

    match state.peek() {
        Some(&Token { kind: TokenKind::Operator(","), .. }) => {
            let (new_state, parsed_step) = ParseExpression.parse(state.advance(1))?;
            state = new_state;
            step = Some(parsed_step);
        },
        Some(&Token { kind: TokenKind::Keyword("do"), .. }) => {},
        _ => return Err(ParseAbort::NoMatch),
    }

    let (state, _) = ParseToken { kind: TokenKind::Keyword("do") }.parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseToken { kind: TokenKind::Keyword("end") }.parse(state)?;

    Ok((state, NumericFor {
        var, start, end, step, body,
    }))
});

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_function_call() {
        let tokens = [
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Identifier("print"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::OpenParen,
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Identifier("i"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::CloseParen,
                whitespace: "",
            },
        ];

        let state = ParseState::new(&tokens);
        let (state, function_call) = ParseFunctionCall.parse(state).unwrap();
        assert_eq!(function_call, FunctionCall {
            name_expression: Box::new(Expression::Name("print")),
            arguments: vec![
                Expression::Name("i"),
            ]
        })
    }

    #[test]
    fn parse_for_loop() {
        let data = [
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Keyword("for"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Identifier("i"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Operator("="),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::NumberLiteral("1"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Operator(","),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::NumberLiteral("10"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Operator(","),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::NumberLiteral("2"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Keyword("do"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Identifier("print"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::OpenParen,
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Identifier("i"),
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::CloseParen,
                whitespace: "",
            },
            Token {
                line: 1,
                column: 1,
                kind: TokenKind::Keyword("end"),
                whitespace: "",
            },
        ];

        let parsed_chunk = parse_from_tokens(&data).unwrap();
        let statement = &parsed_chunk.statements[0];

        match statement {
            &Statement::NumericFor(ref numeric_for) => {
                assert_eq!(numeric_for.var, "i");
                assert_eq!(numeric_for.start, Expression::Number("1"));
                assert_eq!(numeric_for.end, Expression::Number("10"));
                assert_eq!(numeric_for.step, Some(Expression::Number("2")));
                assert_eq!(numeric_for.body, Chunk {
                    statements: vec![
                        Statement::FunctionCall(
                            FunctionCall {
                                name_expression: Box::new(Expression::Name("print")),
                                arguments: vec![
                                    Expression::Name("i")
                                ]
                            }
                        )
                    ]
                })
            },
            _ => panic!("Incorrect statement kind {:?}, statement")
        };
    }
}