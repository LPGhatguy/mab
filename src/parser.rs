use std::borrow::Cow;

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

struct ParseToken<'a>(pub TokenKind<'a>);

define_parser!(ParseToken<'state>, &'state Token<'state>, |this: &ParseToken<'state>, state: ParseState<'state>| {
    match state.peek() {
        Some(token) => {
            if token.kind == this.0 {
                Ok((state.advance(1), token))
            } else {
                Err(ParseAbort::NoMatch)
            }
        },
        None => Err(ParseAbort::NoMatch),
    }
});

struct ParseNumber;
define_parser!(ParseNumber, Cow<'state, str>, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::NumberLiteral(ref value), .. }) => Ok((state.advance(1), Cow::from(value.as_ref()))),
        _ => Err(ParseAbort::NoMatch),
    }
});

struct ParseIdentifier;
define_parser!(ParseIdentifier, Cow<'state, str>, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::Identifier(ref name), .. }) => Ok((state.advance(1), Cow::from(name.as_ref()))),
        _ => Err(ParseAbort::NoMatch),
    }
});

struct ParseKeyword(pub &'static str);
define_parser!(ParseKeyword, (), |this: &ParseKeyword, state: ParseState<'state>| {
    let (state, _) = ParseToken(TokenKind::Keyword(this.0.into())).parse(state)?;

    Ok((state, ()))
});

struct ParseOperator(pub &'static str);
define_parser!(ParseOperator, (), |this: &ParseOperator, state: ParseState<'state>| {
    let (state, _) = ParseToken(TokenKind::Operator(this.0.into())).parse(state)?;

    Ok((state, ()))
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
        ParseIfStatement => Statement::IfStatement,
        ParseWhileLoop => Statement::WhileLoop,
        ParseRepeatLoop => Statement::RepeatLoop,
        ParseFunctionDeclaration => Statement::FunctionDeclaration,
    })
});

// exp ::= unop exp | value [binop exp]
struct ParseExpression;
define_parser!(ParseExpression, Expression<'state>, |_, state| {
    ParseValue.parse(state)
});

struct ParseValue;
define_parser!(ParseValue, Expression<'state>, |_, state| {
    parse_first_of!(state, {
        ParseNumber => Expression::Number,
        ParseFunctionCall => Expression::FunctionCall,
        ParseIdentifier => Expression::Name,
        ParseTableLiteral => Expression::Table,
    })
});

// local namelist [`=´ explist]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment<'state>, |_, state| {
    let (state, _) = ParseKeyword("local").parse(state)?;

    let (state, names) = DelimitedOneOrMore(ParseIdentifier, ParseOperator(",")).parse(state)?;

    let (state, expressions) = match ParseOperator("=").parse(state) {
        Ok((state, _)) => DelimitedOneOrMore(ParseExpression, ParseOperator(",")).parse(state)?,
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
    let (state, _) = ParseToken(TokenKind::OpenParen).parse(state)?;
    let (state, expressions) = DelimitedZeroOrMore(ParseExpression, ParseOperator(","), false).parse(state)?;
    let (state, _) = ParseToken(TokenKind::CloseParen).parse(state)?;

    Ok((state, FunctionCall {
        name_expression: Box::new(Expression::Name(name)),
        arguments: expressions,
    }))
});

struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'state>, |_, state| {
    let (state, _) = ParseKeyword("for").parse(state)?;
    let (state, var) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseOperator("=").parse(state)?;
    let (state, start) = ParseExpression.parse(state)?;
    let (state, _) = ParseOperator(",").parse(state)?;
    let (state, end) = ParseExpression.parse(state)?;

    let (state, step) = match state.peek() {
        Some(&Token { kind: TokenKind::Operator(ref operator), .. }) => {
            if operator == "," {
                let (new_state, parsed_step) = ParseExpression.parse(state.advance(1))?;

                (new_state, Some(parsed_step))
            } else {
                return Err(ParseAbort::NoMatch);
            }
        },
        Some(&Token { kind: TokenKind::Keyword(ref keyword), .. }) => {
            if keyword == "do" {
                (state, None)
            } else {
                return Err(ParseAbort::NoMatch);
            }
        },
        _ => return Err(ParseAbort::NoMatch),
    };

    let (state, _) = ParseKeyword("do").parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseKeyword("end").parse(state)?;

    Ok((state, NumericFor {
        var,
        start,
        end,
        step,
        body,
    }))
});

struct ParseIfStatement;
define_parser!(ParseIfStatement, IfStatement<'state>, |_, state| {
    let (state, _) = ParseKeyword("if").parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;
    let (state, _) = ParseKeyword("then").parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;

    let mut state = state;
    let mut else_if_branches = Vec::new();
    loop {
        let (next_state, _) = match ParseKeyword("elseif").parse(state) {
            Ok(v) => v,
            Err(_) => break,
        };

        let (next_state, condition) = ParseExpression.parse(next_state)?;
        let (next_state, _) = ParseKeyword("then").parse(next_state)?;
        let (next_state, body) = ParseChunk.parse(next_state)?;

        state = next_state;
        else_if_branches.push((condition, body));
    }

    let (state, else_branch) = match ParseKeyword("else").parse(state) {
        Ok((state, _)) => {
            let (state, body) = ParseChunk.parse(state)?;

            (state, Some(body))
        },
        Err(_) => (state, None),
    };

    let (state, _) = ParseKeyword("end").parse(state)?;

    Ok((state, IfStatement {
        condition,
        body,
        else_if_branches,
        else_branch,
    }))
});

struct ParseWhileLoop;
define_parser!(ParseWhileLoop, WhileLoop<'state>, |_, state| {
    let (state, _) = ParseKeyword("while").parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;
    let (state, _) = ParseKeyword("do").parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseKeyword("end").parse(state)?;

    Ok((state, WhileLoop {
        condition,
        body,
    }))
});

struct ParseRepeatLoop;
define_parser!(ParseRepeatLoop, RepeatLoop<'state>, |_, state| {
    let (state, _) = ParseKeyword("repeat").parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseKeyword("until").parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;

    Ok((state, RepeatLoop {
        condition,
        body,
    }))
});

struct ParseFunctionDeclaration;
define_parser!(ParseFunctionDeclaration, FunctionDeclaration<'state>, |_, state| {
    let (state, local) = Optional(ParseKeyword("local")).parse(state)
        .map(|(state, value)| (state, value.is_some()))?;

    let (state, _) = ParseKeyword("function").parse(state)?;
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseToken(TokenKind::OpenParen).parse(state)?;
    let (state, parameters) = DelimitedZeroOrMore(ParseIdentifier, ParseOperator(","), false).parse(state)?;
    let (state, _) = ParseToken(TokenKind::CloseParen).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseKeyword("end").parse(state)?;

    Ok((state, FunctionDeclaration {
        local,
        name,
        parameters,
        body,
    }))
});

struct ParseTableKey;
define_parser!(ParseTableKey, TableKey<'state>, |_, state| {
    // First, try parsing an identifier (Lua allows bare literals as table keys)
    let (state, key) = match ParseIdentifier.parse(state) {
        Ok((state, identifier)) => (state, TableKey::Name(identifier.into())),
        Err(ParseAbort::NoMatch) => {
            let (state, _) = ParseOperator("[").parse(state)?;
            let (state, key) = ParseExpression.parse(state)?;
            let (state, _) = ParseOperator("]").parse(state)?;

            (state, TableKey::Expression(key))
        },
        Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message)),
    };

    Ok((state, key))
});

struct ParseTableValue;
define_parser!(ParseTableValue, (Option<TableKey<'state>>, Expression<'state>), |_, state| {
    let (state, key) = Optional(ParseTableKey).parse(state)?;

    // We only check for '=' if there was a key
    let state = match key {
        Some(_) => ParseOperator("=").parse(state)?.0,
        None => state
    };

    let (state, value) = ParseExpression.parse(state)?;
    Ok((state, (key, value)))
});

struct ParseTableLiteral;
define_parser!(ParseTableLiteral, TableLiteral<'state>, |_, state| {
    let (state, _) = ParseOperator("{").parse(state)?;
    let (state, items) = DelimitedZeroOrMore(ParseTableValue, Or(vec![ ParseOperator(","), ParseOperator(";") ]), true).parse(state)?;
    let (state, _) = ParseOperator("}").parse(state)?;
    Ok((state, TableLiteral {
        items
    }))
});