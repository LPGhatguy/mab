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

struct ParseKeyword(pub &'static str);
define_parser!(ParseKeyword, (), |this: &ParseKeyword, state: ParseState<'state>| {
    let (state, _) = ParseToken(TokenKind::Keyword(this.0)).parse(state)?;

    Ok((state, ()))
});

struct ParseOperator(pub &'static str);
define_parser!(ParseOperator, (), |this: &ParseOperator, state: ParseState<'state>| {
    let (state, _) = ParseToken(TokenKind::Operator(this.0)).parse(state)?;

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
        ParseWhileLoop => Statement::WhileLoop,
        ParseRepeatLoop => Statement::RepeatLoop,
        ParseFunctionDeclaration => Statement::FunctionDeclaration,
    })
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
    let (state, expressions) = DelimitedZeroOrMore(ParseExpression, ParseOperator(",")).parse(state)?;
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
        Some(&Token { kind: TokenKind::Operator(","), .. }) => {
            let (new_state, parsed_step) = ParseExpression.parse(state.advance(1))?;

            (new_state, Some(parsed_step))
        },
        Some(&Token { kind: TokenKind::Keyword("do"), .. }) => (state, None),
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
    let (state, parameters) = DelimitedZeroOrMore(ParseIdentifier, ParseOperator(",")).parse(state)?;
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

#[cfg(test)]
mod test {
    use super::*;
    use tokenizer::TokenKind::*;

    macro_rules! token {
        ($kind:expr) => (
            Token {
                line: 1,
                column: 1,
                whitespace: "",
                kind: $kind,
            }
        )
    }

    #[test]
    fn parse_function_call() {
        let tokens = [
            token!(Identifier("print")),
            token!(OpenParen),
            token!(Identifier("i")),
            token!(CloseParen),
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
    fn parse_function_decl() {
        let tokens = [
            token!(Keyword("local")),
            token!(Keyword("function")),
            token!(Identifier("test")),
            token!(OpenParen),
            token!(Identifier("a")),
            token!(Operator(",")),
            token!(Identifier("b")),
            token!(CloseParen),
            token!(Keyword("end")),
        ];

        let parse_state = ParseState::new(&tokens);
        let (_, parsed_decl) = ParseFunctionDeclaration.parse(parse_state).unwrap();
        assert_eq!(parsed_decl, FunctionDeclaration {
            local: true,
            name: "test",
            parameters: vec![ "a", "b" ],
            body: Chunk {
                statements: vec![],
            },
        });

        let tokens = [
            token!(Keyword("function")),
            token!(Identifier("test")),
            token!(OpenParen),
            token!(CloseParen),
            token!(Identifier("print")),
            token!(OpenParen),
            token!(NumberLiteral("1")),
            token!(CloseParen),
            token!(Keyword("end")),
        ];

        let parse_state = ParseState::new(&tokens);
        let (_, parsed_decl) = ParseFunctionDeclaration.parse(parse_state).unwrap();
        assert_eq!(parsed_decl, FunctionDeclaration {
            local: false,
            name: "test",
            parameters: vec![],
            body: Chunk {
                statements: vec![
                    Statement::FunctionCall(
                        FunctionCall {
                            name_expression: Box::new(Expression::Name("print")),
                            arguments: vec![
                                Expression::Number("1"),
                            ],
                        }
                    ),
                ],
            },
        });
    }

    #[test]
    fn parse_for_loop() {
        let data = [
            token!(Keyword("for")),
            token!(Identifier("i")),
            token!(Operator("=")),
            token!(NumberLiteral("1")),
            token!(Operator(",")),
            token!(NumberLiteral("10")),
            token!(Operator(",")),
            token!(NumberLiteral("2")),
            token!(Keyword("do")),
            token!(Identifier("print")),
            token!(OpenParen),
            token!(Identifier("i")),
            token!(CloseParen),
            token!(Keyword("end")),
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
            _ => panic!("Incorrect statement kind {:?}", statement)
        };
    }
}