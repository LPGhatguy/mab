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

macro_rules! parse_first_of {
    ($state: ident, { $( $parser: path => $constructor: path ),* $(,)* }) => (
        {
            $(
                match $parser.parse($state) {
                    Ok((state, value)) => return Ok((state, $constructor(value))),
                    Err(ParseAbort::NoMatch) => {},
                    Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message)),
                }
            )*

            Err(ParseAbort::NoMatch)
        }
    );
}

struct EatToken<'a> {
    pub kind: TokenKind<'a>,
}

impl<'a> Parser<'a> for EatToken<'a> {
    type Item = &'a Token<'a>;

    fn item_name(&self) -> String {
        format!("token {:?}", self.kind)
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        match state.peek() {
            Some(token) => {
                if token.kind == self.kind {
                    Ok((state.advance(1), token))
                } else {
                    Err(ParseAbort::NoMatch)
                }
            },
            None => Err(ParseAbort::NoMatch),
        }
    }
}

struct ParseNumber;

impl<'a> Parser<'a> for ParseNumber {
    type Item = &'a str;

    fn item_name(&self) -> String {
        "number".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        match state.peek() {
            Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => Ok((state.advance(1), value)),
            _ => Err(ParseAbort::NoMatch),
        }
    }
}

struct ParseIdentifier;

impl<'a> Parser<'a> for ParseIdentifier {
    type Item = &'a str;

    fn item_name(&self) -> String {
        "identifier".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        match state.peek() {
            Some(&Token { kind: TokenKind::Identifier(name), .. }) => Ok((state.advance(1), name)),
            _ => Err(ParseAbort::NoMatch),
        }
    }
}

macro_rules! simple_parser {
    ($name: ident, $result_type: path, $body: expr) => {
        struct $name;

        impl<'a> Parser<'a> for ParseChunk {
            type Item = $result_type;

            fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
                $body(state)
            }
        }
    }
}

simple_parser!(ParseChunk, Chunk<'a>, |state| {
    let (state, statements) = ZeroOrMore(ParseStatement).parse(state)?;

    Ok((state, Chunk {
        statements,
    }))
});

// chunk ::= {stat [`;´]} [laststat [`;´]]
// struct ParseChunk;

// impl<'a> Parser<'a> for ParseChunk {
//     type Item = Chunk<'a>;

//     fn item_name(&self) -> String {
//         "chunk".to_string()
//     }

//     fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
//         let (state, statements) = ZeroOrMore(ParseStatement).parse(state)?;

//         Ok((state, Chunk {
//             statements,
//         }))
//     }
// }

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

impl<'a> Parser<'a> for ParseStatement {
    type Item = Statement<'a>;

    fn item_name(&self) -> String {
        "statement".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        parse_first_of!(state, {
            ParseLocalAssignment => Statement::LocalAssignment,
            ParseFunctionCall => Statement::FunctionCall,
            ParseNumericFor => Statement::NumericFor,
        })
    }
}

// local namelist [`=´ explist]
struct ParseLocalAssignment;

impl<'a> Parser<'a> for ParseLocalAssignment {
    type Item = LocalAssignment<'a>;

    fn item_name(&self) -> String {
        "local assignment".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let (state, _) = EatToken { kind: TokenKind::Keyword("local") }.parse(state)?;

        let (state, names) = DelimitedOneOrMore {
            item_parser: ParseIdentifier,
            delimiter_parser: EatToken { kind: TokenKind::Operator(",") },
        }.parse(state)?;

        let (state, expressions) = match (EatToken { kind: TokenKind::Operator("=") }.parse(state)) {
            Ok((state, _)) => {
                DelimitedOneOrMore {
                    item_parser: ParseExpression,
                    delimiter_parser: EatToken { kind: TokenKind::Operator(",") },
                }.parse(state)?
            },
            Err(_) => (state, Vec::new()),
        };

        Ok((state, LocalAssignment {
            names: names,
            values: expressions,
        }))
    }
}

// functioncall ::= prefixexp args | prefixexp `:´ Name args
// right now:
// functioncall ::= Name `(` explist `)`
struct ParseFunctionCall;

impl<'a> Parser<'a> for ParseFunctionCall {
    type Item = FunctionCall<'a>;

    fn item_name(&self) -> String {
        "function call".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::OpenParen }.parse(state)?;
        let (state, expressions) = DelimitedZeroOrMore {
            item_parser: ParseExpression,
            delimiter_parser: EatToken { kind: TokenKind::Operator(",") },
        }.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::CloseParen }.parse(state)?;

        Ok((state, FunctionCall {
            name_expression: Box::new(Expression::Name(name)),
            arguments: expressions,
        }))
    }
}

// exp ::= unop exp | value [binop exp]
struct ParseExpression;

impl<'a> Parser<'a> for ParseExpression {
    type Item = Expression<'a>;

    fn item_name(&self) -> String {
        "expression".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        parse_first_of!(state, {
            ParseNumber => Expression::Number,
            ParseFunctionCall => Expression::FunctionCall,
            ParseIdentifier => Expression::Name,
        })
    }
}

struct ParseNumericFor;

impl<'a> Parser<'a> for ParseNumericFor {
    type Item = NumericFor<'a>;

    fn item_name(&self) -> String {
        "numeric for".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let (state, _) = EatToken { kind: TokenKind::Keyword("for") }.parse(state)?;
        let (state, var) = ParseIdentifier.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::Operator("=") }.parse(state)?;
        let (state, start) = ParseExpression.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::Operator(",") }.parse(state)?;
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

        let (state, _) = EatToken { kind: TokenKind::Keyword("do") }.parse(state)?;
        let (state, body) = ParseChunk.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::Keyword("end") }.parse(state)?;

        Ok((state, NumericFor {
            var, start, end, step, body,
        }))
    }
}

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