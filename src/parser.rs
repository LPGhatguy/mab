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
        match ParseLocalAssignment.parse(state) {
            Some((state, assignment)) => return Some((state, Statement::LocalAssignment(assignment))),
            None => {},
        }

        match ParseFunctionCall.parse(state) {
            Some((state, call)) => return Some((state, Statement::FunctionCall(call))),
            None => {},
        }

        match ParseNumericFor.parse(state) {
            Some((state, numeric_for)) => return Some((state, Statement::NumericFor(numeric_for))),
            None => {},
        }

        None
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
        match ParseNumber.parse(state) {
            Some((state, value)) => return Some((state, Expression::Number(value))),
            None => {},
        }

        match ParseFunctionCall.parse(state) {
            Some((state, call)) => return Some((state, Expression::FunctionCall(call))),
            None => {},
        }

        match ParseIdentifier.parse(state) {
            Some((state, name)) => return Some((state, Expression::Name(name))),
            None => {},
        }

        None
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

struct ParseNumericFor;

impl<'a> Parser<'a, NumericFor<'a>> for ParseNumericFor {
    fn parse(&self, state: ParseState<'a>) -> Option<(ParseState<'a>, NumericFor<'a>)> {
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
            }
            Some(&Token { kind: TokenKind::Keyword("do"), .. }) => {},
            _ => return None,
        }

        let (state, _) = EatToken { kind: TokenKind::Keyword("do") }.parse(state)?;
        let (state, body) = ParseChunk.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::Keyword("end") }.parse(state)?;

        Some((state, NumericFor {
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