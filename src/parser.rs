use std::borrow::Cow;

use tokenizer::{Token, TokenKind};
use ast::*;

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
                    Err(_) => {},
                }
            )*

            Err(ParseAbort::NoMatch)
        }
    );
}

#[derive(Debug, Clone, PartialEq)]
enum ParseAbort {
    /// Indicates that the parser was unable to match the input, but that it was
    /// not necessarily an error.
    NoMatch,

    /// Indicates that the parser was unable to match the input and hit the
    /// error described by the returned string.
    Error(String)
}

trait EscalateError {
    fn escalate<MessageFn: FnOnce() -> String>(self, message_fn: MessageFn) -> Self;
}

impl<T> EscalateError for Result<T, ParseAbort> {
    fn escalate<MessageFn: FnOnce() -> String>(self, message_fn: MessageFn) -> Result<T, ParseAbort> {
        match self {
            Err(ParseAbort::NoMatch) => Err(ParseAbort::Error(message_fn())),
            Ok(_) | Err(_) => self,
        }
    }
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
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, T), ParseAbort>;
}

struct EatToken<'a> {
    pub kind: TokenKind<'a>,
}

impl<'a> Parser<'a, &'a Token<'a>> for EatToken<'a> {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, &'a Token<'a>), ParseAbort> {
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

impl<'a> Parser<'a, &'a str> for ParseNumber {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, &'a str), ParseAbort> {
        match state.peek() {
            Some(&Token { kind: TokenKind::NumberLiteral(value), .. }) => Ok((state.advance(1), value)),
            _ => Err(ParseAbort::NoMatch),
        }
    }
}

struct ParseIdentifier;

impl<'a> Parser<'a, &'a str> for ParseIdentifier {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, &'a str), ParseAbort> {
        match state.peek() {
            Some(&Token { kind: TokenKind::Identifier(name), .. }) => Ok((state.advance(1), name)),
            _ => Err(ParseAbort::NoMatch),
        }
    }
}

// chunk ::= {stat [`;´]} [laststat [`;´]]
struct ParseChunk;

impl<'a> Parser<'a, Chunk<'a>> for ParseChunk {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Chunk<'a>), ParseAbort> {
        let mut statements = Vec::new();
        let mut state = state;

        loop {
            state = match ParseStatement.parse(state) {
                Ok((next_state, statement)) => {
                    statements.push(statement);
                    next_state
                },
                Err(_) => break,
            };
        }

        let chunk = Chunk {
            statements,
        };

        Ok((state, chunk))
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
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Statement<'a>), ParseAbort> {
        parse_first_of!(state, {
            ParseLocalAssignment => Statement::LocalAssignment,
            ParseFunctionCall => Statement::FunctionCall,
            ParseNumericFor => Statement::NumericFor,
        })
    }
}

/// Parse a list of one or more identifiers separated by commas.
struct ParseNameListOneOrMore;

impl<'a> Parser<'a, Vec<&'a str>> for ParseNameListOneOrMore {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Vec<&'a str>), ParseAbort> {
        let mut names = Vec::new();

        let (mut state, name) = ParseIdentifier.parse(state)?;
        names.push(name);

        loop {
            match (EatToken { kind: TokenKind::Operator(",") }.parse(state)) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, name) = ParseIdentifier.parse(state)?;

            state = next_state;
            names.push(name);
        }

        Ok((state, names))
    }
}

/// Parse a list of one or more expressions separated by commas.
struct ParseExpressionListOneOrMore;

impl<'a> Parser<'a, Vec<Expression<'a>>> for ParseExpressionListOneOrMore {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Vec<Expression<'a>>), ParseAbort> {
        let mut expressions = Vec::new();

        let (mut state, expression) = ParseExpression.parse(state)?;
        expressions.push(expression);

        loop {
            match (EatToken { kind: TokenKind::Operator(",") }.parse(state)) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, expression) = ParseExpression.parse(state)?;

            state = next_state;
            expressions.push(expression);
        }

        Ok((state, expressions))
    }
}

// local namelist [`=´ explist]
struct ParseLocalAssignment;

impl<'a> Parser<'a, LocalAssignment<'a>> for ParseLocalAssignment {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, LocalAssignment<'a>), ParseAbort> {
        let (state, _) = EatToken { kind: TokenKind::Keyword("local") }.parse(state)?;

        println!("Parsing local!");

        let (state, names) = ParseNameListOneOrMore.parse(state)?;

        println!("Found names: {:?}", names);

        let (state, expressions) = match (EatToken { kind: TokenKind::Operator("=") }.parse(state)) {
            Ok((state, _)) => ParseExpressionListOneOrMore.parse(state)?,
            Err(_) => (state, Vec::new()),
        };

        println!("Found expressions: {:?}", expressions);

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

impl<'a> Parser<'a, FunctionCall<'a>> for ParseFunctionCall {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, FunctionCall<'a>), ParseAbort> {
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::OpenParen }.parse(state)?;
        let (state, expressions) = ParseExpressionList.parse(state)?;
        let (state, _) = EatToken { kind: TokenKind::CloseParen }.parse(state)?;

        Ok((state, FunctionCall {
            name_expression: Box::new(Expression::Name(name)),
            arguments: expressions,
        }))
    }
}

// exp ::= unop exp | value [binop exp]
struct ParseExpression;

impl<'a> Parser<'a, Expression<'a>> for ParseExpression {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Expression<'a>), ParseAbort> {
        parse_first_of!(state, {
            ParseNumber => Expression::Number,
            ParseFunctionCall => Expression::FunctionCall,
            ParseIdentifier => Expression::Name,
        })
    }
}

// explist ::= {exp `,´} exp
struct ParseExpressionList;

impl<'a> Parser<'a, Vec<Expression<'a>>> for ParseExpressionList {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Vec<Expression<'a>>), ParseAbort> {
        let mut state = state;
        let mut expressions = Vec::new();

        loop {
            match ParseExpression.parse(state) {
                Ok((next_state, expression)) => {
                    expressions.push(expression);
                    state = next_state;
                },
                Err(_) => break,
            }

            match (EatToken { kind: TokenKind::Operator(",") }.parse(state)) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }
        }

        Ok((state, expressions))
    }
}

struct ParseNumericFor;

impl<'a> Parser<'a, NumericFor<'a>> for ParseNumericFor {
    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, NumericFor<'a>), ParseAbort> {
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
            _ => panic!("Incorrect statement kind {:?}, statement")
        };
    }
}