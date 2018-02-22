use lexer::Token;

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

    pub fn eat_simple(&self, eat_token: Token) -> Option<(ParseState<'a>, &'a Token<'a>)> {
        match self.peek() {
            Some(token) => {
                if *token == eat_token {
                    Some((self.advance(1), token))
                } else {
                    None
                }
            },
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NumberLiteral<'a> {
    value: &'a str,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    NumberLiteral(NumberLiteral<'a>),
}

#[derive(Debug, Clone)]
pub struct LocalAssignment<'a> {
    name: &'a str,
    value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    LocalAssignment(LocalAssignment<'a>),
}

#[derive(Debug, Clone)]
pub struct Chunk<'a> {
    statements: Vec<Statement<'a>>,
}

#[derive(Debug, Clone)]
pub enum AstNode<'a> {
    Statement(Statement<'a>),
    Expression(Expression<'a>),
    Chunk(Chunk<'a>),
}

pub fn parse<'a>(tokens: &'a [Token<'a>]) -> Option<AstNode<'a>> {
    let state = ParseState::new(tokens);

    let (_, assignment) = parse_local_assignment(state)?;

    Some(AstNode::Statement(Statement::LocalAssignment(assignment)))
}

fn eat_whitespace<'a>(state: ParseState<'a>) -> ParseState<'a> {
    match state.peek() {
        Some(&Token::Whitespace(_)) => state.advance(1),
        _ => state,
    }
}

fn parse_local_assignment<'a>(state: ParseState<'a>) -> Option<(ParseState<'a>, LocalAssignment<'a>)> {
    let (state, _) = state.eat_simple(Token::Keyword("local"))?;
    let state = eat_whitespace(state);

    let (state, name) = {
        match state.peek() {
            Some(&Token::Identifier(name)) => (state.advance(1), name),
            _ => return None,
        }
    };
    let state = eat_whitespace(state);

    let (state, _) = state.eat_simple(Token::Operator("="))?;
    let state = eat_whitespace(state);

    let (state, expression) = parse_expression(state)?;

    Some((state, LocalAssignment {
        name,
        value: expression,
    }))
}

fn parse_expression<'a>(state: ParseState<'a>) -> Option<(ParseState<'a>, Expression<'a>)> {
    Some((state, Expression::NumberLiteral(NumberLiteral {
        value: "5",
    })))
}