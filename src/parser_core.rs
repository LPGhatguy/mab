use tokenizer::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseAbort {
    /// Indicates that the parser was unable to match the input, but that it was
    /// not necessarily an error.
    NoMatch,

    /// Indicates that the parser was unable to match the input and hit the
    /// error described by the returned string.
    Error(String)
}

#[derive(Debug, Clone, Copy)]
pub struct ParseState<'a> {
    pub tokens: &'a [Token<'a>],
    pub position: usize,
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

pub trait Parser<'a> {
    type Item: 'a;

    fn item_name(&self) -> String {
        "UNNAMED_ITEM".to_string()
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort>;
}

#[macro_export]
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

#[macro_export]
macro_rules! define_parser {
    ($name: ty, $result_type: ty, $body: expr) => {
        impl<'state> Parser<'state> for $name {
            type Item = $result_type;

            fn parse(&self, state: ParseState<'state>) -> Result<(ParseState<'state>, Self::Item), ParseAbort> {
                $body(self, state)
            }
        }
    }
}

pub struct ZeroOrMore<ItemParser>(pub ItemParser);

impl<'a, ItemParser: Parser<'a>> Parser<'a> for ZeroOrMore<ItemParser> {
    type Item = Vec<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("zero or more {}", self.0.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let mut values = Vec::new();
        let mut state = state;

        loop {
            match self.0.parse(state) {
                Ok((next_state, value)) => {
                    values.push(value);
                    state = next_state;
                },
                Err(ParseAbort::NoMatch) => break,
                Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message)),
            }
        }

        Ok((state, values))
    }
}

pub struct DelimitedOneOrMore<ItemParser, DelimiterParser> {
    pub item_parser: ItemParser,
    pub delimiter_parser: DelimiterParser,
}

impl<'a, ItemParser: Parser<'a>, DelimiterParser: Parser<'a>> Parser<'a> for DelimitedOneOrMore<ItemParser, DelimiterParser> {
    type Item = Vec<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("one or more {} separated by {}", self.item_parser.item_name(), self.delimiter_parser.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let mut values = Vec::new();

        let (mut state, value) = self.item_parser.parse(state)?;
        values.push(value);

        loop {
            match self.delimiter_parser.parse(state) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, value) = self.item_parser.parse(state)?;

            state = next_state;
            values.push(value);
        }

        Ok((state, values))
    }
}

pub struct DelimitedZeroOrMore<ItemParser, DelimiterParser> {
    pub item_parser: ItemParser,
    pub delimiter_parser: DelimiterParser,
}

impl<'a, ItemParser: Parser<'a>, DelimiterParser: Parser<'a>> Parser<'a> for DelimitedZeroOrMore<ItemParser, DelimiterParser> {
    type Item = Vec<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("zero or more {} separated by {}", self.item_parser.item_name(), self.delimiter_parser.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let mut values = Vec::new();

        let mut state = match self.item_parser.parse(state) {
            Ok((next_state, value)) => {
                values.push(value);
                next_state
            },
            Err(_) => return Ok((state, Vec::new())),
        };

        loop {
            match self.delimiter_parser.parse(state) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, value) = self.item_parser.parse(state)?;

            state = next_state;
            values.push(value);
        }

        Ok((state, values))
    }
}