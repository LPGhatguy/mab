use tokenizer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseAbort {
    /// Indicates that the parser was unable to match the input, but that it was
    /// not necessarily an error.
    NoMatch,

    /// Indicates that the parser was unable to match the input and hit the
    /// error described by the returned string.
    #[allow(dead_code)]
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
    ($state: ident, { $( $parser: expr => $constructor: expr ),* $(,)* }) => (
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

pub struct DelimitedOneOrMore<ItemParser, DelimiterParser>(pub ItemParser, pub DelimiterParser);

impl<'a, ItemParser: Parser<'a>, DelimiterParser: Parser<'a>> Parser<'a> for DelimitedOneOrMore<ItemParser, DelimiterParser> {
    type Item = Vec<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("one or more {} separated by {}", self.0.item_name(), self.1.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let mut values = Vec::new();

        let (mut state, value) = self.0.parse(state)?;
        values.push(value);

        loop {
            match self.1.parse(state) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, value) = self.0.parse(state)?;

            state = next_state;
            values.push(value);
        }

        Ok((state, values))
    }
}

pub struct DelimitedZeroOrMore<ItemParser, DelimiterParser>(pub ItemParser, pub DelimiterParser, pub bool);

impl<'a, ItemParser: Parser<'a>, DelimiterParser: Parser<'a>> Parser<'a> for DelimitedZeroOrMore<ItemParser, DelimiterParser> {
    type Item = Vec<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("zero or more {} separated by {}", self.0.item_name(), self.1.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        let mut values = Vec::new();

        let mut state = match self.0.parse(state) {
            Ok((next_state, value)) => {
                values.push(value);
                next_state
            },
            Err(_) => return Ok((state, Vec::new())),
        };

        loop {
            match self.1.parse(state) {
                Ok((next_state, _)) => {
                    state = next_state;
                },
                Err(_) => break,
            }

            let (next_state, value) = match self.0.parse(state) {
                Ok((next_state, value)) => (next_state, value),
                Err(ParseAbort::NoMatch) => {
                    // 2: allow trailing delimiter
                    if self.2 {
                        break
                    }
                    else {
                        return Err(ParseAbort::NoMatch)
                    }
                },
                Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message))
            };

            state = next_state;
            values.push(value);
        }

        Ok((state, values))
    }
}

pub struct Optional<InnerParser>(pub InnerParser);

impl<'a, ItemParser: Parser<'a>> Parser<'a> for Optional<ItemParser> {
    type Item = Option<ItemParser::Item>;

    fn item_name(&self) -> String {
        format!("optional {}", self.0.item_name())
    }

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        match self.0.parse(state) {
            Ok((new_state, matched_value)) => Ok((new_state, Some(matched_value))),
            Err(ParseAbort::NoMatch) => Ok((state, None)),
            Err(ParseAbort::Error(message)) => Err(ParseAbort::Error(message)),
        }
    }
}

pub struct Or<'a, InnerParser: 'a>(pub &'a [InnerParser]);

impl<'a, InnerParser: Parser<'a>> Parser<'a> for Or<'a, InnerParser> {
    type Item = InnerParser::Item;

    fn parse(&self, state: ParseState<'a>) -> Result<(ParseState<'a>, Self::Item), ParseAbort> {
        for parser in self.0 {
            match parser.parse(state) {
                Ok((new_state, matched_value)) => return Ok((new_state, matched_value)),
                Err(ParseAbort::NoMatch) => (),
                Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message)),
            }
        }

        Err(ParseAbort::NoMatch)
    }
}