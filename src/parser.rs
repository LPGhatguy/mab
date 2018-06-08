use std::borrow::Cow;

use tokenizer::{Token, TokenKind, Symbol, StringLiteral};
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
        Some(Token { kind: TokenKind::EndOfFile, .. }) => {},
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

struct ParseSymbol(pub Symbol);
define_parser!(ParseSymbol, Symbol, |this: &ParseSymbol, state: ParseState<'state>| {
    let (state, token) = ParseToken(TokenKind::Symbol(this.0)).parse(state)?;
    let symbol = match token.kind {
        TokenKind::Symbol(symbol) => symbol,
        // The parsing will only succeed if we can eat a symbol.
        _ => unreachable!()
    };

    Ok((state, symbol))
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
        ParseGenericFor => Statement::GenericFor,
        ParseIfStatement => Statement::IfStatement,
        ParseWhileLoop => Statement::WhileLoop,
        ParseRepeatLoop => Statement::RepeatLoop,
        ParseFunctionDeclaration => Statement::FunctionDeclaration,
        ParseAssignment => Statement::Assignment,
    })
});

struct ParseUnaryOp;
define_parser!(ParseUnaryOp, UnaryOpKind, |_, state: ParseState<'state>| {
    if let Some(&Token { kind: TokenKind::Symbol(symbol), .. }) = state.peek() {
        let kind = match symbol {
            Symbol::Minus => UnaryOpKind::Negate,
            Symbol::Hash => UnaryOpKind::Length,
            Symbol::Not => UnaryOpKind::BooleanNot,
            _ => return Err(ParseAbort::NoMatch),
        };

        Ok((state.advance(1), kind))
    } else {
        Err(ParseAbort::NoMatch)
    }
});

struct ParseBinaryOp;
define_parser!(ParseBinaryOp, BinaryOpKind, |_, state: ParseState<'state>| {
    if let Some(&Token { kind: TokenKind::Symbol(symbol), .. }) = state.peek() {
        let kind = match symbol {
            Symbol::Plus => BinaryOpKind::Add,
            Symbol::Minus => BinaryOpKind::Subtract,
            Symbol::Star => BinaryOpKind::Multiply,
            Symbol::Slash => BinaryOpKind::Divide,
            Symbol::Caret => BinaryOpKind::Exponent,
            Symbol::TwoDots => BinaryOpKind::Concat,
            _ => return Err(ParseAbort::NoMatch),
        };

        Ok((state.advance(1), kind))
    } else {
        Err(ParseAbort::NoMatch)
    }
});

struct ParseExpressionAtPrecedence(u8);
define_parser!(ParseExpressionAtPrecedence, Expression<'state>, |this: &ParseExpressionAtPrecedence, state| {
    let min_precedence = this.0;
    let (mut state, mut atom_lhs) = ParseExpressionAtom.parse(state)?;

    loop {
        let (next_state, operator) = match ParseBinaryOp.parse(state) {
            Ok(v) => v,
            Err(_) => break,
        };

        if operator.precedence() < min_precedence {
            break;
        }

        let next_min_precedence = if operator.is_right_associative() {
            operator.precedence()
        } else {
            operator.precedence() + 1
        };

        let (next_state, atom_rhs) = ParseExpressionAtPrecedence(next_min_precedence).parse(next_state)?;
        state = next_state;

        atom_lhs = Expression::BinaryOp(BinaryOp {
            operator,
            left: Box::new(atom_lhs),
            right: Box::new(atom_rhs),
        });
    }

    Ok((state, atom_lhs))
});

struct ParseExpression;
define_parser!(ParseExpression, Expression<'state>, |_, state| {
    ParseExpressionAtPrecedence(1).parse(state)
});

struct ParseExpressionAtom;
define_parser!(ParseExpressionAtom, Expression<'state>, |_, state| {
    if let Ok((next_state, expression)) = ParseUnaryExpression.parse(state) {
        Ok((next_state, expression))
    } else if let Ok((next_state, expression)) = ParseParenExpression.parse(state) {
        Ok((next_state, expression))
    } else if let Ok((next_state, expression)) = ParseValue.parse(state) {
        Ok((next_state, expression))
    } else {
        Err(ParseAbort::NoMatch)
    }
});

struct ParseUnaryExpression;
define_parser!(ParseUnaryExpression, Expression<'state>, |_, state| {
    let (state, operator) = ParseUnaryOp.parse(state)?;
    let (state, argument) = ParseExpressionAtPrecedence(operator.precedence()).parse(state)?;

    Ok((state, Expression::UnaryOp(UnaryOp {
        operator,
        argument: Box::new(argument),
    })))
});

struct ParseParenExpression;
define_parser!(ParseParenExpression, Expression<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let (state, expression) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;

    Ok((state, Expression::ParenExpression(Box::new(expression))))
});

struct ParseIndex;
define_parser!(ParseIndex, Index<'state>, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::Symbol(Symbol::Dot), .. }) => {
            let (state, name) = ParseIdentifier.parse(state.advance(1))?;
            println!("parsed dot index {:?}", name);
            Ok((state, Index::Name(name)))
        },
        Some(&Token { kind: TokenKind::Symbol(Symbol::LeftBracket), .. }) => {
            let (state, expression) = ParseExpression.parse(state.advance(1))?;
            let (state, _) = ParseSymbol(Symbol::RightBracket).parse(state)?;
            println!("parsed bracketed index {:?}", expression);
            Ok((state, Index::Bracketed(expression)))
        },
        _ => Err(ParseAbort::NoMatch),
    }
});

struct ParsePrefix;
define_parser!(ParsePrefix, Expression<'state>, |_, state| {
    // prefix ::= '(' exp ')' | Name
    match ParseParenExpression.parse(state) {
        Ok((state, prefix_expr)) => Ok((state, prefix_expr)),
        Err(ParseAbort::NoMatch) => {
            let (state, name) = ParseIdentifier.parse(state)?;
            Ok((state, Expression::Name(name)))
        },
        Err(ParseAbort::Error(message)) => Err(ParseAbort::Error(message)),
    }
});

struct ParseFunctionArgumentList;
define_parser!(ParseFunctionArgumentList, Vec<Expression<'state>>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let (state, args) = ZeroOrMore(ParseExpression).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;
    Ok((state, args))
});

struct ParseFunctionArguments;
define_parser!(ParseFunctionArguments, FunctionArguments<'state>, |_, state| {
    parse_first_of!(state, {
        ParseTableLiteral => FunctionArguments::Table,
        ParseString => FunctionArguments::String,
        ParseFunctionArgumentList => FunctionArguments::List,
    })
});

struct ParseMethodCall;
define_parser!(ParseMethodCall, (Cow<'state, str>, FunctionArguments<'state>), |_, state| {
    let (state, _) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, args) = ParseFunctionArguments.parse(state)?;
    Ok((state, (name, args)))
});

struct ParseCall;
define_parser!(ParseCall, Call<'state>, |_, state| {
    parse_first_of!(state, {
        ParseFunctionArguments => Call::Anonymous,
        ParseMethodCall => |(name, args)| Call::Method(name, args),
    })
});

struct ParseSuffix;
define_parser!(ParseSuffix, Suffix<'state>, |_, state| {
    // suffix ::= call | index
    parse_first_of!(state, {
        ParseIndex => Suffix::Index,
        ParseCall => Suffix::Call,
    })
});

struct ParseComplexVarName;
define_parser!(ParseComplexVarName, VarName<'state>, |_, state| {
    // var ::= prefix {suffix} index | Name
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;
    let mut result_suffixes = suffixes.clone();
    let last_suffix = suffixes.last();

    // This is necessary because a suffix can *also* be an index, so we might have parsed what
    // should have been the index as a suffix!
    let (state, index) = match ParseIndex.parse(state) {
        // If there was an index after the last suffix (if the last suffix was a call), then
        // we're done!
        Ok((new_state, index)) => (new_state, index),
        // Otherwise, the index is actually the last suffix in disguise, and we have to pull
        // it out of the vector.
        Err(ParseAbort::NoMatch) => {
            match last_suffix {
                Some(Suffix::Index(index)) => {
                    result_suffixes.pop();
                    (state, index.clone())
                },
                _ => return Err(ParseAbort::NoMatch),
            }
        },
        Err(ParseAbort::Error(message)) => return Err(ParseAbort::Error(message))
    };

    Ok((state, VarName::Expression(prefix, result_suffixes, index)))
});

struct ParseVarName;
define_parser!(ParseVarName, VarName<'state>, |_, state| {
    match ParseComplexVarName.parse(state) {
        Ok((state, name)) => Ok((state, name)),
        Err(ParseAbort::NoMatch) => {
            let (state, name) = ParseIdentifier.parse(state)?;
            Ok((state, VarName::Name(name)))
        },
        Err(ParseAbort::Error(message)) => Err(ParseAbort::Error(message)),
    }
});

struct ParseValue;
define_parser!(ParseValue, Expression<'state>, |_, state| {
    parse_first_of!(state, {
        ParseNumber => Expression::Number,
        ParseFunctionCall => Expression::FunctionCall,
        ParseIdentifier => Expression::Name,
        ParseTableLiteral => Expression::Table,
        ParseBoolean => Expression::Bool,
        // Hack: parse_first_of! cannot handle unit values
        ParseNil => |_| Expression::Nil,
        ParseString => Expression::String,
    })
});

struct ParseBoolean;
define_parser!(ParseBoolean, bool, |_, state| {
    let (state, matched) = Or(&[ ParseSymbol(Symbol::True), ParseSymbol(Symbol::False) ]).parse(state)?;
    Ok((state, matched == Symbol::True))
});

struct ParseNil;
define_parser!(ParseNil, (), |_, state| {
    let (state, _) = ParseSymbol(Symbol::Nil).parse(state)?;
    Ok((state, ()))
});

struct ParseString;
define_parser!(ParseString, StringLiteral<'state>, |_, state: ParseState<'state>| {
    match state.peek() {
        Some(&Token { kind: TokenKind::StringLiteral(ref value), .. }) => Ok((state.advance(1), value.clone())),
        _ => Err(ParseAbort::NoMatch),
    }
});

struct ParseAssignment;
define_parser!(ParseAssignment, Assignment<'state>, |_, state| {
    let (state, names) = DelimitedOneOrMore(ParseVarName, ParseSymbol(Symbol::Comma)).parse(state)?;
    // An = is required for assignment, unlike local assignment
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, expressions) = DelimitedOneOrMore(ParseExpression, ParseSymbol(Symbol::Comma)).parse(state)?;

    Ok((state, Assignment {
        names: names,
        values: expressions,
    }))
});

// local namelist [`=´ explist]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Local).parse(state)?;

    let (state, names) = DelimitedOneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma)).parse(state)?;

    let (state, expressions) = match ParseSymbol(Symbol::Equal).parse(state) {
        Ok((state, _)) => DelimitedOneOrMore(ParseExpression, ParseSymbol(Symbol::Comma)).parse(state)?,
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
    let (state, _) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let (state, expressions) = DelimitedZeroOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;

    Ok((state, FunctionCall {
        name_expression: Box::new(Expression::Name(name)),
        arguments: expressions,
    }))
});

struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::For).parse(state)?;
    let (state, var) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, start) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Comma).parse(state)?;
    let (state, end) = ParseExpression.parse(state)?;

    let (state, step) = match state.peek() {
        Some(&Token { kind: TokenKind::Symbol(Symbol::Comma), .. }) => {
            let (new_state, parsed_step) = ParseExpression.parse(state.advance(1))?;

            (new_state, Some(parsed_step))
        },
        Some(&Token { kind: TokenKind::Symbol(Symbol::Do), .. }) => {
            (state, None)
        },
        _ => return Err(ParseAbort::NoMatch),
    };

    let (state, _) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Ok((state, NumericFor {
        var,
        start,
        end,
        step,
        body,
    }))
});

struct ParseGenericFor;
define_parser!(ParseGenericFor, GenericFor<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::For).parse(state)?;
    let (state, vars) = DelimitedOneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma)).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::In).parse(state)?;
    let (state, item_source) = DelimitedOneOrMore(ParseExpression, ParseSymbol(Symbol::Comma)).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Ok((state, GenericFor {
        vars,
        item_source,
        body,
    }))
});

struct ParseIfStatement;
define_parser!(ParseIfStatement, IfStatement<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::If).parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Then).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;

    let mut state = state;
    let mut else_if_branches = Vec::new();
    loop {
        let (next_state, _) = match ParseSymbol(Symbol::ElseIf).parse(state) {
            Ok(v) => v,
            Err(_) => break,
        };

        let (next_state, condition) = ParseExpression.parse(next_state)?;
        let (next_state, _) = ParseSymbol(Symbol::Then).parse(next_state)?;
        let (next_state, body) = ParseChunk.parse(next_state)?;

        state = next_state;
        else_if_branches.push((condition, body));
    }

    let (state, else_branch) = match ParseSymbol(Symbol::Else).parse(state) {
        Ok((state, _)) => {
            let (state, body) = ParseChunk.parse(state)?;

            (state, Some(body))
        },
        Err(_) => (state, None),
    };

    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Ok((state, IfStatement {
        condition,
        body,
        else_if_branches,
        else_branch,
    }))
});

struct ParseWhileLoop;
define_parser!(ParseWhileLoop, WhileLoop<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::While).parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Ok((state, WhileLoop {
        condition,
        body,
    }))
});

struct ParseRepeatLoop;
define_parser!(ParseRepeatLoop, RepeatLoop<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Repeat).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Until).parse(state)?;
    let (state, condition) = ParseExpression.parse(state)?;

    Ok((state, RepeatLoop {
        condition,
        body,
    }))
});

struct ParseFunctionDeclaration;
define_parser!(ParseFunctionDeclaration, FunctionDeclaration<'state>, |_, state| {
    let (state, local) = Optional(ParseSymbol(Symbol::Local)).parse(state)
        .map(|(state, value)| (state, value.is_some()))?;

    let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let (state, parameters) = DelimitedZeroOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;
    let (state, body) = ParseChunk.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

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
            let (state, _) = ParseSymbol(Symbol::LeftBracket).parse(state)?;
            let (state, key) = ParseExpression.parse(state)?;
            let (state, _) = ParseSymbol(Symbol::RightBracket).parse(state)?;

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
        Some(_) => ParseSymbol(Symbol::Equal).parse(state)?.0,
        None => state
    };

    let (state, value) = ParseExpression.parse(state)?;
    Ok((state, (key, value)))
});

struct ParseTableLiteral;
define_parser!(ParseTableLiteral, TableLiteral<'state>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::LeftBrace).parse(state)?;
    let (state, items) = DelimitedZeroOrMore(ParseTableValue, Or(&[ ParseSymbol(Symbol::Comma), ParseSymbol(Symbol::Semicolon) ]), true).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightBrace).parse(state)?;
    Ok((state, TableLiteral {
        items
    }))
});