#[macro_use]
extern crate lazy_static;
extern crate regex;

mod ast;
mod emitter;
mod lexer;
mod parser;

static TEST_INPUT: &'static str = include_str!("input.lua");

fn main() {
    let tokens = lexer::lex(TEST_INPUT).expect("Could not lex!");
    let ast = parser::parse(&tokens);

    println!("{:?}", tokens);
    println!("{:?}", ast);
}