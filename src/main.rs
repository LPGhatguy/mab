#[macro_use]
extern crate lazy_static;
extern crate regex;

mod lexer;

static TEST_INPUT: &'static str = include_str!("input.lua");

fn main() {
    let tokens = lexer::lex(TEST_INPUT);

    println!("{:?}", tokens);
}
