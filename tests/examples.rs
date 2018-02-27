extern crate lua_parser;

use std::fs::{File, read_dir};
use std::io::Read;

use lua_parser::{tokenize, parse_from_tokens};

#[test]
fn examples() {
    for entry in read_dir("parse_examples/should_parse").unwrap() {
        let entry = entry.unwrap();

        let contents = {
            let mut file = File::open(entry.path())
                .expect("Unable to open file!");

            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .expect("Unable to read from file!");

            contents
        };

        let tokens = match tokenize(&contents) {
            Ok(tokens) => tokens,
            Err(err) => {
                panic!("Failed to tokenize file {}: {:?}", entry.path().display(), err);
            },
        };

        match parse_from_tokens(&tokens) {
            Some(_) => {},
            None => {
                panic!("Failed to parse file {}: None", entry.path().display());
            },
        }
    }

    for entry in read_dir("parse_examples/should_not_parse").unwrap() {
        let entry = entry.unwrap();

        let contents = {
            let mut file = File::open(entry.path())
                .expect("Unable to open file!");

            let mut contents = String::new();
            file.read_to_string(&mut contents)
                .expect("Unable to read from file!");

            contents
        };

        let tokens = match tokenize(&contents) {
            Ok(tokens) => tokens,
            Err(_) => continue,
        };

        let ast = match parse_from_tokens(&tokens) {
            Some(ast) => ast,
            None => continue,
        };

        panic!("File should not parse:\n{}\n\nTokens: {:?}\n\nAST: {:?}", entry.path().display(), tokens, ast);
    }
}