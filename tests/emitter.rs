extern crate lua_parser;

use lua_parser::emitter::emit_chunk;
use lua_parser::ast2::*;

#[test]
fn emitter() {
    let chunk = Chunk {
        statements: vec![
            Statement::LocalAssignment(LocalAssignment {
                names: vec![
                    "a",
                    "b",
                ],
                values: vec![
                    Expression::BoolLiteral(true),
                    Expression::NumberLiteral("5.23"),
                ],
            }),
            Statement::FunctionCall(FunctionCall {
                name_expression: Box::new(Expression::Identifier("print")),
                arguments: vec![
                    Expression::Identifier("a"),
                    Expression::Identifier("b"),
                ],
            }),
        ],
    };

    let mut out = String::new();
    emit_chunk(&chunk, &mut out);

    println!("{}", out);
}