use std::fmt::{self, Write};

use ast::*;

pub fn emit_chunk<'a>(w: &mut Write, chunk: &Chunk<'a>) -> fmt::Result {
    for statement in &chunk.statements {
        emit_statement(w, statement)?;
    }

    Ok(())
}

fn emit_statement<'a>(w: &mut Write, statement: &Statement<'a>) -> fmt::Result {
    match statement {
        &Statement::Assignment(ref value) => emit_assignment(w, value)?,
        &Statement::LocalAssignment(ref value) => emit_local_assignment(w, value)?,
        &Statement::FunctionCall(ref value) => emit_function_call(w, value)?,
        &Statement::NumericFor(ref value) => emit_numeric_for(w, value)?,
        &Statement::GenericFor(ref value) => emit_generic_for(w, value)?,
        &Statement::IfStatement(ref value) => emit_if_statement(w, value)?,
        &Statement::WhileLoop(ref value) => emit_while_loop(w, value)?,
        &Statement::RepeatLoop(ref value) => emit_repeat_loop(w, value)?,
        &Statement::FunctionDeclaration(ref value) => emit_function_declaration(w, value)?,
    }

    Ok(())
}

fn emit_assignment<'a>(w: &mut Write, _assignment: &Assignment<'a>) -> fmt::Result {
    write!(w, "assignment")?;

    Ok(())
}

fn emit_local_assignment<'a>(w: &mut Write, _assignment: &LocalAssignment<'a>) -> fmt::Result {
    write!(w, "local assignment")?;

    Ok(())
}

fn emit_function_call<'a>(w: &mut Write, _function_call: &FunctionCall<'a>) -> fmt::Result {
    write!(w, "function call")?;

    Ok(())
}

fn emit_numeric_for<'a>(w: &mut Write, _numeric_for: &NumericFor<'a>) -> fmt::Result {
    write!(w, "numeric for")?;

    Ok(())
}

fn emit_generic_for<'a>(w: &mut Write, _generic_for: &GenericFor<'a>) -> fmt::Result {
    write!(w, "generic for")?;

    Ok(())
}

fn emit_if_statement<'a>(w: &mut Write, _if_statement: &IfStatement<'a>) -> fmt::Result {
    write!(w, "if statement")?;

    Ok(())
}

fn emit_while_loop<'a>(w: &mut Write, _while_loop: &WhileLoop<'a>) -> fmt::Result {
    write!(w, "while loop")?;

    Ok(())
}

fn emit_repeat_loop<'a>(w: &mut Write, _repeat_loop: &RepeatLoop<'a>) -> fmt::Result {
    write!(w, "repeat loop")?;

    Ok(())
}

fn emit_function_declaration<'a>(w: &mut Write, _function_declaration: &FunctionDeclaration<'a>) -> fmt::Result {
    write!(w, "function declaration")?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emit_empty_chunk() {
        let chunk = Chunk {
            statements: Vec::new(),
        };

        let mut output = String::new();
        emit_chunk(&mut output, &chunk).unwrap();

        assert_eq!(&output, "");
    }
}