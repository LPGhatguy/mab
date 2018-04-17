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
        &Statement::WhileLoop(ref value) => emit_while_loop(w, value)?,
    }

    Ok(())
}

fn emit_assignment<'a>(w: &mut Write, assignment: &Assignment<'a>) -> fmt::Result {
    Ok(())
}

fn emit_local_assignment<'a>(w: &mut Write, assignment: &LocalAssignment<'a>) -> fmt::Result {
    Ok(())
}

fn emit_function_call<'a>(w: &mut Write, function_call: &FunctionCall<'a>) -> fmt::Result {
    Ok(())
}

fn emit_numeric_for<'a>(w: &mut Write, numeric_for: &NumericFor<'a>) -> fmt::Result {
    Ok(())
}

fn emit_while_loop<'a>(w: &mut Write, while_loop: &WhileLoop<'a>) -> fmt::Result {
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
        emit_chunk(&mut output, &chunk);

        assert_eq!(&output, "");
    }
}