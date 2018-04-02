use ast2::*;

pub fn emit_chunk<'a>(chunk: &Chunk<'a>, buffer: &mut String) {
    for (index, statement) in chunk.statements.iter().enumerate() {
        emit_statement(statement, buffer);

        if index < chunk.statements.len() - 1 {
            buffer.push('\n');
        }
    }
}

fn emit_statement<'a>(statement: &Statement<'a>, buffer: &mut String) {
    match statement {
        &Statement::FunctionCall(ref call) => emit_function_call(call, buffer),
        &Statement::LocalAssignment(ref assignment) => emit_local_assignment(assignment, buffer),
    }
}

fn emit_expression<'a>(expression: &Expression<'a>, buffer: &mut String) {
    match expression {
        &Expression::FunctionCall(ref call) => emit_function_call(call, buffer),
        &Expression::Identifier(ref name) => buffer.push_str(name),
        &Expression::Nil => buffer.push_str("nil"),
        &Expression::BoolLiteral(value) => if value {
            buffer.push_str("true");
        } else {
            buffer.push_str("false");
        },
        &Expression::NumberLiteral(value) => buffer.push_str(value),
    }
}

fn emit_function_call<'a>(call: &FunctionCall<'a>, buffer: &mut String) {
    emit_expression(&call.name_expression, buffer);
    buffer.push('(');

    for (index, arg) in call.arguments.iter().enumerate() {
        emit_expression(arg, buffer);

        if index < call.arguments.len() - 1 {
            buffer.push_str(", ");
        }
    }

    buffer.push(')');
}

fn emit_local_assignment<'a>(assignment: &LocalAssignment<'a>, buffer: &mut String) {
    buffer.push_str("local ");

    for (index, name) in assignment.names.iter().enumerate() {
        buffer.push_str(name);

        if index < assignment.names.len() - 1 {
            buffer.push_str(", ");
        }
    }

    buffer.push_str(" = ");

    for (index, value) in assignment.values.iter().enumerate() {
        emit_expression(value, buffer);

        if index < assignment.values.len() - 1 {
            buffer.push_str(", ");
        }
    }
}