#[derive(Debug, Clone)]
pub struct FunctionCall<'a> {
    pub name_expression: Box<Expression<'a>>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub struct LocalAssignment<'a> {
    pub names: Vec<&'a str>,
    pub values: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    FunctionCall(FunctionCall<'a>),
    Identifier(&'a str),
    Nil,
    BoolLiteral(bool),
    NumberLiteral(&'a str),
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    FunctionCall(FunctionCall<'a>),
    LocalAssignment(LocalAssignment<'a>),
}

#[derive(Debug, Clone)]
pub struct Chunk<'a> {
    pub statements: Vec<Statement<'a>>,
}