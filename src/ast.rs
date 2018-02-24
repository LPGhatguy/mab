#[derive(Debug, Clone)]
pub struct NumberLiteral<'a> {
    pub value: &'a str,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator<'a> {
    pub left: Expression<'a>,
    pub right: Expression<'a>,
    pub operator: &'a str,
}

#[derive(Debug, Clone)]
pub struct FunctionCall<'a> {
    pub name: &'a str,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    NumberLiteral(NumberLiteral<'a>),
    BinaryOperator(Box<BinaryOperator<'a>>),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug, Clone)]
pub struct LocalAssignment<'a> {
    pub name: &'a str,
    pub value: Expression<'a>,
}

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    LocalAssignment(LocalAssignment<'a>),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug, Clone)]
pub struct Chunk<'a> {
    pub statements: Vec<Statement<'a>>,
}