#[derive(Debug, Clone)]
pub struct FunctionCall<'a> {
    pub name_expression: Box<Expression<'a>>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Nil,
    Bool(bool),
    Number(&'a str),
    String(&'a str),
    VarArg, // `...`
    Function,
    Table,
    FunctionCall(FunctionCall<'a>),
    Name(&'a str),
    ParenExpression(Box<Expression<'a>>),
}

#[derive(Debug, Clone)]
pub struct LocalAssignment<'a> {
    pub name: &'a str,
    pub value: Expression<'a>,
}

// stat ::=  ‘;’ |
//     varlist ‘=’ explist |
//     functioncall |
//     label |
//     break |
//     goto Name |
//     do block end |
//     while exp do block end |
//     repeat block until exp |
//     if exp then block {elseif exp then block} [else block] end |
//     for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
//     for namelist in explist do block end |
//     function funcname funcbody |
//     local function Name funcbody |
//     local namelist [‘=’ explist]
#[derive(Debug, Clone)]
pub enum Statement<'a> {
    LocalAssignment(LocalAssignment<'a>),
    FunctionCall(FunctionCall<'a>),
}

// chunk ::= block
// block ::= {stat} [retstat]
#[derive(Debug, Clone)]
pub struct Chunk<'a> {
    pub statements: Vec<Statement<'a>>,
}