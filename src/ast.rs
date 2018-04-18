#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionCall<'a> {
    #[serde(borrow)]
    pub name_expression: Box<Expression<'a>>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Assignment<'a> {
    #[serde(borrow)]
    pub names: Vec<&'a str>,
    pub values: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LocalAssignment<'a> {
    #[serde(borrow)]
    pub names: Vec<&'a str>,
    pub values: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NumericFor<'a> {
    pub var: &'a str,
    pub start: Expression<'a>,
    pub end: Expression<'a>,
    pub step: Option<Expression<'a>>,
    pub body: Chunk<'a>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct WhileLoop<'a> {
    #[serde(borrow)]
    pub condition: Expression<'a>,
    pub body: Chunk<'a>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RepeatLoop<'a> {
    #[serde(borrow)]
    pub condition: Expression<'a>,
    pub body: Chunk<'a>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a str,
    pub body: Chunk<'a>,
    pub parameters: Vec<&'a str>,
    pub local: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression<'a> {
    Nil,
    Bool(bool),
    Number(&'a str),
    String(&'a str),
    VarArg,
    Function,
    Table(TableLiteral<'a>),
    #[serde(borrow)]
    FunctionCall(FunctionCall<'a>),
    Name(&'a str),
    ParenExpression(Box<Expression<'a>>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TableLiteral<'a> {
    #[serde(borrow)]
    pub items: Vec<(Expression<'a>, Expression<'a>)>,
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement<'a> {
    #[serde(borrow)]
    Assignment(Assignment<'a>),
    LocalAssignment(LocalAssignment<'a>),
    FunctionCall(FunctionCall<'a>),
    NumericFor(NumericFor<'a>),
    WhileLoop(WhileLoop<'a>),
    RepeatLoop(RepeatLoop<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
}

// chunk ::= block
// block ::= {stat} [retstat]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Chunk<'a> {
    #[serde(borrow)]
    pub statements: Vec<Statement<'a>>,
}