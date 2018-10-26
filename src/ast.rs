use std::borrow::Cow;
use tokenizer::StringLiteral;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOpKind {
    Negate, // -
    BooleanNot, // not
    Length, // #
}

impl UnaryOpKind {
    pub fn precedence(&self) -> u8 {
        11
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum BinaryOpKind {
    Add, // +
    Subtract, // -
    Multiply, // *
    Divide, // /
    Exponent, // ^
    Concat, // ..
}

impl BinaryOpKind {
    // From the 5.3 manual, ranked lowest to highest:
    //  1 or
    //  2 and
    //  3 <     >     <=    >=    ~=    ==
    //  4 |
    //  5 ~
    //  6 &
    //  7 <<    >>
    //  8 ..
    //  9 +     -
    // 10 *     /     //    %
    // 11 unary operators (not   #     -     ~)
    // 12 ^
    pub fn precedence(&self) -> u8 {
        match *self {
            BinaryOpKind::Concat => 8,
            BinaryOpKind::Add | BinaryOpKind::Subtract => 9,
            BinaryOpKind::Multiply | BinaryOpKind::Divide => 10,
            BinaryOpKind::Exponent => 12,
        }
    }

    pub fn is_right_associative(&self) -> bool {
        match *self {
            BinaryOpKind::Exponent | BinaryOpKind::Concat => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnaryOp<'a> {
    pub operator: UnaryOpKind,
    #[serde(borrow)]
    pub argument: Box<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BinaryOp<'a> {
    pub operator: BinaryOpKind,
    #[serde(borrow)]
    pub left: Box<Expression<'a>>,
    pub right: Box<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionCall<'a> {
    #[serde(borrow)]
    pub name_expression: Box<Expression<'a>>,
    pub arguments: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Assignment<'a> {
    #[serde(borrow)]
    pub names: Vec<Cow<'a, str>>,
    pub values: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LocalAssignment<'a> {
    #[serde(borrow)]
    pub names: Vec<Cow<'a, str>>,
    pub values: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NumericFor<'a> {
    #[serde(borrow)]
    pub var: Cow<'a, str>,
    pub start: Expression<'a>,
    pub end: Expression<'a>,
    pub step: Option<Expression<'a>>,
    pub body: Chunk<'a>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GenericFor<'a> {
    #[serde(borrow)]
    pub vars: Vec<Cow<'a, str>>,
    pub item_source: Vec<Expression<'a>>,
    pub body: Chunk<'a>
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct IfStatement<'a> {
    #[serde(borrow)]
    pub condition: Expression<'a>,
    pub body: Chunk<'a>,
    pub else_if_branches: Vec<(Expression<'a>, Chunk<'a>)>,
    pub else_branch: Option<Chunk<'a>>,
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

#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionName<'a> {
    pub segments: Vec<Cow<'a, str>>,
    pub method: Option<Cow<'a, str>>,
}

impl<'a> fmt::Display for FunctionName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = self.segments.join(".");
        f.write_str(&s)?;
        if let Some(m) = &self.method {
            f.write_str(":")?;
            f.write_str(&m)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod function_name_tests {
    use super::*;

    #[test]
    pub fn to_string() {
        assert_eq!(
            FunctionName { segments: vec!["one".into()], method: None }.to_string(),
            "one"
        );

        assert_eq!(
            FunctionName { segments: vec!["one".into(), "two".into()], method: None }.to_string(),
            "one.two"
        );

        assert_eq!(
            FunctionName { segments: vec!["one".into(), "two".into()], method: Some("three".into()) }.to_string(),
            "one.two:three"
        );
    }
}

impl<'a> fmt::Debug for FunctionName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionDeclaration<'a> {
    #[serde(borrow)]
    pub name: FunctionName<'a>,
    pub body: Chunk<'a>,
    pub parameters: Vec<Cow<'a, str>>,
    pub local: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression<'a> {
    Nil,
    Bool(bool),
    #[serde(borrow)]
    Number(Cow<'a, str>),
    String(StringLiteral<'a>),
    VarArg,
    Table(TableLiteral<'a>),
    FunctionCall(FunctionCall<'a>),
    Name(Cow<'a, str>),
    ParenExpression(Box<Expression<'a>>),
    UnaryOp(UnaryOp<'a>),
    BinaryOp(BinaryOp<'a>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TableKey<'a> {
    #[serde(borrow)]
    // '[' expression ']'
    Expression(Expression<'a>),

    // identifier
    Name(Cow<'a, str>),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TableLiteral<'a> {
    #[serde(borrow)]
    pub items: Vec<(Option<TableKey<'a>>, Expression<'a>)>,
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
    GenericFor(GenericFor<'a>),
    IfStatement(IfStatement<'a>),
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