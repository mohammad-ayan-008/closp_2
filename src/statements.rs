#![allow(dead_code)]
use crate::expressions::Expression;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Str,
    Void,
    Boolean,
    Char,
    Null,
    Pointer(Box<Type>),
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub params: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub data_type: Type,
    pub expression: Option<Expression>,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct Assignment {
    pub target: Expression,
    pub value: Expression,
}

#[derive(Debug)]
pub struct IFElseStatement{
    pub condition:Expression,
    pub then_block:Block,
    pub else_block:Option<Block>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Option<Expression>),
    Variable(Variable),
    Assignment(Assignment),
    IFStmt(IFElseStatement),
    ExpressionStatement(Expression),
    Block(Block),
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}
