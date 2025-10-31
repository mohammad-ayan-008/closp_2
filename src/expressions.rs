#![allow(dead_code, non_camel_case_types,clippy::upper_case_acronyms)]

use crate::statements::Type;
#[derive(Debug)]
pub enum Binaryop {
    ADD,
    SUB,
    DIV,
    MUL,
    GT,
    GTE,
    LT,
    LTE,
    NotEq,
    EqualEqual,
    And,
    Or,
    BITAND,
    BITOR,
}


#[derive(Debug)]
pub enum UnaryOP{
    Negate,
    Not
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        lhs: Box<Expression>,
        op: Binaryop,
        rhs: Box<Expression>,
    },
    Unary {
        token: UnaryOP,
        exp: Box<Expression>,
    },
    Cast{
        expected:Type,
        expr:Box<Expression>,
    },
    FunctionCall{
        name:String,
        args:Vec<Expression>
    },
    Float_Literal(f64),
    Int_Literal(i64),
    String_Literal(String),
    Identifier(String),
    Null,
}


