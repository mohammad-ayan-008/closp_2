use crate::expressions::Expression;

#[derive(Debug)]
pub enum Type{
    Int,
    Float,
    Str,
    Void,
    Boolean,
    Null,
    Pointer(Box<Type>)
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
pub struct Program {
    items: Vec<Item>,
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
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Option<Expression>),
    Block(Block),
}

#[derive(Debug)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}


