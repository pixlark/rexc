//! Provides a backend-independent intermediate representation of Rexc
//! code. This is just in case we ever want to emit to LLVM or something...
//! as it is, we're just emitting C.

#[derive(Copy, Clone)] // NOTE: Probably temporary while Type is limited to Int
pub enum Type {
    Void,
    Int,
}

pub enum Literal {
    Int(i64),
}

#[derive(Copy, Clone, Debug)]
pub enum Operation {
    Add,
}

pub enum Rhs {
    Parameter(String),
    Variable(usize),
    Literal(Literal),
    Operation(Operation, Box<Rhs>, Box<Rhs>),
    FunctionCall(String, Vec<usize>),
}

pub struct Assignment {
    pub type_: Type,
    pub lhs: Option<usize>,
    pub rhs: Rhs,
}

pub type TypedVariable = (Type, usize);

pub enum BlockTerminator {
    ConditionalBranch(usize, usize, TypedVariable),
    Return(usize, Type),
}

pub struct Block {
    pub index: usize,
    pub assignments: Vec<Assignment>,
    pub block_terminator: BlockTerminator,
}

pub struct Function {
    pub name: String,
    pub returns: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Vec<Block>,
}
