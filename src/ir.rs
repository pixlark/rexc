//! Provides a backend-independent intermediate representation of Rexc
//! code. This is just in case we ever want to emit to LLVM or something...
//! as it is, we're just emitting C.

#[derive(Debug, Copy, Clone)]
pub struct BlockLocator(pub usize);

impl BlockLocator {
    pub fn index(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Variable(pub usize);

impl Variable {
    pub fn index(&self) -> usize {
        self.0
    }
}

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
    Variable(Variable),
    Literal(Literal),
    Operation(Operation, Variable, Variable),
    FunctionCall(String, Vec<Variable>),
}

pub struct Assignment {
    pub type_: Type,
    pub lhs: Option<Variable>,
    pub rhs: Rhs,
}

pub type TypedVariable = (Type, Variable);

pub enum BlockTerminator {
    Branch(BlockLocator),
    ConditionalBranch(BlockLocator, BlockLocator, TypedVariable),
    Return(Variable, Type),
}

pub struct Block {
    pub locator: BlockLocator,
    pub assignments: Vec<Assignment>,
    pub block_terminator: BlockTerminator,
}

pub struct Function {
    pub name: String,
    pub returns: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Vec<Block>,
}
