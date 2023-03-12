//! This module defines the abstract syntax tree for the language, which is
//! the form that our source files take after being parsed.

use super::ir;

#[derive(Debug, Copy, Clone, PartialEq, Eq)] // NOTE: Probably temporary while Type is limited to Int
pub enum Type {
    Int,
    Bool,
}

pub type InferredType = Option<Type>;
pub type InferredTypedExpression = (InferredType, Expression);

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expression {
    Literal(Literal),
    Variable(String),
    Operation(
        ir::Operation,
        Box<InferredTypedExpression>,
        Box<InferredTypedExpression>,
    ),
}

#[derive(Debug)]
pub struct MakeVariable {
    pub type_: Type,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug)]
pub struct SetVariable {
    pub lhs: String,
    pub rhs: InferredTypedExpression,
}

#[derive(Debug)]
pub struct If {
    pub condition: InferredTypedExpression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    MakeVariable(MakeVariable),
    SetVariable(SetVariable),
    Return(InferredTypedExpression),
    If(If),
    Loop(Vec<Statement>),
    Break,
    // NOTE: Temporary, eventually print will be function in the standard
    //       library. Right now it's built-in just so we can get up and running
    //       with end-to-end tests.
    Print(InferredTypedExpression),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(Type, String)>,
    pub returns: Type,
    pub body: Vec<Statement>,
}
