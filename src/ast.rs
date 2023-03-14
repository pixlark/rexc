//! This module defines the abstract syntax tree for the language, which is
//! the form that our source files take after being parsed.

use std::rc::Rc;

use super::ir;

/// Type is special in our AST because many different
/// instances of `Type` might point to the same "underlying"
/// type (for instance a struct). So all non-trivial type
/// information that represents a shared underlying type
/// is held behind an `Rc`.
/// Because of this, it's perfectly acceptable and expected
/// to call `.clone()` on `Type`s all over the place, because
/// all you're doing is bumping the reference count on possible
/// non-trivial type information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Pointer(Box<Type>),
    Function(Rc<(Type, Vec<Type>)>),
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
    Unit,
    Literal(Literal),
    Variable(String),
    Dereference(Box<Expression>),
    Operation(
        ir::Operation,
        Box<InferredTypedExpression>,
        Box<InferredTypedExpression>,
    ),
    FunctionCall(InferredType, String, Vec<InferredTypedExpression>),
    Allocate(Type),
}

#[derive(Debug)]
pub struct MakeVariable {
    pub type_: Type,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Name(String),
    Dereference(Box<LValue>),
}

#[derive(Debug)]
pub struct SetVariable {
    pub lhs: LValue,
    pub rhs: InferredTypedExpression,
}

#[derive(Debug)]
pub struct If {
    pub condition: InferredTypedExpression,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    BareExpression(InferredTypedExpression),
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

#[derive(Debug)]
pub struct File {
    pub functions: Vec<Function>,
}
