//! This module defines the abstract syntax tree for the language, which is
//! the form that our source files take after being parsed.

use std::cell::RefCell;
use std::rc::Rc;

use super::ir;
use super::parse::Span;

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
    /// SAFETY:
    ///   Because we've flattened `Dereference` to denote number of derefs with a `usize`
    /// rather than recursively, we end up having to construct new `Dereference`s to recurse
    /// on: `Dereference(n - 1, expr)`.
    ///   This means copying around the `Expression` pointer, meaning it needs to be `Rc` and not
    /// `Box`. Furthermore, because `.typecheck()` takes an `&mut` self reference, we need to have
    /// a `RefCell` inside the `Rc` to grant us mutable access.
    ///   We only do one compiler phase at a time, and these extra clones that get created are
    /// mutably recursed on only once each. Thus we know our `RefCell` won't end up with more than
    /// one mutable reference. So this use of `Rc<RefCell<Expression>>` is safe.
    Dereference(usize, Rc<RefCell<Expression>>),
    Operation(
        ir::Operation,
        Box<InferredTypedExpression>,
        Box<InferredTypedExpression>,
    ),
    FunctionCall(InferredType, String, Vec<InferredTypedExpression>),
    Allocate(Box<InferredTypedExpression>),
}

#[derive(Debug)]
pub struct MakeVariable {
    pub type_: Type,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub name: String,
    pub derefs: usize,
}

#[derive(Debug)]
pub struct SetVariable {
    pub lhs: LValue,
    pub rhs: InferredTypedExpression,
}

#[derive(Debug)]
pub struct If {
    pub condition: InferredTypedExpression,
    pub body: Vec<Span<Statement>>,
}

#[derive(Debug)]
pub enum Statement {
    BareExpression(InferredTypedExpression),
    MakeVariable(MakeVariable),
    SetVariable(SetVariable),
    Return(InferredTypedExpression),
    If(If),
    Loop(Vec<Span<Statement>>),
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
    pub body: Vec<Span<Statement>>,
}

#[derive(Debug)]
pub struct File {
    pub functions: Vec<Function>,
}
