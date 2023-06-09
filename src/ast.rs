//! This module defines the abstract syntax tree for the language, which is
//! the form that our source files take after being parsed.

use std::cell::RefCell;
use std::rc::Rc;

use crate::delayed::*;
use crate::ice::{InternalCompilerError::*, *};
use crate::ir;
use crate::parse::Span;

/// Type is special in our AST because many different
/// instances of `Type` might point to the same "underlying"
/// type (for instance a custom `data` type). So all non-trivial
/// type information that represents a shared underlying type
/// is held behind an `Rc`.
/// Because of this, it's perfectly acceptable and expected
/// to call `.clone()` on `Type`s all over the place, because
/// all you're doing is bumping the reference count on possible
/// non-trivial type information.
#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Nil,
    Int,
    Bool,
    Pointer(Box<Type>),
    Function(Rc<(Type, Vec<Type>)>),
    /// `Delayed<Rc<RefCell<DataType>>>` is a mouthful, so here's
    /// an explanation:
    ///   - Before typechecking, names have not been resolved
    ///     into data types yet, so `Delayed` represents whether
    ///     a type is "filled" or not
    ///   - There will likely be multiple references to a data
    ///     type, so `Rc` ensures we can share it to multiple
    ///     places
    ///   - If our data type is recursive, then the typechecker
    ///     needs to somehow hold a reference to the data type
    ///     in its type map, while simultaneously using a mutable
    ///     reference to that data type to perform the typecheck.
    ///     In that specific circumstance, shared mutability is
    ///     required, so `RefCell` allows us to do that
    Named((String, Delayed<Rc<RefCell<DataType>>>)),
}

impl std::cmp::PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Pointer(..), Self::Nil) => true,
            (Self::Nil, Self::Pointer(..)) => true,
            (Self::Pointer(lhs), Self::Pointer(rhs)) => lhs == rhs,
            (Self::Function(lhs), Self::Function(rhs)) => {
                let (lhs_return, lhs_params) = lhs.as_ref();
                let (rhs_return, rhs_params) = rhs.as_ref();
                if lhs_return != rhs_return {
                    return false;
                }
                if lhs_params.len() != rhs_params.len() {
                    return false;
                }
                for (lhs_param, rhs_param) in lhs_params.iter().zip(rhs_params.iter()) {
                    if lhs_param != rhs_param {
                        return false;
                    }
                }
                true
            }
            (Self::Named((_lhs_name, lhs_type)), Self::Named((_rhs_name, rhs_type))) => {
                match (lhs_type, rhs_type) {
                    (Filled(l), Filled(r)) => Rc::ptr_eq(l, r),
                    _ => ice_error!(ComparedUntypedDataTypeReferences),
                }
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::cmp::Eq for Type {}

pub type InferredTypedExpression = (Delayed<Type>, Expression);

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Bool(bool),
}

#[derive(Debug)]
pub struct New {
    pub name: String,
    pub fields: Vec<(String, Expression)>,
}

#[derive(Debug)]
pub enum ExpressionKind {
    Unit,
    // Temporary! Eventually there will be some Option/Maybe/etc generic type
    // and the ability to create nil references will be removed.
    Nil,
    Literal(Literal),
    Variable(String),
    Dereference(Box<Expression>),
    UnaryOperation(ir::UnaryOperation, Box<InferredTypedExpression>),
    Operation(
        ir::Operation,
        Box<InferredTypedExpression>,
        Box<InferredTypedExpression>,
    ),
    FunctionCall(Delayed<Type>, String, Vec<InferredTypedExpression>),
    Allocate(Box<InferredTypedExpression>),
    New((Delayed<Type>, New)),
    FieldAccess {
        type_: Delayed<Type>,
        lhs: Box<InferredTypedExpression>,
        field: String,
        needs_dereference: Delayed<bool>,
    },
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Expression {
    pub fn new(kind: ExpressionKind, span: Span) -> Expression {
        Expression { kind, span }
    }
}

#[derive(Debug)]
pub struct MakeVariable {
    pub type_: Type,
    pub lhs: String,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub enum LValueKind {
    Identifier(String),
    Dereference(Box<LValue>),
    FieldAccess {
        lhs: Box<LValue>,
        field: String,
        needs_dereference: Delayed<bool>,
    },
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub kind: LValueKind,
    pub type_: Delayed<Type>,
}

impl LValue {
    pub fn new(kind: LValueKind) -> LValue {
        LValue { kind, type_: Empty }
    }
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
    pub else_: Vec<Statement>,
}

#[derive(Debug)]
pub enum StatementKind {
    BareExpression(InferredTypedExpression),
    MakeVariable(MakeVariable),
    SetVariable(SetVariable),
    Return(InferredTypedExpression),
    If(If),
    While {
        condition: InferredTypedExpression,
        body: Vec<Statement>,
    },
    Loop(Vec<Statement>),
    Break,
    // NOTE: Temporary, eventually print will be function in the standard
    //       library. Right now it's built-in just so we can get up and running
    //       with end-to-end tests.
    Print(InferredTypedExpression),
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

impl Statement {
    pub fn new(kind: StatementKind, span: Span) -> Statement {
        Statement { kind, span }
    }
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<(Type, String)>,
    pub returns: Type,
    pub body: Vec<Statement>,
    pub span: Span,
}

pub struct DataType {
    pub name: String,
    pub fields: Vec<(Type, String)>,
    pub span: Span,
}

impl std::fmt::Debug for DataType {
    //! Recursive DataTypes will print infinitely if we let the default recursive
    //! debug implementation be derived.
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "DataType({:?})", self.name)
    }
}

#[derive(Debug)]
pub struct File {
    pub data_types: Vec<Rc<RefCell<DataType>>>,
    pub functions: Vec<Function>,
}
