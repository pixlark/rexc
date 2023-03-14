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

#[derive(Clone)] // NOTE: Probably temporary while Type is limited to Int
pub enum Type {
    Void,
    Int,
    Pointer(Box<Type>),
    Function(std::rc::Rc<(Type, Vec<Type>)>),
}

pub enum Literal {
    Int(i64),
}

#[derive(Copy, Clone, Debug)]
pub enum Operation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
}

pub enum FunctionReference {
    Local(Variable),
    Parameter(String),
    FileScope(String),
}

pub enum Rhs {
    Void,
    Parameter(String),
    Variable(Variable),
    Dereference(Box<Rhs>),
    FileScopeVariable(String),
    Literal(Literal),
    Operation(Operation, Variable, Variable),
    FunctionCall(FunctionReference, Vec<Variable>),
    SizeOf(Type),
}

#[derive(Clone)]
pub enum LValue {
    Variable(Variable),
    Dereference(Box<LValue>),
}

pub enum Step {
    NewAssignment(TypedVariable, Rhs),
    /// Variable assignment makes this IR not actually SSA. However, the biggest reason
    /// we're targeting an SSA-like format is so that we can one day emit LLVM. Thankfully,
    /// LLVM has an automatic way of dealing with mutable variables, so we don't have to do
    /// dominance frontier SSA calculations ourselves. Instead, we just let ourselves mutate
    /// variables, easy-peasy.
    Assignment(LValue, Rhs),
    Discarded(Rhs),
}

pub type TypedVariable = (Type, Variable);

pub enum BlockTerminator {
    /// Branch terminators spend some of their time "unfilled" during IR construction
    /// because some block locators aren't known until later.
    Branch(Option<BlockLocator>),
    ConditionalBranch(BlockLocator, BlockLocator, TypedVariable),
    Return(Variable, Type),
}

pub struct Block {
    pub locator: BlockLocator,
    pub assignments: Vec<Step>,
    pub block_terminator: BlockTerminator,
}

pub struct Function {
    pub name: String,
    pub returns: Type,
    pub parameters: Vec<(Type, String)>,
    pub body: Vec<Block>,
}

pub struct CompilationUnit {
    pub functions: Vec<Function>,
}
