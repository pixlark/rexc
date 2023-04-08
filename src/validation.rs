use crate::ast::*;
use crate::parse::Span;
use crate::visitor::*;

#[derive(Debug, Clone)]
pub enum ValidationErrorKind {
    NoReturnStatement,
    HasNoMainFunction,
}

#[derive(Debug, Clone)]
pub struct ValidationError {
    pub kind: ValidationErrorKind,
    pub span: Span,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            ValidationErrorKind::NoReturnStatement => {
                write!(f, "Function has no return statement.")
            }
            ValidationErrorKind::HasNoMainFunction => {
                write!(f, "Compilation unit has no main function.")
            }
        }
    }
}

//
// NoReturnStatement:
//   Checks that every function ends with a return statement.
//   Note that void functions have a return statement generated automatically
//   in the first desugaring phase.
//

struct NoReturnStatement;

impl Visitor<ValidationError> for NoReturnStatement {
    fn visit_function(&mut self, function: &mut Function) -> Result<(), ValidationError> {
        let err = ValidationError {
            kind: ValidationErrorKind::NoReturnStatement,
            span: function.span.clone(),
        };
        let last_stmt = function.body.last().ok_or(err.clone())?;
        match last_stmt.kind {
            StatementKind::Return(..) => Ok(()),
            _ => Err(err),
        }
    }
}

//
// HasMainFunction:
//   Ensures that the program has an entry point called `main`.
//

struct HasMainFunction;

impl Visitor<ValidationError> for HasMainFunction {
    fn visit_file(&mut self, file: &mut File) -> Result<(), ValidationError> {
        for function in file.functions.iter() {
            if &function.name == "main" {
                return Ok(());
            }
        }
        Err(ValidationError {
            kind: ValidationErrorKind::HasNoMainFunction,
            span: Span::Empty,
        })
    }
}

//
// Overall validation phase
//

// TODO(Brooke): Figure out how to make these visitor impls immutable,
//               because `validate` *really* shouldn't have to take
//               a mutable reference here.
impl File {
    pub fn validate(&mut self) -> Result<(), ValidationError> {
        NoReturnStatement.visit_file(self)?;
        HasMainFunction.visit_file(self)?;
        Ok(())
    }
}
