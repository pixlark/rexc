use super::ast::*;

#[derive(Debug, Clone)]
pub enum ValidationErrorKind {
    NoReturnStatement,
}

#[derive(Debug, Clone)]
pub struct ValidationError {
    kind: ValidationErrorKind,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            ValidationErrorKind::NoReturnStatement => {
                write!(f, "Function has no return statement!")
            }
        }
    }
}

impl Function {
    fn no_return_statement(&self) -> Result<(), ValidationError> {
        let err = ValidationError {
            kind: ValidationErrorKind::NoReturnStatement,
        };
        let last_stmt = self.body.last().ok_or(err.clone())?;
        match last_stmt {
            Statement::Return(..) => Ok(()),
            _ => Err(err),
        }
    }
    pub fn validate(&self) -> Result<(), ValidationError> {
        self.no_return_statement()?;
        Ok(())
    }
}

impl File {
    pub fn validate(&self) -> Result<(), ValidationError> {
        for function in self.functions.iter() {
            function.validate()?;
        }
        Ok(())
    }
}
