//! The typechecking phase does two things - firstly, it fills in all of the
//! `InferredType` fields on the AST that are left empty by the parser. Secondly,
//! it checks the types as it goes to ensure that all typing in the program is
//! consistent (no type errors).

use super::ast;
use super::ir;
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeErrorKind {
    BadVariableDeclarationType(String),
    BadReturnType,
    IncompatibleOperands(ir::Operation),
    UnboundVariable(String),
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            TypeErrorKind::BadVariableDeclarationType(var) => write!(
                f,
                "Variable {}'s declared type does not match its actual type.",
                var
            ),
            TypeErrorKind::BadReturnType => {
                write!(f, "Returned expression's type does not match return type.")
            }
            TypeErrorKind::IncompatibleOperands(op) => {
                write!(
                    f,
                    "Operation {:?} has incompatible left/right operands.",
                    op
                )
            }
            TypeErrorKind::UnboundVariable(var) => {
                write!(f, "Variable {} is used but was never bound.", var)
            }
        }
    }
}

pub type TypeMap = HashMap<String, ast::Type>;

impl ir::Operation {
    fn accepts(&self, left: &ast::Type, right: &ast::Type) -> bool {
        match self {
            Self::Add => matches!((left, right), (ast::Type::Int, ast::Type::Int)),
        }
    }
    fn produce(&self, lhs: &ast::Type, rhs: &ast::Type) -> ast::Type {
        match self {
            Self::Add => match (lhs, rhs) {
                (ast::Type::Int, ast::Type::Int) => ast::Type::Int,
                _ => unreachable!(),
            },
        }
    }
}

impl ast::Expression {
    // TODO(Brooke): This should not be &mut
    fn typecheck(&mut self, type_map: &mut TypeMap) -> Result<ast::Type, TypeError> {
        match self {
            ast::Expression::Literal(lit) => match lit {
                ast::Literal::Int(_) => Ok(ast::Type::Int),
                ast::Literal::Bool(_) => Ok(ast::Type::Bool),
            },
            ast::Expression::Operation(op, lhs, rhs) => {
                let infer_left = lhs.typecheck(type_map)?;
                let infer_right = rhs.typecheck(type_map)?;
                if !op.accepts(&infer_left, &infer_right) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleOperands(*op),
                    });
                }
                let produced_type = op.produce(&infer_left, &infer_right);
                Ok(produced_type)
            }
            ast::Expression::Variable(name) => match type_map.get(name) {
                Some(type_) => Ok(*type_),
                None => Err(TypeError {
                    kind: TypeErrorKind::UnboundVariable(name.clone()),
                }),
            },
        }
    }
}

impl ast::Statement {
    fn typecheck(
        &mut self,
        type_map: &mut TypeMap,
        function_returns: ast::Type,
    ) -> Result<(), TypeError> {
        match self {
            ast::Statement::MakeVariable(ast::MakeVariable { type_, lhs, rhs }) => {
                let infer_type = rhs.typecheck(type_map)?;
                if infer_type != *type_ {
                    return Err(TypeError {
                        kind: TypeErrorKind::BadVariableDeclarationType(lhs.clone()),
                    });
                }
                type_map.insert(lhs.clone(), *type_);
            }
            ast::Statement::Return((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map)?;
                if infer_type != function_returns {
                    return Err(TypeError {
                        kind: TypeErrorKind::BadReturnType,
                    });
                }
                *unfilled_type = Some(infer_type);
            }
            ast::Statement::If(ast::If {
                condition: (unfilled_type, expression),
                body,
            }) => {
                let infer_type = expression.typecheck(type_map)?;
                // TODO: This should give an error if the condition expression isn't a bool
                *unfilled_type = Some(infer_type);
                for statement in body {
                    statement.typecheck(type_map, function_returns)?;
                }
            }
            ast::Statement::Print((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map)?;
                *unfilled_type = Some(infer_type);
            }
        }
        Ok(())
    }
}

impl ast::Function {
    pub fn typecheck(&mut self) -> Result<(), TypeError> {
        let mut type_map = TypeMap::new();

        // Add parameter types
        type_map.extend(self.parameters.iter().map(|(t, s)| (s.clone(), *t)));

        for statement in self.body.iter_mut() {
            statement.typecheck(&mut type_map, self.returns)?;
        }

        Ok(())
    }
}
