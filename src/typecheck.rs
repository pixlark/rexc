//! The typechecking phase does two things - firstly, it fills in all of the
//! `InferredType` fields on the AST that are left empty by the parser. Secondly,
//! it checks the types as it goes to ensure that all typing in the program is
//! consistent (no type errors).

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::ast;
use super::internal_error::*;
use super::ir;
use super::parse::Span;

#[derive(Debug)]
pub enum TypeErrorKind {
    BadVariableDeclarationType(String),
    BadReturnType,
    IncompatibleOperands(ir::Operation),
    UnboundVariable(String),
    AssignedWrongTypeToVariable(ast::LValue),
    CalledNonFunction(String),
    WrongArgumentCount(String),
    WrongArgumentType(String, usize),
    DereferencedNonPointer,
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
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
            TypeErrorKind::AssignedWrongTypeToVariable(lhs) => {
                write!(
                    f,
                    "Tried to assign an expression of the wrong type to lvalue {:?}",
                    lhs
                )
            }
            TypeErrorKind::CalledNonFunction(var) => {
                write!(f, "Tried to call {}, which is not a function.", var)
            }
            TypeErrorKind::WrongArgumentCount(func) => {
                write!(f, "Called {} with the wrong number of arguments.", func)
            }
            TypeErrorKind::WrongArgumentType(name, arg_index) => {
                write!(
                    f,
                    "Argument #{} in call to {} is the incorrect type.",
                    arg_index, name
                )
            }
            TypeErrorKind::DereferencedNonPointer => {
                write!(f, "Tried to dereference a non-pointer type.")
            }
        }
    }
}

pub struct TypeMap(Vec<HashMap<String, ast::Type>>);

impl TypeMap {
    fn new() -> TypeMap {
        TypeMap(Vec::new())
    }
    fn get(&self, name: &str) -> Option<ast::Type> {
        for scope in self.0.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }
    fn bind(&mut self, name: String, type_: ast::Type) -> Option<()> {
        let scope = self.0.last_mut()?;
        if scope.get(&name).is_some() {
            return None;
        }
        scope.insert(name, type_);
        Some(())
    }
    fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.0.pop();
    }
}

impl ir::Operation {
    fn accepts(&self, left: &ast::Type, right: &ast::Type) -> bool {
        match self {
            Self::Add
            | Self::Subtract
            | Self::Multiply
            | Self::Divide
            | Self::LessThan
            | Self::GreaterThan
            | Self::LessThanOrEqualTo
            | Self::GreaterThanOrEqualTo => {
                matches!((left, right), (ast::Type::Int, ast::Type::Int))
            }
            Self::Equals | Self::NotEquals => matches!(
                (left, right),
                (ast::Type::Int, ast::Type::Int) | (ast::Type::Bool, ast::Type::Bool)
            ),
        }
    }
    fn produce(&self, lhs: &ast::Type, rhs: &ast::Type) -> ast::Type {
        match self {
            Self::Add | Self::Subtract | Self::Multiply | Self::Divide => match (lhs, rhs) {
                (ast::Type::Int, ast::Type::Int) => ast::Type::Int,
                _ => unreachable!(),
            },
            Self::LessThan
            | Self::GreaterThan
            | Self::LessThanOrEqualTo
            | Self::GreaterThanOrEqualTo => match (lhs, rhs) {
                (ast::Type::Int, ast::Type::Int) => ast::Type::Bool,
                _ => unreachable!(),
            },
            Self::Equals | Self::NotEquals => match (lhs, rhs) {
                (ast::Type::Int, ast::Type::Int) => ast::Type::Bool,
                (ast::Type::Bool, ast::Type::Bool) => ast::Type::Bool,
                _ => unreachable!(),
            },
        }
    }
}

impl ast::Expression {
    fn get_deref(&self) -> Result<Rc<RefCell<ast::Expression>>, TypeError> {
        match &self.kind {
            ast::ExpressionKind::Dereference(1, interior) => Ok(interior.clone()),
            ast::ExpressionKind::Dereference(n, interior) => {
                rexc_assert(*n > 1);
                Ok(Rc::new(RefCell::new(ast::Expression::new(
                    ast::ExpressionKind::Dereference(*n - 1, interior.clone()),
                    self.span.clone(),
                ))))
            }
            _ => Err(TypeError {
                kind: TypeErrorKind::DereferencedNonPointer,
                span: self.span.clone(),
            }),
        }
    }
    fn typecheck(&mut self, type_map: &mut TypeMap) -> Result<ast::Type, TypeError> {
        match &mut self.kind {
            ast::ExpressionKind::Unit => Ok(ast::Type::Unit),
            ast::ExpressionKind::Literal(lit) => match lit {
                ast::Literal::Int(_) => Ok(ast::Type::Int),
                ast::Literal::Bool(_) => Ok(ast::Type::Bool),
            },
            ast::ExpressionKind::Operation(op, lhs, rhs) => {
                let (lhs_unfilled_type, lhs) = lhs.as_mut();
                let (rhs_unfilled_type, rhs) = rhs.as_mut();

                let infer_left = lhs.typecheck(type_map)?;
                let infer_right = rhs.typecheck(type_map)?;

                if !op.accepts(&infer_left, &infer_right) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleOperands(*op),
                        span: self.span.clone(),
                    });
                }

                *lhs_unfilled_type = Some(infer_left.clone());
                *rhs_unfilled_type = Some(infer_right.clone());

                let produced_type = op.produce(&infer_left, &infer_right);
                Ok(produced_type)
            }
            ast::ExpressionKind::Variable(name) => match type_map.get(name) {
                Some(type_) => Ok(type_),
                None => Err(TypeError {
                    kind: TypeErrorKind::UnboundVariable(name.clone()),
                    span: self.span.clone(),
                }),
            },
            ast::ExpressionKind::Dereference(..) => {
                let derefed_expression = self.get_deref()?;
                let derefed_type = derefed_expression.borrow_mut().typecheck(type_map)?;
                match derefed_type {
                    ast::Type::Pointer(ptr_interior_type) => Ok(*ptr_interior_type),
                    _ => Err(TypeError {
                        kind: TypeErrorKind::DereferencedNonPointer,
                        span: self.span.clone(),
                    }),
                }
            }
            ast::ExpressionKind::FunctionCall(unfilled_type, name, arguments) => {
                let type_ = type_map.get(name).ok_or(TypeError {
                    kind: TypeErrorKind::UnboundVariable(name.clone()),
                    span: self.span.clone(),
                })?;
                match type_ {
                    ast::Type::Function(rc) => {
                        // Typecheck argument list
                        let arg_types = &rc.1;
                        if arguments.len() != arg_types.len() {
                            return Err(TypeError {
                                kind: TypeErrorKind::WrongArgumentCount(name.clone()),
                                span: self.span.clone(),
                            });
                        }
                        for (i, ((unfilled_type, arg), expected_type)) in
                            arguments.iter_mut().zip(arg_types).enumerate()
                        {
                            let arg_type = arg.typecheck(type_map)?;
                            if arg_type != *expected_type {
                                return Err(TypeError {
                                    kind: TypeErrorKind::WrongArgumentType(name.clone(), i),
                                    span: self.span.clone(),
                                });
                            }
                            *unfilled_type = Some(arg_type.clone());
                        }

                        let returns = &rc.0;
                        *unfilled_type = Some(returns.clone());
                        Ok(returns.clone())
                    }
                    _ => Err(TypeError {
                        kind: TypeErrorKind::CalledNonFunction(name.clone()),
                        span: self.span.clone(),
                    }),
                }
            }
            ast::ExpressionKind::Allocate(ite) => {
                let (unfilled_type, expression) = ite.as_mut();
                let inferred_type = expression.typecheck(type_map)?;
                *unfilled_type = Some(inferred_type.clone());
                Ok(ast::Type::Pointer(Box::new(inferred_type)))
            }
        }
    }
}

impl ast::LValue {
    fn get_deref(&self, span: Span) -> Result<ast::LValue, TypeError> {
        if self.derefs == 0 {
            Err(TypeError {
                kind: TypeErrorKind::DereferencedNonPointer,
                span,
            })
        } else {
            Ok(ast::LValue {
                name: self.name.clone(),
                derefs: self.derefs - 1,
            })
        }
    }
    fn typecheck(&mut self, type_map: &TypeMap, span: Span) -> Result<ast::Type, TypeError> {
        // TODO(Brooke): Evaluate this `.unwrap()`
        if self.derefs == 0 {
            Ok(type_map.get(&self.name).unwrap())
        } else {
            let mut derefed_lvalue = self.get_deref(span.clone())?;
            let derefed_lvalue_type = derefed_lvalue.typecheck(type_map, span.clone())?;
            match derefed_lvalue_type {
                ast::Type::Pointer(interior) => Ok(*interior),
                _ => Err(TypeError {
                    kind: TypeErrorKind::DereferencedNonPointer,
                    span,
                }),
            }
        }
    }
}

impl ast::Statement {
    fn typecheck(
        &mut self,
        type_map: &mut TypeMap,
        function_returns: ast::Type,
    ) -> Result<(), TypeError> {
        match &mut self.kind {
            ast::StatementKind::BareExpression((unfilled_type, expression)) => {
                *unfilled_type = Some(expression.typecheck(type_map)?);
            }
            ast::StatementKind::MakeVariable(ast::MakeVariable { type_, lhs, rhs }) => {
                let infer_type = rhs.typecheck(type_map)?;
                if infer_type != *type_ {
                    return Err(TypeError {
                        kind: TypeErrorKind::BadVariableDeclarationType(lhs.clone()),
                        span: self.span.clone(),
                    });
                }
                type_map.bind(lhs.clone(), type_.clone());
            }
            ast::StatementKind::SetVariable(ast::SetVariable {
                lhs,
                rhs: (unfilled_type, rhs),
            }) => {
                let lhs_type = lhs.typecheck(type_map, self.span.clone())?;
                let rhs_type = rhs.typecheck(type_map)?;
                if lhs_type != rhs_type {
                    return Err(TypeError {
                        kind: TypeErrorKind::AssignedWrongTypeToVariable(lhs.clone()),
                        span: self.span.clone(),
                    });
                }
                *unfilled_type = Some(rhs_type);
            }
            ast::StatementKind::Return((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map)?;
                if infer_type != function_returns {
                    return Err(TypeError {
                        kind: TypeErrorKind::BadReturnType,
                        span: self.span.clone(),
                    });
                }
                *unfilled_type = Some(infer_type);
            }
            ast::StatementKind::If(ast::If {
                condition: (unfilled_type, expression),
                body,
            }) => {
                let infer_type = expression.typecheck(type_map)?;
                // TODO: This should give an error if the condition expression isn't a bool
                *unfilled_type = Some(infer_type);

                type_map.push_scope();
                for statement in body {
                    statement.typecheck(type_map, function_returns.clone())?;
                }
                type_map.pop_scope();
            }
            ast::StatementKind::Loop(body) => {
                type_map.push_scope();
                for statement in body {
                    statement.typecheck(type_map, function_returns.clone())?;
                }
                type_map.pop_scope();
            }
            ast::StatementKind::Break => {}
            ast::StatementKind::Print((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map)?;
                *unfilled_type = Some(infer_type);
            }
        }
        Ok(())
    }
}

impl ast::Function {
    pub fn typecheck(&mut self, type_map: &mut TypeMap) -> Result<(), TypeError> {
        // Function scope
        type_map.push_scope();

        for (t, s) in self.parameters.iter() {
            type_map.bind(s.clone(), t.clone());
        }

        for statement in self.body.iter_mut() {
            statement.typecheck(type_map, self.returns.clone())?;
        }

        Ok(())
    }
}

impl ast::File {
    pub fn typecheck(&mut self) -> Result<(), TypeError> {
        let mut type_map = TypeMap::new();
        type_map.push_scope(); // File scope

        // Because we allow out-of-order definition of functions,
        // we need to do two passes: One to fill up our file scope
        // with function types, and the second to do our actual
        // full typechecking
        for ast::Function {
            name,
            parameters,
            returns,
            ..
        } in self.functions.iter_mut()
        {
            type_map.bind(
                name.clone(),
                ast::Type::Function(Rc::new((
                    returns.clone(),
                    parameters.iter().map(|(t, _)| t.clone()).collect(),
                ))),
            );
        }

        // This is the *full* typechecking pass
        for function in self.functions.iter_mut() {
            function.typecheck(&mut type_map)?;
        }

        Ok(())
    }
}
