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
    IncompatibleOperand,
    UnboundVariable(String),
    AssignedWrongTypeToVariable(ast::LValue),
    CalledNonFunction(String),
    WrongArgumentCount(String),
    WrongArgumentType(String, usize),
    DereferencedNonPointer,
    NoSuchDataType(String),
    WrongFieldCount(String, usize),
    BadFieldType(String, String),
    NoSuchField(String, String),
    BadFieldAccess,
    ShadowedVariable(String),
    MutatedNonLocalVariable(String),
    RecursiveDataType,
    ConditionNotBool,
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
            TypeErrorKind::IncompatibleOperand => {
                write!(f, "Incompatible operand(s) supplied to operator.")
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
            TypeErrorKind::NoSuchDataType(name) => {
                write!(f, "Referenced nonexistent data type {}.", name)
            }
            TypeErrorKind::WrongFieldCount(name, count) => {
                write!(
                    f,
                    "Wrong number of fields supplied to {}, which has {}.",
                    name, count
                )
            }
            TypeErrorKind::BadFieldType(name, field) => {
                write!(f, "Field {} of type {} has incorrect type.", field, name)
            }
            TypeErrorKind::NoSuchField(name, field) => {
                write!(f, "Type {} has no field called {}.", name, field)
            }
            TypeErrorKind::BadFieldAccess => {
                write!(f, "Tried to perform a field access ('.') on something that's not a data structure.")
            }
            TypeErrorKind::ShadowedVariable(name) => {
                write!(f, "Shadowed {}. For ease of implementation, shadowing variables is currently illegal. This is subject to change.", name)
            }
            TypeErrorKind::MutatedNonLocalVariable(name) => {
                write!(f, "Tried to mutate {}, which is not a local variable! File-scope variables are immutable.", name)
            }
            TypeErrorKind::RecursiveDataType => {
                write!(f, "Type is recursive and has infinite size! (Try using a pointer for recursive references.)")
            }
            TypeErrorKind::ConditionNotBool => {
                write!(f, "Statement condition is not a bool.")
            }
        }
    }
}

#[derive(Copy, Clone)]
enum BindingKind {
    Local,
    Parameter,
    FileScope,
}

#[derive(Clone)]
struct Binding {
    kind: BindingKind,
    type_: ast::Type,
}

pub struct NameMap(Vec<HashMap<String, Binding>>);

impl NameMap {
    fn new() -> NameMap {
        NameMap(Vec::new())
    }
    fn get(&self, name: &str) -> Option<Binding> {
        for scope in self.0.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }
    fn bind(&mut self, kind: BindingKind, name: String, type_: ast::Type) -> Option<()> {
        let scope = self.0.last_mut()?;
        if scope.get(&name).is_some() {
            return None;
        }
        scope.insert(name, Binding { kind, type_ });
        Some(())
    }
    fn push_scope(&mut self) {
        self.0.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.0.pop();
    }
}

#[derive(Clone)]
enum TypeBinding {
    Incomplete,
    Complete(Rc<RefCell<ast::DataType>>),
}

pub struct TypeMap(HashMap<String, TypeBinding>);

impl TypeMap {
    fn new() -> TypeMap {
        TypeMap(HashMap::new())
    }
    fn get(&self, name: &str) -> Option<TypeBinding> {
        self.0.get(name).cloned()
    }
    fn bind(&mut self, name: String) -> Option<()> {
        if self.0.get(&name).is_some() {
            return None;
        }
        self.0.insert(name, TypeBinding::Incomplete);
        Some(())
    }
    fn fill_binding(&mut self, name: &str, data_type: Rc<RefCell<ast::DataType>>) -> Option<()> {
        let binding = self.0.get(name);
        match binding {
            Some(TypeBinding::Incomplete) => {
                self.0
                    .insert(String::from(name), TypeBinding::Complete(data_type));
                Some(())
            }
            None | Some(TypeBinding::Complete(..)) => None,
        }
    }
}

/// Parts of typechecking may require more than one pass to resolve. These pieces
/// return a `TypedStatus`, indicating whether another pass needs to be made in order
/// to completely fill the descendant type annotations.
#[must_use]
#[derive(Copy, Clone)]
enum TypedStatus {
    Ok,
    Incomplete,
}

impl TypedStatus {
    fn unwrap(self) {
        match self {
            TypedStatus::Ok => {}
            TypedStatus::Incomplete => {
                rexc_panic("Had an incomplete typing pass when it was assumed that was impossible!")
            }
        }
    }
}

impl ast::Type {
    fn typecheck(&mut self, type_map: &TypeMap, span: &Span) -> Result<TypedStatus, TypeError> {
        match self {
            ast::Type::Named((name, unfilled_data_type)) => match type_map.get(name) {
                Some(TypeBinding::Complete(t)) => {
                    *unfilled_data_type = Some(t);
                    Ok(TypedStatus::Ok)
                }
                Some(TypeBinding::Incomplete) => Ok(TypedStatus::Incomplete),
                None => Err(TypeError {
                    kind: TypeErrorKind::NoSuchDataType(name.clone()),
                    span: span.clone(),
                }),
            },
            ast::Type::Pointer(inner) => inner.typecheck(type_map, span),
            _ => Ok(TypedStatus::Ok),
        }
    }
}

impl ir::UnaryOperation {
    fn accepts(&self, type_: &ast::Type) -> bool {
        match self {
            Self::Not => matches!(type_, ast::Type::Bool),
        }
    }
    fn produce(&self, type_: &ast::Type) -> ast::Type {
        match self {
            Self::Not => match type_ {
                ast::Type::Bool => ast::Type::Bool,
                _ => unreachable!(),
            },
        }
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
            Self::Equals | Self::NotEquals => match (left, right) {
                (ast::Type::Int, ast::Type::Int) => true,
                (ast::Type::Bool, ast::Type::Bool) => true,
                (ast::Type::Pointer(left_inner), ast::Type::Pointer(right_inner)) => {
                    left_inner == right_inner
                }
                (ast::Type::Pointer(..), ast::Type::Nil) => true,
                (ast::Type::Nil, ast::Type::Pointer(..)) => true,
                _ => false,
            },
            Self::And | Self::Or => matches!((left, right), (ast::Type::Bool, ast::Type::Bool)),
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
                (ast::Type::Pointer(..), ast::Type::Pointer(..)) => ast::Type::Bool,
                (ast::Type::Pointer(..), ast::Type::Nil) => ast::Type::Bool,
                (ast::Type::Nil, ast::Type::Pointer(..)) => ast::Type::Bool,
                _ => unreachable!(),
            },
            Self::And | Self::Or => match (lhs, rhs) {
                (ast::Type::Bool, ast::Type::Bool) => ast::Type::Bool,
                _ => unreachable!(),
            },
        }
    }
}

impl ast::DataType {
    fn typecheck(&mut self, type_map: &TypeMap) -> Result<TypedStatus, TypeError> {
        let mut status = TypedStatus::Ok;
        for (field_type, _) in self.fields.iter_mut() {
            // Recursive type?
            if let ast::Type::Named((name, _)) = field_type {
                if name == &self.name {
                    return Err(TypeError {
                        kind: TypeErrorKind::RecursiveDataType,
                        span: self.span.clone(),
                    });
                }
            }
            if let TypedStatus::Incomplete = field_type.typecheck(type_map, &self.span)? {
                status = TypedStatus::Incomplete;
            }
        }
        Ok(status)
    }
    fn check_field_access(&self, field: &str) -> Result<ast::Type, TypeError> {
        let (type_, _) = match self.fields.iter().find(|(_, name)| name == field) {
            Some(f) => f,
            None => {
                return Err(TypeError {
                    kind: TypeErrorKind::NoSuchField(self.name.clone(), String::from(field)),
                    span: self.span.clone(),
                })
            }
        };
        Ok(type_.clone())
    }
}

impl ast::Expression {
    fn typecheck(
        &mut self,
        type_map: &TypeMap,
        name_map: &mut NameMap,
    ) -> Result<ast::Type, TypeError> {
        match &mut self.kind {
            ast::ExpressionKind::Unit => Ok(ast::Type::Unit),
            ast::ExpressionKind::Nil => Ok(ast::Type::Nil),
            ast::ExpressionKind::Literal(lit) => match lit {
                ast::Literal::Int(_) => Ok(ast::Type::Int),
                ast::Literal::Bool(_) => Ok(ast::Type::Bool),
            },
            ast::ExpressionKind::UnaryOperation(op, inner) => {
                let (unfilled_type, expr) = inner.as_mut();
                let infer = expr.typecheck(type_map, name_map)?;
                if !op.accepts(&infer) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleOperand,
                        span: self.span.clone(),
                    });
                }
                *unfilled_type = Some(infer.clone());
                let produced_type = op.produce(&infer);
                Ok(produced_type)
            }
            ast::ExpressionKind::Operation(op, lhs, rhs) => {
                let (lhs_unfilled_type, lhs) = lhs.as_mut();
                let (rhs_unfilled_type, rhs) = rhs.as_mut();

                let infer_left = lhs.typecheck(type_map, name_map)?;
                let infer_right = rhs.typecheck(type_map, name_map)?;

                if !op.accepts(&infer_left, &infer_right) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleOperand,
                        span: self.span.clone(),
                    });
                }

                *lhs_unfilled_type = Some(infer_left.clone());
                *rhs_unfilled_type = Some(infer_right.clone());

                let produced_type = op.produce(&infer_left, &infer_right);
                Ok(produced_type)
            }
            ast::ExpressionKind::Variable(name) => match name_map.get(name) {
                Some(Binding { type_, .. }) => Ok(type_),
                None => Err(TypeError {
                    kind: TypeErrorKind::UnboundVariable(name.clone()),
                    span: self.span.clone(),
                }),
            },
            ast::ExpressionKind::Dereference(derefed_expression) => {
                let derefed_type = derefed_expression.typecheck(type_map, name_map)?;
                match derefed_type {
                    ast::Type::Pointer(ptr_interior_type) => Ok(*ptr_interior_type),
                    _ => Err(TypeError {
                        kind: TypeErrorKind::DereferencedNonPointer,
                        span: self.span.clone(),
                    }),
                }
            }
            ast::ExpressionKind::FunctionCall(unfilled_type, name, arguments) => {
                let binding = name_map.get(name).ok_or(TypeError {
                    kind: TypeErrorKind::UnboundVariable(name.clone()),
                    span: self.span.clone(),
                })?;
                match binding.type_ {
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
                            let arg_type = arg.typecheck(type_map, name_map)?;
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
                let inferred_type = expression.typecheck(type_map, name_map)?;
                *unfilled_type = Some(inferred_type.clone());
                Ok(ast::Type::Pointer(Box::new(inferred_type)))
            }
            ast::ExpressionKind::New((unfilled_type, ast::New { name, fields })) => {
                let type_ = match type_map.get(name) {
                    Some(TypeBinding::Complete(t)) => t,
                    Some(TypeBinding::Incomplete) => rexc_panic("Somehow an incomplete type binding got past the `data` typechecking phase!"),
                    None => {
                        return Err(TypeError {
                            kind: TypeErrorKind::NoSuchDataType(name.clone()),
                            span: self.span.clone(),
                        })
                    }
                };
                let full_type = ast::Type::Named((name.clone(), Some(type_.clone())));
                *unfilled_type = Some(full_type.clone());
                if fields.len() != type_.borrow().fields.len() {
                    return Err(TypeError {
                        kind: TypeErrorKind::WrongFieldCount(
                            name.clone(),
                            type_.borrow().fields.len(),
                        ),
                        span: self.span.clone(),
                    });
                }
                for (field_name, field_expr) in fields {
                    let type_ref = type_.borrow();
                    let mut find_iter = type_ref.fields.iter().filter(|(_, s)| field_name == s);
                    match find_iter.next() {
                        Some((field_type, _)) => {
                            let inferred_type = field_expr.typecheck(type_map, name_map)?;
                            if inferred_type != *field_type {
                                return Err(TypeError {
                                    kind: TypeErrorKind::BadFieldType(
                                        name.clone(),
                                        field_name.clone(),
                                    ),
                                    span: self.span.clone(),
                                });
                            }
                        }
                        None => {
                            return Err(TypeError {
                                kind: TypeErrorKind::NoSuchField(name.clone(), field_name.clone()),
                                span: self.span.clone(),
                            })
                        }
                    }
                }
                Ok(full_type)
            }
            ast::ExpressionKind::FieldAccess {
                type_: unfilled_type,
                lhs,
                field,
                needs_dereference,
            } => {
                let lhs_unfilled_type = &mut lhs.0;
                let lhs = &mut lhs.1;

                let lhs_type = lhs.typecheck(type_map, name_map)?;
                let mut lhs_check_type = &lhs_type;

                // Check if we need to do an automatic dereference to access
                // the field behind a pointer.
                *needs_dereference = Some(false);
                if let ast::Type::Pointer(inner) = &lhs_type {
                    lhs_check_type = inner;
                    *needs_dereference = Some(true);
                }

                let data_type = match lhs_check_type {
                    ast::Type::Named((_, data_type)) => data_type.clone(),
                    _ => {
                        return Err(TypeError {
                            kind: TypeErrorKind::BadFieldAccess,
                            span: self.span.clone(),
                        })
                    }
                };

                *lhs_unfilled_type = Some(lhs_type);

                let data_type = data_type.rexc_unwrap("Somehow a field access expression got past the typechecker without having its data_type reference filled in!");
                let type_ = data_type.borrow().check_field_access(field)?;

                *unfilled_type = Some(type_.clone());
                Ok(type_)
            }
        }
    }
}

impl ast::LValue {
    fn typecheck(&mut self, name_map: &NameMap, span: Span) -> Result<ast::Type, TypeError> {
        let type_ = match &mut self.kind {
            ast::LValueKind::Identifier(name) => match name_map.get(name) {
                Some(Binding {
                    kind: BindingKind::Local,
                    type_,
                }) => type_,
                Some(Binding {
                    kind: BindingKind::Parameter,
                    type_,
                }) => type_,
                Some(Binding { .. }) => {
                    return Err(TypeError {
                        kind: TypeErrorKind::MutatedNonLocalVariable(name.clone()),
                        span,
                    })
                }
                None => unreachable!(), // TODO(Brooke): Is it?
            },
            ast::LValueKind::Dereference(inner) => {
                let inner_type = inner.typecheck(name_map, span.clone())?;
                match inner_type {
                    ast::Type::Pointer(ptr_interior) => *ptr_interior,
                    _ => {
                        return Err(TypeError {
                            kind: TypeErrorKind::DereferencedNonPointer,
                            span,
                        })
                    }
                }
            }
            ast::LValueKind::FieldAccess {
                lhs,
                field,
                needs_dereference,
            } => {
                let lhs_type = lhs.typecheck(name_map, span.clone())?;
                let mut lhs_check_type = &lhs_type;

                // Check if we need to automatically dereference to access the field
                *needs_dereference = Some(false);
                if let ast::Type::Pointer(inner) = &lhs_type {
                    lhs_check_type = inner;
                    *needs_dereference = Some(true);
                }

                let data_type = match lhs_check_type {
                    ast::Type::Named((_, data_type)) => data_type.clone(),
                    _ => {
                        return Err(TypeError {
                            kind: TypeErrorKind::BadFieldAccess,
                            span,
                        })
                    }
                };

                let data_type = data_type.rexc_unwrap("Somehow a field access expression got past the typechecker without having its data_type reference filled in!");
                let field_type = data_type.borrow().check_field_access(field)?;
                field_type
            }
        };
        self.type_ = Some(type_.clone());
        Ok(type_)
    }
}

impl ast::Statement {
    fn typecheck(
        &mut self,
        type_map: &TypeMap,
        name_map: &mut NameMap,
        function_returns: ast::Type,
    ) -> Result<(), TypeError> {
        match &mut self.kind {
            ast::StatementKind::BareExpression((unfilled_type, expression)) => {
                *unfilled_type = Some(expression.typecheck(type_map, name_map)?);
            }
            ast::StatementKind::MakeVariable(ast::MakeVariable { type_, lhs, rhs }) => {
                type_.typecheck(type_map, &self.span)?.unwrap();
                let infer_type = rhs.typecheck(type_map, name_map)?;
                if infer_type != *type_ {
                    return Err(TypeError {
                        kind: TypeErrorKind::BadVariableDeclarationType(lhs.clone()),
                        span: self.span.clone(),
                    });
                }
                if name_map
                    .bind(BindingKind::Local, lhs.clone(), type_.clone())
                    .is_none()
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::ShadowedVariable(lhs.clone()),
                        span: self.span.clone(),
                    });
                }
            }
            ast::StatementKind::SetVariable(ast::SetVariable {
                lhs,
                rhs: (unfilled_type, rhs),
            }) => {
                let lhs_type = lhs.typecheck(name_map, self.span.clone())?;
                let rhs_type = rhs.typecheck(type_map, name_map)?;
                if lhs_type != rhs_type {
                    return Err(TypeError {
                        kind: TypeErrorKind::AssignedWrongTypeToVariable(lhs.clone()),
                        span: self.span.clone(),
                    });
                }
                *unfilled_type = Some(rhs_type);
            }
            ast::StatementKind::Return((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map, name_map)?;
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
                else_,
            }) => {
                let infer_type = expression.typecheck(type_map, name_map)?;

                if infer_type != ast::Type::Bool {
                    return Err(TypeError {
                        kind: TypeErrorKind::ConditionNotBool,
                        span: self.span.clone(),
                    });
                }

                *unfilled_type = Some(infer_type);

                name_map.push_scope();
                for statement in body {
                    statement.typecheck(type_map, name_map, function_returns.clone())?;
                }
                name_map.pop_scope();

                name_map.push_scope();
                for statement in else_ {
                    statement.typecheck(type_map, name_map, function_returns.clone())?;
                }
                name_map.pop_scope();
            }
            ast::StatementKind::Loop(body) => {
                name_map.push_scope();
                for statement in body {
                    statement.typecheck(type_map, name_map, function_returns.clone())?;
                }
                name_map.pop_scope();
            }
            ast::StatementKind::While { .. } => should_have_been_desugared(),
            ast::StatementKind::Break => {}
            ast::StatementKind::Print((unfilled_type, expression)) => {
                let infer_type = expression.typecheck(type_map, name_map)?;
                *unfilled_type = Some(infer_type);
            }
        }
        Ok(())
    }
}

impl ast::Function {
    pub fn typecheck(
        &mut self,
        type_map: &TypeMap,
        name_map: &mut NameMap,
    ) -> Result<(), TypeError> {
        // Function scope
        name_map.push_scope();

        for (t, s) in self.parameters.iter_mut() {
            name_map.bind(BindingKind::Parameter, s.clone(), t.clone());
        }

        for statement in self.body.iter_mut() {
            statement.typecheck(type_map, name_map, self.returns.clone())?;
        }

        Ok(())
    }
}

impl ast::File {
    pub fn typecheck(&mut self) -> Result<(), TypeError> {
        let mut type_map = TypeMap::new();

        for data_type in self.data_types.iter_mut() {
            type_map.bind(data_type.borrow().name.clone());
            let typed_status = data_type.borrow_mut().typecheck(&type_map)?;
            type_map.fill_binding(&data_type.borrow().name, data_type.clone());
            if let TypedStatus::Incomplete = typed_status {
                // Recursive data types require a second pass of typechecking
                data_type.borrow_mut().typecheck(&type_map)?.unwrap();
            }
        }

        let mut name_map = NameMap::new();
        name_map.push_scope(); // File scope

        // Because we allow out-of-order definition of functions,
        // we need to do two passes: One to fill up our file scope
        // with function types, and the second to do our actual
        // full typechecking
        for ast::Function {
            name,
            parameters,
            returns,
            span,
            ..
        } in self.functions.iter_mut()
        {
            // For each function, we also need to typecheck the *types* first,
            // which fills all `DataType` type annotations with references to
            // the proper data type.
            for (type_, _) in parameters.iter_mut() {
                type_.typecheck(&type_map, span)?.unwrap();
            }
            returns.typecheck(&type_map, span)?.unwrap();

            // Now that we've filled in those types, we can add to the name map,
            // which will then hold proper data type references (rather than unfilled
            // `None`s which would panic later when comparing types).
            name_map.bind(
                BindingKind::FileScope,
                name.clone(),
                ast::Type::Function(Rc::new((
                    returns.clone(),
                    parameters.iter().map(|(t, _)| t.clone()).collect(),
                ))),
            );
        }

        // This is the *full* typechecking pass
        for function in self.functions.iter_mut() {
            function.typecheck(&type_map, &mut name_map)?;
        }

        Ok(())
    }
}
