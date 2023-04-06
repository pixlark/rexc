//! The `Visitor` trait allows traversals of the AST to be quickly developed by
//! overriding some of the default `visit_*` implementations

use super::ast::*;

pub trait Visitor: Sized {
    fn visit_lvalue(&mut self, lvalue: &mut LValue) {
        walk_lvalue(self, lvalue);
    }
    fn visit_expression(&mut self, expression: &mut Expression) {
        walk_expression(self, expression);
    }
    fn visit_statement(&mut self, statement: &mut Statement) {
        walk_statement(self, statement);
    }
    fn visit_type(&mut self, type_: &mut Type) {
        walk_type(self, type_);
    }
    fn visit_function(&mut self, function: &mut Function) {
        walk_function(self, function);
    }
    fn visit_data_type(&mut self, data_type: &mut DataType) {
        walk_data_type(self, data_type);
    }
    fn visit_file(&mut self, file: &mut File) {
        walk_file(self, file);
    }
}

pub fn walk_lvalue<V: Visitor>(visitor: &mut V, lvalue: &mut LValue) {
    match &mut lvalue.kind {
        LValueKind::Identifier(..) => {}
        LValueKind::Dereference(inner) => {
            visitor.visit_lvalue(inner);
        }
        LValueKind::FieldAccess {
            lhs,
            field: _,
            needs_dereference: _,
        } => {
            visitor.visit_lvalue(lhs);
        }
    }
}

pub fn walk_expression<V: Visitor>(visitor: &mut V, expression: &mut Expression) {
    match &mut expression.kind {
        ExpressionKind::Unit
        | ExpressionKind::Nil
        | ExpressionKind::Literal(..)
        | ExpressionKind::Variable(..) => {}
        ExpressionKind::Dereference(inner) => {
            visitor.visit_expression(inner);
        }
        ExpressionKind::Operation(_, lhs, rhs) => {
            let (lhs_type, lhs) = lhs.as_mut();
            let (rhs_type, rhs) = rhs.as_mut();
            if let Some(t) = lhs_type {
                visitor.visit_type(t);
            }
            if let Some(t) = rhs_type {
                visitor.visit_type(t);
            }
            visitor.visit_expression(lhs);
            visitor.visit_expression(rhs);
        }
        ExpressionKind::FunctionCall(type_, _, arguments) => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            for (arg_type, arg) in arguments {
                if let Some(t) = arg_type {
                    visitor.visit_type(t);
                }
                visitor.visit_expression(arg);
            }
        }
        ExpressionKind::Allocate(ite) => {
            let (type_, expression) = ite.as_mut();
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            visitor.visit_expression(expression);
        }
        ExpressionKind::New((type_, New { name: _, fields })) => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            for (_, field_expression) in fields {
                visitor.visit_expression(field_expression);
            }
        }
        ExpressionKind::FieldAccess {
            type_,
            lhs,
            field: _,
            needs_dereference: _,
        } => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            let (lhs_type, lhs) = lhs.as_mut();
            if let Some(t) = lhs_type {
                visitor.visit_type(t);
            }
            visitor.visit_expression(lhs);
        }
    }
}

pub fn walk_statement<V: Visitor>(visitor: &mut V, statement: &mut Statement) {
    match &mut statement.kind {
        StatementKind::BareExpression((type_, expression)) => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            visitor.visit_expression(expression);
        }
        StatementKind::MakeVariable(MakeVariable { type_, lhs: _, rhs }) => {
            visitor.visit_type(type_);
            visitor.visit_expression(rhs);
        }
        StatementKind::SetVariable(SetVariable {
            lhs,
            rhs: (rhs_type, rhs),
        }) => {
            visitor.visit_lvalue(lhs);
            if let Some(t) = rhs_type {
                visitor.visit_type(t);
            }
            visitor.visit_expression(rhs);
        }
        StatementKind::Return((type_, expression)) => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            visitor.visit_expression(expression);
        }
        StatementKind::If(If {
            condition: (cond_type, cond),
            body,
        }) => {
            if let Some(t) = cond_type {
                visitor.visit_type(t);
            }
            visitor.visit_expression(cond);
            for statement in body {
                visitor.visit_statement(statement);
            }
        }
        StatementKind::Loop(body) => {
            for statement in body {
                visitor.visit_statement(statement);
            }
        }
        StatementKind::Break => {}
        StatementKind::Print((type_, expression)) => {
            if let Some(t) = type_ {
                visitor.visit_type(t);
            }
            visitor.visit_expression(expression);
        }
    }
}

pub fn walk_type<V: Visitor>(_visitor: &mut V, _type: &mut Type) {
    //! TODO(Brooke): If we want this visitor to be recursive, how do we
    //!               do that without violating the `RefCell` invariant?
}

pub fn walk_function<V: Visitor>(visitor: &mut V, function: &mut Function) {
    for (parameter_type, _) in function.parameters.iter_mut() {
        visitor.visit_type(parameter_type);
    }
    visitor.visit_type(&mut function.returns);
    for statement in function.body.iter_mut() {
        visitor.visit_statement(statement);
    }
}

pub fn walk_data_type<V: Visitor>(visitor: &mut V, data_type: &mut DataType) {
    for (field_type, _) in data_type.fields.iter_mut() {
        visitor.visit_type(field_type);
    }
}

pub fn walk_file<V: Visitor>(visitor: &mut V, file: &mut File) {
    for data_type_rc in file.data_types.iter() {
        let mut data_type_ref = data_type_rc.as_ref().borrow_mut();
        visitor.visit_data_type(&mut data_type_ref);
    }
    for function in file.functions.iter_mut() {
        visitor.visit_function(function);
    }
}
