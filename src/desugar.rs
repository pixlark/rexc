use crate::ast::*;
use crate::ice::{InternalCompilerError::*, *};
use crate::parse::Span;
use crate::visitor::*;

//
// elided_void_returns:
//  Automatic insertion of `return` at the end of void functions
//

impl File {
    fn elided_void_returns(&mut self) {
        for function in self.functions.iter_mut() {
            if matches!(function.returns, Type::Unit) {
                let elided = function.body.is_empty()
                    || !matches!(
                        function.body.last(),
                        Some(Statement {
                            kind: StatementKind::Return(..),
                            ..
                        })
                    );
                if elided {
                    function.body.push(Statement::new(
                        StatementKind::Return((
                            None,
                            Expression::new(ExpressionKind::Unit, Span::Empty),
                        )),
                        Span::Empty,
                    ));
                }
            }
        }
    }
}

//
// AutomaticDereferenceFieldAccess:
//  If field access is done on a pointer, automatically inserts a dereference
//  `ptr.field -> (at ptr).field`
//

struct AutomaticDereferenceFieldAccess;

impl Visitor<()> for AutomaticDereferenceFieldAccess {
    fn visit_expression(&mut self, expression: &mut Expression) -> Result<(), ()> {
        if let ExpressionKind::FieldAccess {
            type_: _,
            lhs,
            field: _,
            needs_dereference,
        } = &mut expression.kind
        {
            if needs_dereference.unwrap_with_ice(UnfilledTypecheckerData) {
                *needs_dereference = Some(false);
                replace_with::replace_with(
                    lhs,
                    || Box::new((None, Dummy::dummy())),
                    |lhs| {
                        let (lhs_type, lhs_expression) = *lhs;
                        let lhs_type = lhs_type.unwrap_with_ice(UnfilledTypecheckerData);
                        let lhs_inner_type = match lhs_type {
                            Type::Pointer(inner) => *inner,
                            _ => ice_error!(InvalidTypecheckerData),
                        };
                        Box::new((
                            Some(lhs_inner_type),
                            Expression {
                                kind: ExpressionKind::Dereference(Box::new(lhs_expression)),
                                span: Span::Empty,
                            },
                        ))
                    },
                );
            }
        }
        walk_expression(self, expression)
    }
    fn visit_lvalue(&mut self, lvalue: &mut LValue) -> Result<(), ()> {
        if let LValueKind::FieldAccess {
            lhs,
            field: _,
            needs_dereference,
        } = &mut lvalue.kind
        {
            if needs_dereference.unwrap_with_ice(UnfilledTypecheckerData) {
                *needs_dereference = Some(false);
                replace_with::replace_with(
                    lhs,
                    || Box::new(Dummy::dummy()),
                    |lhs| {
                        let inner_type =
                            match lhs.type_.as_ref().unwrap_with_ice(UnfilledTypecheckerData) {
                                Type::Pointer(inner) => *inner.clone(),
                                _ => ice_error!(InvalidTypecheckerData),
                            };
                        Box::new(LValue {
                            kind: LValueKind::Dereference(lhs),
                            type_: Some(inner_type),
                        })
                    },
                );
            }
        }
        walk_lvalue(self, lvalue)
    }
}

//
// WhileLoopDesugar:
//  Transform a `while` loop into a `loop` with `if`.
//

struct WhileLoopDesugar;

impl Visitor<()> for WhileLoopDesugar {
    fn visit_statement(&mut self, statement: &mut Statement) -> Result<(), ()> {
        if let StatementKind::While { .. } = &mut statement.kind {
            replace_with::replace_with(statement, Dummy::dummy, |statement| {
                let (condition, body) = match statement.kind {
                    StatementKind::While { condition, body } => (condition, body),
                    _ => ice_unreachable!(),
                };
                let new_body = vec![Statement {
                    kind: StatementKind::If(If {
                        condition,
                        body,
                        else_: vec![Statement {
                            kind: StatementKind::Break,
                            span: Span::Empty,
                        }],
                    }),
                    span: statement.span.clone(),
                }];
                Statement {
                    kind: StatementKind::Loop(new_body),
                    span: statement.span,
                }
            })
        }
        walk_statement(self, statement)
    }
}

//
// Overall desugar phase
//

impl File {
    pub fn desugar_pre_typecheck(&mut self) {
        self.elided_void_returns();
        let _ = WhileLoopDesugar.visit_file(self);
    }
    pub fn desugar_post_typecheck(&mut self) {
        let _ = AutomaticDereferenceFieldAccess.visit_file(self);
    }
}

//
// Dummy values for the AST
//   These allow us to perform AST modifications in-place. If a panic happens
//   during modification, then this dummy value is left in its place to prevent
//   the value from being dropped twice.
//

trait Dummy {
    fn dummy() -> Self;
}

impl Dummy for Type {
    fn dummy() -> Self {
        Self::Unit
    }
}

impl Dummy for Literal {
    fn dummy() -> Self {
        Self::Int(Default::default())
    }
}

impl Dummy for Expression {
    fn dummy() -> Self {
        Self {
            kind: ExpressionKind::Unit,
            span: Span::Empty,
        }
    }
}

impl Dummy for LValue {
    fn dummy() -> Self {
        Self {
            kind: LValueKind::Identifier(Default::default()),
            type_: None,
        }
    }
}

impl Dummy for Statement {
    fn dummy() -> Self {
        Self {
            kind: StatementKind::Break,
            span: Span::Empty,
        }
    }
}

impl Dummy for Function {
    fn dummy() -> Self {
        Self {
            name: Default::default(),
            parameters: Default::default(),
            returns: Type::dummy(),
            body: Default::default(),
            span: Span::Empty,
        }
    }
}

impl Dummy for DataType {
    fn dummy() -> Self {
        Self {
            name: Default::default(),
            fields: Default::default(),
            span: Span::Empty,
        }
    }
}

impl Dummy for File {
    fn dummy() -> Self {
        Self {
            data_types: Default::default(),
            functions: Default::default(),
        }
    }
}
