use super::ast::*;
use super::parse::Span;

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
// automatic_dereference_field_access:
//  If field access is done on a pointer, automatically inserts a dereference
//  `ptr.field -> (at ptr).field`
//

// TODO(Brooke): Implement this with Visitor once that's done

//
// Overall desugar phase
//

impl File {
    pub fn desugar(&mut self) {
        self.elided_void_returns();
        //self.automatic_dereference_field_access();
    }
}
