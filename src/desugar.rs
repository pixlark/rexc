use super::ast::*;
use super::new_parser::Span;

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

    pub fn desugar(&mut self) {
        self.elided_void_returns();
    }
}
