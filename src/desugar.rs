use super::ast::*;

impl File {
    fn elided_void_returns(&mut self) {
        for function in self.functions.iter_mut() {
            if matches!(function.returns, Type::Unit) {
                let elided = function.body.is_empty()
                    || !matches!(function.body.last(), Some(Statement::Return(..)));
                if elided {
                    function
                        .body
                        .push(Statement::Return((None, Expression::Unit)));
                }
            }
        }
    }

    pub fn desugar(&mut self) {
        self.elided_void_returns();
    }
}
