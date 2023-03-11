//! Transforms the desugared abstract syntax tree into
//! an intermediate representation that can be emitted
//! by any of the backends.

use std::collections::HashMap;
use std::thread::current;

use super::ast::*;
use super::ir;

struct BlockConstructor {
    index: usize,
    assignments: Vec<ir::Assignment>,
    block_terminator: Option<ir::BlockTerminator>,
}

struct FunctionConstructor {
    name: String,
    returns: Option<ir::Type>,
    parameters: Vec<(ir::Type, String)>,
    body: Vec<BlockConstructor>,
    variable_counter: usize,
    variable_map: HashMap<String, usize>,
}

impl BlockConstructor {
    fn new(index: usize) -> BlockConstructor {
        BlockConstructor {
            index,
            assignments: Vec::new(),
            block_terminator: None,
        }
    }
    fn construct(self) -> ir::Block {
        assert!(self.block_terminator.is_some());
        ir::Block {
            index: self.index,
            assignments: self.assignments,
            block_terminator: self.block_terminator.unwrap(),
        }
    }
}

impl FunctionConstructor {
    fn new(name: String) -> FunctionConstructor {
        FunctionConstructor {
            name,
            returns: None,
            parameters: Vec::new(),
            body: Vec::new(),
            variable_counter: 0,
            variable_map: HashMap::new(),
        }
    }
    fn construct(self) -> ir::Function {
        assert!(self.returns.is_some());
        ir::Function {
            name: self.name,
            returns: self.returns.unwrap(),
            parameters: self.parameters,
            body: self.body.into_iter().map(|b| b.construct()).collect(),
        }
    }
    fn add_block(&mut self) -> usize {
        self.body.push(BlockConstructor::new(self.body.len()));
        self.body.len() - 1
    }
    fn add_assignment(&mut self, block: usize, type_: ir::Type, rhs: ir::Rhs) -> usize {
        let index = self.variable_counter;
        self.variable_counter += 1;

        let block = &mut self.body[block];

        block.assignments.push(ir::Assignment {
            type_,
            lhs: Some(index),
            rhs,
        });

        index
    }
    fn add_return(&mut self, block: usize, type_: ir::Type, var: usize) {
        let block = &mut self.body[block];
        assert!(block.block_terminator.is_none());
        block.block_terminator = Some(ir::BlockTerminator::Return(var, type_));
    }
    fn add_conditional_jump(
        &mut self,
        from_block: usize,
        to_block: usize,
        else_block: usize,
        condition_var: usize,
        condition_type: ir::Type,
    ) {
        let from_block = &mut self.body[from_block];
        assert!(from_block.block_terminator.is_none());
        from_block.block_terminator = Some(ir::BlockTerminator::ConditionalBranch(
            to_block,
            else_block,
            (condition_type, condition_var),
        ));
    }
    fn add_void_function_call(&mut self, block: usize, name: String, arguments: Vec<usize>) {
        let block = &mut self.body[block];
        let rhs = ir::Rhs::FunctionCall(name, arguments);
        block.assignments.push(ir::Assignment {
            type_: ir::Type::Void,
            lhs: None,
            rhs,
        });
    }
}

impl Type {
    fn to_ir(self) -> ir::Type {
        match self {
            Type::Int => ir::Type::Int,
            Type::Bool => ir::Type::Int,
        }
    }
}

impl Expression {
    fn to_ir(self, ctor: &mut FunctionConstructor, block: usize) -> ir::Rhs {
        match self {
            Expression::Literal(literal) => match literal {
                Literal::Int(i) => ir::Rhs::Literal(ir::Literal::Int(i)),
                Literal::Bool(b) => ir::Rhs::Literal(ir::Literal::Int(if b { 1 } else { 0 })),
            },
            Expression::Variable(name) => {
                let mut params_with_name = ctor.parameters.iter().filter(|(_, s)| s == &name);
                let param = params_with_name.next();
                assert!(params_with_name.next().is_none());
                if let Some((_, param)) = param {
                    ir::Rhs::Parameter(String::from(param))
                } else {
                    let var = ctor.variable_map.get(&name).unwrap();
                    ir::Rhs::Variable(*var)
                }
            }
            Expression::Operation(operation, left, right) => {
                let (left_type, left) = *left;
                let (right_type, right) = *right;
                // Create SSA assignment chain for left expression
                let left_rhs = left.to_ir(ctor, block);
                let left = ctor.add_assignment(block, left_type.unwrap().to_ir(), left_rhs);
                // Create SSA assignment chain for right expression
                let right_rhs = right.to_ir(ctor, block);
                let right = ctor.add_assignment(block, right_type.unwrap().to_ir(), right_rhs);
                // Return new RHS which uses those chains
                ir::Rhs::Operation(operation, left, right)
            }
        }
    }
}

fn body_to_ir(body: Vec<Statement>, ctor: &mut FunctionConstructor) -> usize {
    let starting_block = ctor.add_block();
    let mut current_block = starting_block;

    for statement in body {
        match statement {
            Statement::MakeVariable(MakeVariable { type_, lhs, rhs }) => {
                let ir_rhs = rhs.to_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.to_ir(), ir_rhs);
                ctor.variable_map.insert(lhs, index);
            }
            Statement::Return((type_, expression)) => {
                let type_ = type_.unwrap();
                let rhs = expression.to_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.to_ir(), rhs);
                ctor.add_return(current_block, type_.to_ir(), index);
            }
            Statement::If(If {
                condition: (type_, condition),
                body: if_body,
            }) => {
                let type_ = type_.unwrap();
                let rhs = condition.to_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.to_ir(), rhs);

                let if_entry_block = body_to_ir(if_body, ctor);
                let after_block = ctor.add_block();

                ctor.add_conditional_jump(
                    current_block,
                    if_entry_block,
                    after_block,
                    index,
                    type_.to_ir(),
                );

                current_block = after_block;
            }
            Statement::Print((type_, expression)) => {
                let type_ = type_.unwrap();
                let prelude_function_name = String::from(match type_ {
                    Type::Int => "print_int",
                    Type::Bool => "print_bool",
                });

                let rhs = expression.to_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.to_ir(), rhs);
                ctor.add_void_function_call(current_block, prelude_function_name, vec![index]);
            }
        }
    }

    starting_block
}

impl Function {
    pub fn to_ir(self) -> ir::Function {
        let mut ctor = FunctionConstructor::new(self.name.clone());

        ctor.returns = Some(self.returns.to_ir());
        ctor.parameters = self
            .parameters
            .into_iter()
            .map(|(t, s)| (t.to_ir(), s))
            .collect();

        body_to_ir(self.body, &mut ctor);

        ctor.construct()
    }
}
