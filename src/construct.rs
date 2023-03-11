//! Transforms the desugared abstract syntax tree into
//! an intermediate representation that can be emitted
//! by any of the backends.

use std::collections::HashMap;

use super::ast::*;
use super::ir;

struct BlockConstructor {
    locator: ir::BlockLocator,
    assignments: Vec<ir::Assignment>,
    block_terminator: Option<ir::BlockTerminator>,
}

struct FunctionConstructor {
    name: String,
    returns: Option<ir::Type>,
    parameters: Vec<(ir::Type, String)>,
    body: Vec<BlockConstructor>,
    variable_counter: usize,
    variable_map: HashMap<String, ir::Variable>,
}

impl BlockConstructor {
    fn new(locator: ir::BlockLocator) -> BlockConstructor {
        BlockConstructor {
            locator,
            assignments: Vec::new(),
            block_terminator: None,
        }
    }
    fn construct(self) -> ir::Block {
        assert!(self.block_terminator.is_some());
        ir::Block {
            locator: self.locator,
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
    fn add_block(&mut self) -> ir::BlockLocator {
        let locator = ir::BlockLocator(self.body.len());
        self.body.push(BlockConstructor::new(locator));
        locator
    }
    fn get_block(&mut self, block: ir::BlockLocator) -> &mut BlockConstructor {
        &mut self.body[block.index()]
    }
    fn add_assignment(
        &mut self,
        block: ir::BlockLocator,
        type_: ir::Type,
        rhs: ir::Rhs,
    ) -> ir::Variable {
        let var = ir::Variable(self.variable_counter);
        self.variable_counter += 1;

        let block = self.get_block(block);

        block.assignments.push(ir::Assignment {
            type_,
            lhs: Some(var),
            rhs,
        });

        var
    }
    fn add_return(&mut self, block: ir::BlockLocator, type_: ir::Type, var: ir::Variable) {
        let block = self.get_block(block);
        assert!(block.block_terminator.is_none());
        block.block_terminator = Some(ir::BlockTerminator::Return(var, type_));
    }
    fn add_unconditional_jump(&mut self, from_block: ir::BlockLocator, to_block: ir::BlockLocator) {
        unimplemented!();
    }
    fn add_conditional_jump(
        &mut self,
        from_block: ir::BlockLocator,
        to_block: ir::BlockLocator,
        else_block: ir::BlockLocator,
        condition_var: ir::Variable,
        condition_type: ir::Type,
    ) {
        let from_block = self.get_block(from_block);
        assert!(from_block.block_terminator.is_none());
        from_block.block_terminator = Some(ir::BlockTerminator::ConditionalBranch(
            to_block,
            else_block,
            (condition_type, condition_var),
        ));
    }
    fn add_void_function_call(
        &mut self,
        block: ir::BlockLocator,
        name: String,
        arguments: Vec<ir::Variable>,
    ) {
        let block = self.get_block(block);
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
    fn to_ir(self, ctor: &mut FunctionConstructor, block: ir::BlockLocator) -> ir::Rhs {
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

struct BodyInformation {
    starting_block: ir::BlockLocator,
    ending_block: ir::BlockLocator,
}

fn body_to_ir(body: Vec<Statement>, ctor: &mut FunctionConstructor) -> BodyInformation {
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

                // Construct body of if statement, which can be an arbitrary number of block
                let BodyInformation {
                    starting_block: body_starting_block,
                    ending_block: body_ending_block,
                } = body_to_ir(if_body, ctor);
                let post_if_block = ctor.add_block();

                // b0:
                //   # before if statement
                //   if (cond) { goto b1; } else { goto bN; }
                // b1..bN-2:
                //   # body of if statement
                // bN-1:
                //   # ...
                //   return x; OR goto bN;
                // bN:
                //   # post if statement

                ctor.add_conditional_jump(
                    current_block,
                    body_starting_block,
                    post_if_block,
                    index,
                    type_.to_ir(),
                );

                let body_ending_block = ctor.get_block(body_ending_block);
                if !matches!(
                    body_ending_block.block_terminator,
                    Some(ir::BlockTerminator::Return(_, _))
                ) {
                    // If the if body's very last block doesn't *return*, then
                    // it needs to jump back to after the if block.
                    body_ending_block.block_terminator =
                        Some(ir::BlockTerminator::Branch(post_if_block));
                }

                current_block = post_if_block;
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

    BodyInformation {
        starting_block,
        ending_block: current_block,
    }
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
