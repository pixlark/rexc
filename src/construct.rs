//! Transforms the desugared abstract syntax tree into
//! an intermediate representation that can be emitted
//! by any of the backends.

use std::collections::HashMap;
use std::rc::Rc;

use super::ast::*;
use super::internal_error::*;
use super::ir;

#[derive(Debug)]
pub enum ConstructErrorKind {}

#[derive(Debug)]
pub struct ConstructError {
    pub kind: ConstructErrorKind,
}

struct BlockConstructor {
    locator: ir::BlockLocator,
    assignments: Vec<ir::Step>,
    block_terminator: Option<ir::BlockTerminator>,
}

struct FunctionConstructor<'a> {
    name: String,
    returns: Option<ir::Type>,
    parameters: Vec<(ir::Type, String)>,
    body: Vec<BlockConstructor>,
    variable_counter: usize,
    variable_map: HashMap<String, ir::Variable>,
    file_scope_variables: &'a [String],
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
        ir::Block {
            locator: self.locator,
            assignments: self.assignments,
            block_terminator: self.block_terminator.rexc_unwrap(
                "Somehow .construct() was called on a BlockConstructor that has no terminator!",
            ),
        }
    }
}

impl FunctionConstructor<'_> {
    fn new(name: String, file_scope_variables: &[String]) -> FunctionConstructor {
        FunctionConstructor {
            name,
            returns: None,
            parameters: Vec::new(),
            body: Vec::new(),
            variable_counter: 0,
            variable_map: HashMap::new(),
            file_scope_variables,
        }
    }
    fn construct(self) -> ir::Function {
        ir::Function {
            name: self.name,
            returns: self.returns.rexc_unwrap(
                "Somehow .construct() was called on a FunctionConstructor that has no return type!",
            ),
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

        block
            .assignments
            .push(ir::Step::NewAssignment((type_, var), rhs));

        var
    }
    fn add_reassignment(&mut self, block: ir::BlockLocator, lhs: ir::LValue, rhs: ir::Rhs) {
        let block = self.get_block(block);
        block.assignments.push(ir::Step::Assignment(lhs, rhs));
    }
    fn add_return(&mut self, block: ir::BlockLocator, type_: ir::Type, var: ir::Variable) {
        let block = self.get_block(block);
        rexc_assert(block.block_terminator.is_none());
        block.block_terminator = Some(ir::BlockTerminator::Return(var, type_));
    }
    fn add_unfilled_branch(&mut self, from_block: ir::BlockLocator) {
        let from_block = self.get_block(from_block);
        rexc_assert(from_block.block_terminator.is_none());
        from_block.block_terminator = Some(ir::BlockTerminator::Branch(None));
    }
    fn fill_unfilled_branch(&mut self, from_block: ir::BlockLocator, to_block: ir::BlockLocator) {
        let from_block = self.get_block(from_block);
        rexc_assert(matches!(
            from_block.block_terminator,
            Some(ir::BlockTerminator::Branch(None))
        ));
        from_block.block_terminator = Some(ir::BlockTerminator::Branch(Some(to_block)));
    }
    fn add_unconditional_jump(&mut self, from_block: ir::BlockLocator, to_block: ir::BlockLocator) {
        let from_block = self.get_block(from_block);
        rexc_assert(from_block.block_terminator.is_none());
        from_block.block_terminator = Some(ir::BlockTerminator::Branch(Some(to_block)));
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
        rexc_assert(from_block.block_terminator.is_none());
        from_block.block_terminator = Some(ir::BlockTerminator::ConditionalBranch(
            to_block,
            else_block,
            (condition_type, condition_var),
        ));
    }
    fn add_discarded_value(&mut self, block: ir::BlockLocator, rhs: ir::Rhs) {
        let block = self.get_block(block);
        block.assignments.push(ir::Step::Discarded(rhs));
    }
    fn add_void_function_call(
        &mut self,
        block: ir::BlockLocator,
        name: String,
        arguments: Vec<ir::Variable>,
    ) {
        let block = self.get_block(block);
        let rhs = ir::Rhs::FunctionCall(ir::FunctionReference::FileScope(name), arguments);
        block.assignments.push(ir::Step::Discarded(rhs));
    }
}

impl Type {
    fn into_ir(&self) -> ir::Type {
        match self {
            Type::Unit => ir::Type::Void,
            Type::Int => ir::Type::Int,
            Type::Bool => ir::Type::Int,
            Type::Pointer(type_) => ir::Type::Pointer(Box::new(type_.into_ir())),
            Type::Function(rc) => ir::Type::Function(std::rc::Rc::new((
                rc.0.into_ir(),
                rc.1.iter().map(|t| t.into_ir()).collect(),
            ))),
        }
    }
}

impl Expression {
    fn into_ir(self, ctor: &mut FunctionConstructor, block: ir::BlockLocator) -> ir::Rhs {
        match self.kind {
            ExpressionKind::Unit => ir::Rhs::Void,
            ExpressionKind::Literal(literal) => match literal {
                Literal::Int(i) => ir::Rhs::Literal(ir::Literal::Int(i)),
                Literal::Bool(b) => ir::Rhs::Literal(ir::Literal::Int(if b { 1 } else { 0 })),
            },
            ExpressionKind::Variable(name) => {
                if ctor.file_scope_variables.contains(&name) {
                    ir::Rhs::FileScopeVariable(name.clone())
                } else {
                    let mut params_with_name = ctor.parameters.iter().filter(|(_, s)| s == &name);
                    let param = params_with_name.next();
                    rexc_assert(params_with_name.next().is_none());
                    if let Some((_, param)) = param {
                        ir::Rhs::Parameter(String::from(param))
                    } else {
                        let var = ctor
                            .variable_map
                            .get(&name)
                            .rexc_unwrap("Somehow a variable was never added to the variable map!");
                        ir::Rhs::Variable(*var)
                    }
                }
            }
            ExpressionKind::Dereference(count, interior) => {
                let moved = Rc::try_unwrap(interior).rexc_unwrap("Somehow an dereference expression reached construction while still having other references alive to its interior!");
                ir::Rhs::Dereference(count, Box::new(moved.into_inner().into_ir(ctor, block)))
            }
            ExpressionKind::Operation(operation, left, right) => {
                let (left_type, left) = *left;
                let (right_type, right) = *right;
                // Create SSA assignment chain for left expression
                let left_rhs = left.into_ir(ctor, block);
                let left = ctor.add_assignment(
                    block,
                    left_type.rexc_unwrap("Somehow the left side of an operation passed the typechecker without having a type filled in!").into_ir(),
                    left_rhs
                );
                // Create SSA assignment chain for right expression
                let right_rhs = right.into_ir(ctor, block);
                let right = ctor.add_assignment(
                    block,
                    right_type.rexc_unwrap("Somehow the right side of an operation passed the typechecker without having a type filled in!").into_ir(),
                    right_rhs
                );
                // Return new RHS which uses those chains
                ir::Rhs::Operation(operation, left, right)
            }
            ExpressionKind::FunctionCall(type_, name, arguments) => {
                let _type = type_.rexc_unwrap("Somehow a function call passed the typechecker without having a type filled in!");

                // Create SSA assignment chain for each argument
                let mut arg_vars = Vec::new();
                for (arg_type, argument) in arguments {
                    let arg_type = arg_type.rexc_unwrap("Somehow a function call's argument passed the typechecker without having a type filled in!");
                    let arg_type = arg_type.into_ir();
                    let rhs = argument.into_ir(ctor, block);
                    arg_vars.push(ctor.add_assignment(block, arg_type, rhs));
                }

                if ctor.variable_map.contains_key(&name) {
                    // Locally defined function alias
                    ir::Rhs::FunctionCall(
                        ir::FunctionReference::Local(*ctor.variable_map.get(&name).unwrap()),
                        arg_vars,
                    )
                } else {
                    // Standard function
                    ir::Rhs::FunctionCall(ir::FunctionReference::FileScope(name), arg_vars)
                }
            }
            ExpressionKind::Allocate(ite) => {
                let (type_, expr) = *ite;
                let type_ = type_.rexc_unwrap("Somehow an alloc expression got past the typechecker without having its type filled in!");
                let type_ir = type_.into_ir();

                // Allocate the space
                let sizeof =
                    ctor.add_assignment(block, ir::Type::Int, ir::Rhs::SizeOf(type_ir.clone()));
                let allocation = ctor.add_assignment(
                    block,
                    ir::Type::Pointer(Box::new(type_ir.clone())),
                    ir::Rhs::FunctionCall(
                        ir::FunctionReference::FileScope(String::from("alloc")),
                        vec![sizeof],
                    ),
                );

                // Create the initial value
                let rhs = expr.into_ir(ctor, block);
                let value = ctor.add_assignment(block, type_ir.clone(), rhs);

                // Assign the value to the allocated pointer
                ctor.add_reassignment(
                    block,
                    ir::LValue {
                        var: allocation,
                        derefs: 1,
                    },
                    ir::Rhs::Variable(value),
                );

                // Return the pointer variable
                ir::Rhs::Variable(allocation)
            }
        }
    }
}

impl LValue {
    fn into_ir(&mut self, ctor: &mut FunctionConstructor) -> ir::LValue {
        let var = *ctor.variable_map.get(&self.name).unwrap();
        ir::LValue {
            var,
            derefs: self.derefs,
        }
    }
}

struct BodyInformation {
    starting_block: ir::BlockLocator,
    ending_block: ir::BlockLocator,
}

// TODO(Brooke): This should really take an iterator, not a Vec
fn body_into_ir(
    body: Vec<Statement>,
    ctor: &mut FunctionConstructor,
    break_blocks: &mut Option<&mut Vec<ir::BlockLocator>>,
) -> BodyInformation {
    let starting_block = ctor.add_block();
    let mut current_block = starting_block;

    for statement in body {
        match statement.kind {
            StatementKind::BareExpression((type_, expression)) => {
                let _type = type_.rexc_unwrap("Somehow a bare expression passed the typechecker without its type being filled in!")
                    .into_ir();
                let rhs = expression.into_ir(ctor, current_block);

                ctor.add_discarded_value(current_block, rhs);
            }
            StatementKind::MakeVariable(MakeVariable { type_, lhs, rhs }) => {
                let ir_rhs = rhs.into_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.into_ir(), ir_rhs);
                ctor.variable_map.insert(lhs, index);
            }
            StatementKind::SetVariable(SetVariable {
                mut lhs,
                rhs: (_type, rhs),
            }) => {
                let ir_rhs = rhs.into_ir(ctor, current_block);
                /*let ir_lhs = ctor.variable_map.get_lvalue(&lhs)
                .rexc_unwrap("Somehow a bad variable assignment passed the typechecker without UnboundVariable being thrown!");*/
                let ir_lhs = lhs.into_ir(ctor);

                ctor.add_reassignment(current_block, ir_lhs, ir_rhs);
            }
            StatementKind::Return((type_, expression)) => {
                let type_ = type_.rexc_unwrap("Somehow a return statement passed the typechecker without its type being filled in!");
                let rhs = expression.into_ir(ctor, current_block);
                let type_ir = type_.into_ir();
                let index = ctor.add_assignment(current_block, type_ir.clone(), rhs);
                ctor.add_return(current_block, type_ir, index);
                // If we don't break we might continue the loop and start constructing
                // unreachable post-return code!
                break;
            }
            StatementKind::If(If {
                condition: (type_, condition),
                body: if_body,
            }) => {
                let type_ = type_.rexc_unwrap("Somehow an if condition passed the typechecker without its type being filled in!");
                let rhs = condition.into_ir(ctor, current_block);
                let type_ir = type_.into_ir();
                let index = ctor.add_assignment(current_block, type_ir.clone(), rhs);

                // Construct body of if statement, which can be an arbitrary number of blocks
                let BodyInformation {
                    starting_block: body_starting_block,
                    ending_block: body_ending_block,
                } = body_into_ir(if_body.into_iter().collect(), ctor, break_blocks);
                // If statement don't have `break`s
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
                    type_ir,
                );

                match ctor.get_block(body_ending_block).block_terminator {
                    None => {
                        // If the if body's very last block doesn't terminate, then
                        // it needs to jump forwards to after the if block.
                        ctor.add_unconditional_jump(body_ending_block, post_if_block);
                    }
                    Some(ir::BlockTerminator::Branch(None)) => {
                        // Unfilled branches are left in the terminator position
                        // by `break` statements, because they can only be filled
                        // once the loop body is fully constructed.
                    }
                    Some(ir::BlockTerminator::Return(..)) => {
                        // Return statements make the following code unreachable,
                        // so when there's a return statement we don't bother trying
                        // to add an additional goto to reach the next block
                    }
                    Some(ir::BlockTerminator::Branch(Some(..))) => {
                        panic!("Somehow the final block in an if-body has a filled branch already added!");
                    }
                    Some(ir::BlockTerminator::ConditionalBranch(..)) => {
                        panic!(
                            "Somehow the final block in an if-body had a condbranch already added!"
                        );
                    }
                }

                current_block = post_if_block;
            }
            StatementKind::Loop(body) => {
                let mut break_blocks = Vec::new();

                // Construct body of loop statement, which can be an arbitrary number of blocks
                let BodyInformation {
                    starting_block: body_starting_block,
                    ending_block: body_ending_block,
                } = body_into_ir(
                    body.into_iter().collect(),
                    ctor,
                    &mut Some(&mut break_blocks),
                );

                ctor.add_unconditional_jump(current_block, body_starting_block);

                let post_loop_block = ctor.add_block();
                for break_block in break_blocks {
                    ctor.fill_unfilled_branch(break_block, post_loop_block);
                }

                match ctor.get_block(body_ending_block).block_terminator {
                    None => {
                        // If the loop body's very last block doesn't terminate, then
                        // it needs to jump back to the beginning of the loop.
                        ctor.add_unconditional_jump(body_ending_block, body_starting_block);
                    }
                    Some(ir::BlockTerminator::Return(..)) => {
                        // Return statements mean the loop will not continue,
                        // so when there's a return statement we don't bother trying
                        // to add an additional goto to reset the loop
                    }
                    Some(ir::BlockTerminator::Branch(..)) => {
                        // If our loop ends in a branch (would be pointless, but
                        // still is accepted code), then we don't bother trying to
                        // add an additional goto to reset the loop
                    }
                    Some(ir::BlockTerminator::ConditionalBranch(..)) => {
                        panic!("Somehow the final block in a loop-body had a condbranch already added!");
                    }
                }

                current_block = post_loop_block;
            }
            StatementKind::Break => {
                break_blocks.as_mut().rexc_unwrap("Somehow a break statement was reached wihout a `break_blocks` argument being passed to `body_into_ir`.").push(current_block);
                ctor.add_unfilled_branch(current_block);
                // If we don't break we might continue the loop and start constructing
                // unreachable post-break code!
                break;
            }
            StatementKind::Print((type_, expression)) => {
                let type_ = type_.rexc_unwrap("Somehow a print statement passed the typechecker without its type being filled in!");
                let prelude_function_name = String::from(match type_ {
                    Type::Unit => "print_unit",
                    Type::Int => "print_int",
                    Type::Bool => "print_bool",
                    Type::Pointer(..) => "print_pointer",
                    Type::Function(..) => unimplemented!(),
                });

                let rhs = expression.into_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.into_ir(), rhs);
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
    pub fn into_ir(self, file_scope_variables: &[String]) -> ir::Function {
        let mut ctor = FunctionConstructor::new(self.name.clone(), file_scope_variables);

        ctor.returns = Some(match self.returns {
            Type::Unit => ir::Type::Void,
            _ => self.returns.into_ir(),
        });
        ctor.parameters = self
            .parameters
            .into_iter()
            .map(|(t, s)| (t.into_ir(), s))
            .collect();

        body_into_ir(self.body.into_iter().collect(), &mut ctor, &mut None);

        ctor.construct()
    }
}

impl File {
    pub fn into_ir(self) -> ir::CompilationUnit {
        let file_scope_variables = self
            .functions
            .iter()
            .map(|f| f.name.clone())
            .collect::<Vec<String>>();
        let mut compilation_unit = ir::CompilationUnit {
            functions: Vec::new(),
        };
        for function in self.functions {
            compilation_unit
                .functions
                .push(function.into_ir(&file_scope_variables));
        }
        compilation_unit
    }
}
