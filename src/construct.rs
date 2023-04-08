//! Transforms the desugared abstract syntax tree into
//! an intermediate representation that can be emitted
//! by any of the backends.

#![allow(clippy::wrong_self_convention)]

use std::collections::HashMap;

use super::ast::*;
use super::internal_error::*;
use super::ir;

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
    fn add_uninitialized_assignment(
        &mut self,
        block: ir::BlockLocator,
        type_: ir::Type,
    ) -> ir::Variable {
        let var = ir::Variable(self.variable_counter);
        self.variable_counter += 1;

        let block = self.get_block(block);

        block
            .assignments
            .push(ir::Step::NewUninitialized((type_, var)));

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
        func: ir::FunctionReference,
        arguments: Vec<ir::Variable>,
    ) {
        let block = self.get_block(block);
        let rhs = ir::Rhs::FunctionCall(func, arguments);
        block.assignments.push(ir::Step::Discarded(rhs));
    }
    fn variable_lookup(&self, name: String) -> ir::Rhs {
        if self.file_scope_variables.contains(&name) {
            ir::Rhs::FileScopeVariable(name)
        } else {
            let mut params_with_name = self.parameters.iter().filter(|(_, s)| s == &name);
            let param = params_with_name.next();
            rexc_assert(params_with_name.next().is_none());
            if let Some((_, param)) = param {
                ir::Rhs::Parameter(String::from(param))
            } else {
                let var = self
                    .variable_map
                    .get(&name)
                    .rexc_unwrap("Somehow a variable was never added to the variable map!");
                ir::Rhs::Variable(*var)
            }
        }
    }
}

impl Type {
    fn into_ir(&self) -> ir::Type {
        match self {
            Type::Unit => ir::Type::Void,
            Type::Nil => ir::Type::Null,
            Type::Int => ir::Type::Int,
            Type::Bool => ir::Type::Int,
            Type::Pointer(type_) => ir::Type::Pointer(Box::new(type_.into_ir())),
            Type::Function(rc) => ir::Type::Function(std::rc::Rc::new((
                rc.0.into_ir(),
                rc.1.iter().map(|t| t.into_ir()).collect(),
            ))),
            Type::Named((name, _)) => ir::Type::Named(name.clone()),
        }
    }
}

impl DataType {
    fn get_field(&self, name: &str) -> ir::Field {
        let (field_index, _) = self.fields.iter().enumerate().find(|(_, (_, s))| s == name).rexc_unwrap("Somehow a new expression passed the typechecker with a field assignment that's not in the associated data type!");
        ir::Field(field_index)
    }
}

impl Expression {
    fn into_ir(self, ctor: &mut FunctionConstructor, block: ir::BlockLocator) -> ir::Rhs {
        match self.kind {
            ExpressionKind::Unit => ir::Rhs::Void,
            ExpressionKind::Nil => ir::Rhs::Null,
            ExpressionKind::Literal(literal) => match literal {
                Literal::Int(i) => ir::Rhs::Literal(ir::Literal::Int(i)),
                Literal::Bool(b) => ir::Rhs::Literal(ir::Literal::Int(if b { 1 } else { 0 })),
            },
            ExpressionKind::Variable(name) => ctor.variable_lookup(name),
            ExpressionKind::Dereference(inner) => {
                ir::Rhs::Dereference(Box::new(inner.into_ir(ctor, block)))
            }
            ExpressionKind::UnaryOperation(operation, inner) => {
                let (type_, expr) = *inner;

                let rhs = expr.into_ir(ctor, block);
                let var =
                    ctor.add_assignment(block, type_.expected_from_typechecker().into_ir(), rhs);

                ir::Rhs::UnaryOperation(operation, var)
            }
            ExpressionKind::Operation(operation, left, right) => {
                let (left_type, left) = *left;
                let (right_type, right) = *right;
                // Create SSA assignment chain for left expression
                let left_rhs = left.into_ir(ctor, block);
                let left = ctor.add_assignment(
                    block,
                    left_type.expected_from_typechecker().into_ir(),
                    left_rhs,
                );
                // Create SSA assignment chain for right expression
                let right_rhs = right.into_ir(ctor, block);
                let right = ctor.add_assignment(
                    block,
                    right_type.expected_from_typechecker().into_ir(),
                    right_rhs,
                );
                // Return new RHS which uses those chains
                ir::Rhs::Operation(operation, left, right)
            }
            ExpressionKind::FunctionCall(type_, name, arguments) => {
                let _type = type_.expected_from_typechecker();

                // Create SSA assignment chain for each argument
                let mut arg_vars = Vec::new();
                for (arg_type, argument) in arguments {
                    let arg_type = arg_type.expected_from_typechecker();
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
                let type_ = type_.expected_from_typechecker();
                let type_ir = type_.into_ir();

                // Allocate the space
                let sizeof =
                    ctor.add_assignment(block, ir::Type::Int, ir::Rhs::SizeOf(type_ir.clone()));
                let allocation = ctor.add_assignment(
                    block,
                    ir::Type::Pointer(Box::new(type_ir.clone())),
                    ir::Rhs::FunctionCall(
                        ir::FunctionReference::Builtin(String::from("alloc")),
                        vec![sizeof],
                    ),
                );

                // Create the initial value
                let rhs = expr.into_ir(ctor, block);
                let value = ctor.add_assignment(block, type_ir, rhs);

                // Assign the value to the allocated pointer
                ctor.add_reassignment(
                    block,
                    ir::LValue::Dereference(Box::new(ir::LValue::Variable(allocation))),
                    ir::Rhs::Variable(value),
                );

                // Return the pointer variable
                ir::Rhs::Variable(allocation)
            }
            ExpressionKind::New((type_, New { name, fields })) => {
                let var = ctor.add_uninitialized_assignment(block, ir::Type::Named(name));

                let data_type = match &type_ {
                    Some(Type::Named((_, Some(type_), ..))) => {
                        type_.clone()
                    }
                    _ => rexc_panic("Somehow a new expression was annotated with a non-datatype type, or was not annotated at all!")
                };

                for (field_name, expression) in fields {
                    let rhs = expression.into_ir(ctor, block);
                    let lhs = data_type.borrow().get_field(&field_name);
                    ctor.add_reassignment(
                        block,
                        ir::LValue::FieldAccess(Box::new(ir::LValue::Variable(var)), lhs),
                        rhs,
                    );
                }

                ir::Rhs::Variable(var)
            }
            ExpressionKind::FieldAccess {
                type_: _type,
                lhs: interior,
                field,
                needs_dereference: _,
            } => {
                //let lhs_type = interior.0.rexc_unwrap("Somehow a field access expression passed the typechecker without having its interior type filled in!");
                let lhs_type = interior.0.expected_from_typechecker();
                let interior = interior.1;

                let field = match &lhs_type {
                    Type::Named((_, Some(data_type))) => {
                        data_type.borrow().get_field(&field)
                    },
                    _ => rexc_panic("Somehow a field access expression *not* on a data structure passed the typechecker!"),
                };

                let ir_type = lhs_type.into_ir();
                let rhs = interior.into_ir(ctor, block);
                let var = ctor.add_assignment(block, ir_type, rhs);

                ir::Rhs::FieldAccess(var, field)
            }
        }
    }
}

impl LValue {
    fn into_ir(&mut self, ctor: &mut FunctionConstructor) -> ir::LValue {
        match &mut self.kind {
            LValueKind::Identifier(name) => {
                let var = ctor.variable_lookup(name.clone());
                match var {
                    ir::Rhs::Variable(var) => ir::LValue::Variable(var),
                    ir::Rhs::Parameter(param) => ir::LValue::Parameter(param),
                    ir::Rhs::FileScopeVariable(..) => rexc_panic("Somehow an lvalue that resolves to a non-local variable passed the typechecker."),
                    _ => unreachable!(),
                }
            }
            LValueKind::Dereference(inner) => {
                ir::LValue::Dereference(Box::new(inner.into_ir(ctor)))
            }
            LValueKind::FieldAccess {
                lhs,
                field,
                needs_dereference: _,
            } => {
                let field = match &lhs.type_ {
                    Some(Type::Named((_, Some(data_type)))) => {
                        data_type.borrow().get_field(field)
                    },
                    _ => rexc_panic("Somehow a field access lvalue *not* on a data structure passed the typechecker!"),
                };
                ir::LValue::FieldAccess(Box::new(lhs.into_ir(ctor)), field)
            }
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
                let _type = type_.expected_from_typechecker().into_ir();
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
                let type_ = type_.expected_from_typechecker();
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
                else_: else_body,
            }) => {
                let type_ = type_.expected_from_typechecker();
                let rhs = condition.into_ir(ctor, current_block);
                let type_ir = type_.into_ir();
                let index = ctor.add_assignment(current_block, type_ir.clone(), rhs);

                // Construct body of if statement, which can be an arbitrary number of blocks
                let BodyInformation {
                    starting_block: body_starting_block,
                    ending_block: body_ending_block,
                } = body_into_ir(if_body.into_iter().collect(), ctor, break_blocks);
                let BodyInformation {
                    starting_block: else_starting_block,
                    ending_block: else_ending_block,
                } = body_into_ir(else_body.into_iter().collect(), ctor, break_blocks);
                // If statements don't have `break`s
                let post_everything = ctor.add_block();

                // bStart:
                //   # before if statement
                //   if (cond) { goto bIf; } else { goto bElse; }
                // bIf..bElse-2:
                //   # body of if statement
                // bElse-1:
                //   # ...
                //   return x; OR goto bEnd;
                // bElse..bEnd-2:
                //   # body of else statement
                // bEnd-1:
                //   return x; OR goto bEnd;
                // bEnd:
                //   # post if statement

                ctor.add_conditional_jump(
                    current_block,
                    body_starting_block,
                    else_starting_block,
                    index,
                    type_ir,
                );

                match ctor.get_block(body_ending_block).block_terminator {
                    None => {
                        // If the if body's very last block doesn't terminate, then
                        // it needs to jump forwards to after the if/else block.
                        ctor.add_unconditional_jump(body_ending_block, post_everything);
                    }
                    Some(ir::BlockTerminator::Branch(None)) => {
                        // Unfilled branches are left in the terminator position
                        // by `break` statements, because they can only be filled
                        // once the loop body is fully constructed.

                        // TODO(Brooke): This should probably be more explicit in the type system,
                        //               maybe instead of `Option`, use an enum with `Some`, `None`,
                        //               and `WaitingForBreak`?
                    }
                    Some(ir::BlockTerminator::Return(..)) => {
                        // Return statements make the following code unreachable,
                        // so when there's a return statement we don't bother trying
                        // to add an additional goto to reach the next block
                    }
                    Some(ir::BlockTerminator::Branch(Some(..))) => {
                        rexc_panic("Somehow the final block in an if-body has a filled branch already added!");
                    }
                    Some(ir::BlockTerminator::ConditionalBranch(..)) => {
                        rexc_panic(
                            "Somehow the final block in an if-body has a condbranch already added!",
                        );
                    }
                }

                match ctor.get_block(else_ending_block).block_terminator {
                    None => ctor.add_unconditional_jump(else_ending_block, post_everything),
                    Some(ir::BlockTerminator::Branch(None)) => {}
                    Some(ir::BlockTerminator::Return(..)) => {}
                    Some(ir::BlockTerminator::Branch(Some(..))) => {
                        rexc_panic("Somehow the final block in an else-body has a filled branch already added!");
                    }
                    Some(ir::BlockTerminator::ConditionalBranch(..)) => {
                        rexc_panic("Somehow the final block in an else-body has a condbranch already added!");
                    }
                }

                current_block = post_everything;
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
            StatementKind::While { .. } => should_have_been_desugared(),
            StatementKind::Break => {
                break_blocks.as_mut().rexc_unwrap("Somehow a break statement was reached wihout a `break_blocks` argument being passed to `body_into_ir`.").push(current_block);
                ctor.add_unfilled_branch(current_block);
                // If we don't break we might continue the loop and start constructing
                // unreachable post-break code!
                break;
            }
            StatementKind::Print((type_, expression)) => {
                let type_ = type_.expected_from_typechecker();
                let prelude_function_name = String::from(match type_ {
                    Type::Unit => "print_unit",
                    Type::Nil => "print_nil",
                    Type::Int => "print_int",
                    Type::Bool => "print_bool",
                    Type::Pointer(..) => "print_pointer",
                    Type::Function(..) => unimplemented!(),
                    Type::Named(..) => unimplemented!(),
                });

                let rhs = expression.into_ir(ctor, current_block);
                let index = ctor.add_assignment(current_block, type_.into_ir(), rhs);
                ctor.add_void_function_call(
                    current_block,
                    ir::FunctionReference::Builtin(prelude_function_name),
                    vec![index],
                );
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

impl DataType {
    pub fn into_ir(&self) -> ir::DataType {
        ir::DataType {
            name: self.name.clone(),
            fields: self
                .fields
                .iter()
                .map(|(t, _)| t.clone().into_ir())
                .collect(),
        }
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
            data_types: Vec::new(),
            functions: Vec::new(),
        };
        for data_type in self.data_types {
            compilation_unit
                .data_types
                .push(data_type.borrow().into_ir());
        }
        for function in self.functions {
            compilation_unit
                .functions
                .push(function.into_ir(&file_scope_variables));
        }
        compilation_unit
    }
}
