//! The C backend for Rexc. This is where IR code gets translated directly
//! into C code.

use std::io::{LineWriter, Result, Write};

use super::super::internal_error::*;
use super::super::ir::*;

pub type EmitResult = Result<()>;

/// CEmitter is a convenience trait that makes backend code more readable.
/// It just inverts calls like `object.emit_c(writer)`, which look lopsided,
/// especially when they're right next to CWriter calls, into the form
/// `writer.emit(object)`.
trait CEmitter<W: Write, E: EmitC<W>> {
    fn emit(&mut self, t: &E) -> EmitResult;
}

/// CWriter is just a collection of functions on LineWriter that we use
/// all over to emit C code.
trait CWriter {
    fn space(&mut self) -> EmitResult;
    fn newline(&mut self) -> EmitResult;
    fn string(&mut self, s: &str) -> EmitResult;
    fn variable(&mut self, v: Variable) -> EmitResult;
    fn label(&mut self, b: BlockLocator) -> EmitResult;
    fn lvalue(&mut self, lhs: &LValue) -> EmitResult;
}

impl<W: Write, E: EmitC<W>> CEmitter<W, E> for LineWriter<W> {
    fn emit(&mut self, t: &E) -> EmitResult {
        t.emit_c(self)
    }
}

impl<W: Write> CWriter for LineWriter<W> {
    fn space(&mut self) -> EmitResult {
        write!(self, " ")
    }

    fn newline(&mut self) -> EmitResult {
        writeln!(self)
    }

    fn string(&mut self, s: &str) -> EmitResult {
        write!(self, "{}", s)
    }

    fn variable(&mut self, v: Variable) -> EmitResult {
        write!(self, "_{}", v.index())
    }

    fn label(&mut self, b: BlockLocator) -> EmitResult {
        write!(self, "_b{}", b.index())
    }

    fn lvalue(&mut self, lhs: &LValue) -> EmitResult {
        for _ in 0..lhs.derefs {
            self.string("*")?;
        }
        self.variable(lhs.var)?;
        Ok(())
    }
}

fn emit_function_pointer<W: Write>(
    w: &mut LineWriter<W>,
    f: Type,
    interior: &dyn Fn(&mut LineWriter<W>) -> EmitResult,
) -> EmitResult {
    fn helper<W: Write>(
        w: &mut LineWriter<W>,
        f: Type,
        interior: &dyn Fn(&mut LineWriter<W>) -> EmitResult,
        pointer_count: usize,
    ) -> EmitResult {
        match f {
            Type::Function(f) => {
                w.emit(&f.0)?;
                w.string("(*")?;
                for _ in 0..pointer_count {
                    w.string("*")?;
                }
                interior(w)?;
                w.string(")(")?;
                for (i, parameter) in f.1.iter().enumerate() {
                    w.emit(parameter)?;
                    if i < f.1.len() - 1 {
                        w.string(", ")?;
                    }
                }
                w.string(")")?;
                Ok(())
            }
            Type::Pointer(inner) => helper(w, *inner, interior, pointer_count + 1),
            _ => unreachable!(),
        }
    }
    helper(w, f, interior, 0)
}

impl Type {
    /// Is this a direct function pointer, or a pointer to a
    /// function pointer, or a pointer to a pointer to a...
    /// You get the gist.
    fn is_function_pointer(&self) -> bool {
        match self {
            Type::Function(..) => true,
            Type::Pointer(t) => t.is_function_pointer(),
            _ => false,
        }
    }
}

//
// EmitC
//

/// `EmitC` is the core of the C backend. Anything that implements EmitC
/// can, as you might imagine, emit C code. This means anything from a
/// full-on function to a teeny tiny number literal. Each instance of EmitC
/// will "recursively" call `.emit_c()` on its component parts (although
/// in practice calls to the `CEmitter` trait are used instead for readability).
pub trait EmitC<W: Write> {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult;
}

impl<W: Write> EmitC<W> for Type {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            // TODO(Brooke): Void values (unit values) should be handled properly - i.e.
            //               they should be elided when possible, and converted to 'void'
            //               for function return values. At the moment we represent unit
            //               values as chars, which is wasteful.
            Type::Void => write!(writer, "char"),
            Type::Int => write!(writer, "int"),
            Type::Pointer(inner) => {
                if inner.is_function_pointer() {
                    // Special case for function pointers in C
                    emit_function_pointer(writer, Type::Pointer(inner.clone()), &|_| Ok(()))?;
                } else {
                    writer.emit(inner.as_ref())?;
                    writer.string("*")?;
                }
                Ok(())
            }
            Type::Function(f) => {
                emit_function_pointer(writer, Type::Function(f.clone()), &|_| Ok(()))
            }
        }?;
        Ok(())
    }
}

impl<W: Write> EmitC<W> for Literal {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            Literal::Int(i) => write!(writer, "{}", i),
        }?;
        Ok(())
    }
}

impl<W: Write> EmitC<W> for FunctionReference {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            FunctionReference::Local(var) => writer.variable(*var),
            FunctionReference::Parameter(name) => writer.string(name),
            FunctionReference::FileScope(name) => writer.string(name),
        }
    }
}

impl<W: Write> EmitC<W> for Rhs {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            Rhs::Void => writer.string("0"),
            Rhs::Parameter(s) => writer.string(s),
            Rhs::Variable(var) => writer.variable(*var),
            Rhs::Dereference(count, interior) => {
                for _ in 0..*count {
                    writer.string("*")?;
                }
                writer.emit(interior.as_ref())?;
                Ok(())
            }
            Rhs::FileScopeVariable(s) => writer.string(s),
            Rhs::Literal(literal) => writer.emit(literal),
            Rhs::Operation(op, left, right) => {
                writer.string("(")?;
                writer.variable(*left)?;
                writer.space()?;
                writer.string(match *op {
                    Operation::Add => "+",
                    Operation::Subtract => "-",
                    Operation::Multiply => "*",
                    Operation::Divide => "/",
                    Operation::Equals => "==",
                    Operation::NotEquals => "!=",
                    Operation::LessThan => "<",
                    Operation::GreaterThan => ">",
                    Operation::LessThanOrEqualTo => "<=",
                    Operation::GreaterThanOrEqualTo => ">=",
                })?;
                writer.space()?;
                writer.variable(*right)?;
                writer.string(")")?;
                Ok(())
            }
            Rhs::FunctionCall(name, arguments) => {
                writer.emit(name)?;
                writer.string("(")?;
                for (i, argument) in arguments.iter().enumerate() {
                    writer.variable(*argument)?;
                    if i < arguments.len() - 1 {
                        writer.string(", ")?;
                    }
                }
                writer.string(")")?;
                Ok(())
            }
            Rhs::SizeOf(type_) => {
                writer.string("(sizeof(")?;
                writer.emit(type_)?;
                writer.string("))")?;
                Ok(())
            }
        }?;
        Ok(())
    }
}

impl Function {
    fn emit_c_header<W: Write>(&self, writer: &mut LineWriter<W>) -> EmitResult {
        let interior = |writer: &mut LineWriter<W>| -> EmitResult {
            writer.string(&self.name)?;
            writer.string("(")?;
            for (i, pair) in self.parameters.iter().enumerate() {
                let type_ = &pair.0;
                let name = &pair.1;
                if type_.is_function_pointer() {
                    // Special case for function pointers in C
                    emit_function_pointer(writer, type_.clone(), &|w| {
                        w.emit(&FunctionReference::Parameter(name.clone()))
                    })?;
                } else {
                    writer.emit(type_)?;
                    writer.space()?;
                    writer.string(name)?;
                }
                if i < self.parameters.len() - 1 {
                    writer.string(", ")?;
                }
            }
            writer.string(")")?;
            Ok(())
        };
        if self.returns.is_function_pointer() {
            emit_function_pointer(writer, self.returns.clone(), &interior)?;
        } else {
            self.returns.emit_c(writer)?;
            writer.space()?;
            interior(writer)?;
        }
        Ok(())
    }
    pub fn emit_c_prototype<W: Write>(&self, writer: &mut LineWriter<W>) -> EmitResult {
        self.emit_c_header(writer)?;
        writer.string(";")?;
        writer.newline()?;
        Ok(())
    }
}

impl<W: Write> EmitC<W> for BlockTerminator {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            BlockTerminator::Branch(to) => {
                writer.string("goto ")?;
                writer.label(to.rexc_unwrap(
                    "Somehow the C emitter got hold of an unfilled Branch instruction.",
                ))?;
                writer.string(";")?;
                writer.newline()?;
            }
            BlockTerminator::ConditionalBranch(from, to, condition) => {
                writer.string("if (")?;
                writer.variable(condition.1)?;
                writer.string(") { goto ")?;
                writer.label(*from)?;
                writer.string("; } else { goto ")?;
                writer.label(*to)?;
                writer.string("; }")?;
                writer.newline()?;
            }
            BlockTerminator::Return(variable, _) => {
                writer.string("return ")?;
                writer.variable(*variable)?;
                writer.string(";")?;
                writer.newline()?;
            }
        }
        Ok(())
    }
}

impl<W: Write> EmitC<W> for Block {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        // Label that starts a block
        writer.label(self.locator)?;
        writer.string(":")?;
        writer.newline()?;
        // Block consists of assignments without control flow
        for assignment in self.assignments.iter() {
            match assignment {
                Step::NewAssignment((type_, var), rhs) => {
                    if type_.is_function_pointer() {
                        // Special case for function pointers in C
                        emit_function_pointer(writer, type_.clone(), &|w| {
                            w.emit(&FunctionReference::Local(*var))
                        })?;
                    } else {
                        writer.emit(type_)?;
                        writer.space()?;
                        writer.variable(*var)?;
                    }
                    writer.string(" = ")?;
                    writer.emit(rhs)?;
                    writer.string(";")?;
                    writer.newline()?;
                }
                Step::Assignment(var, rhs) => {
                    writer.lvalue(var)?;
                    writer.string(" = ")?;
                    writer.emit(rhs)?;
                    writer.string(";")?;
                    writer.newline()?;
                }
                Step::Discarded(rhs) => {
                    writer.emit(rhs)?;
                    writer.string(";")?;
                    writer.newline()?;
                }
            }
        }
        // Blocks ends with either a jump to another block, or a return
        // from the function
        writer.emit(&self.block_terminator)?;
        Ok(())
    }
}

impl<W: Write> EmitC<W> for Function {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        self.emit_c_header(writer)?;
        writer.string(" {")?;
        writer.newline()?;
        for block in self.body.iter() {
            writer.emit(block)?;
        }
        writer.string("}")?;
        writer.newline()?;
        Ok(())
    }
}

const C_PRELUDE: &str = include_str!("../../prelude/prelude.c");

impl<W: Write> EmitC<W> for CompilationUnit {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        write!(writer, "{}", C_PRELUDE)?;

        for function in self.functions.iter() {
            function.emit_c_prototype(writer)?;
        }

        for function in self.functions.iter() {
            function.emit_c(writer)?;
        }

        Ok(())
    }
}
