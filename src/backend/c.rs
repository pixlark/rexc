//! The C backend for Rexc. This is where IR code gets translated directly
//! into C code.

use std::io::{LineWriter, Result, Write};

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
    fn variable(&mut self, i: usize) -> EmitResult;
    fn label(&mut self, i: usize) -> EmitResult;
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

    fn variable(&mut self, i: usize) -> EmitResult {
        write!(self, "_{}", i)
    }

    fn label(&mut self, i: usize) -> EmitResult {
        write!(self, "_b{}", i)
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
            Type::Int => write!(writer, "int"),
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

impl<W: Write> EmitC<W> for Rhs {
    fn emit_c(&self, writer: &mut LineWriter<W>) -> EmitResult {
        match self {
            Rhs::Parameter(s) => writer.string(s),
            Rhs::Variable(var) => writer.variable(*var),
            Rhs::Literal(literal) => writer.emit(literal),
            Rhs::Operation(op, left, right) => {
                writer.string("(")?;
                writer.emit(left.as_ref())?;
                writer.space()?;
                match *op {
                    Operation::Add => writer.string("+"),
                }?;
                writer.space()?;
                writer.emit(right.as_ref())?;
                writer.string(")")?;
                Ok(())
            }
            Rhs::FunctionCall(name, arguments) => {
                writer.string(name)?;
                writer.string("(")?;
                for (i, argument) in arguments.iter().enumerate() {
                    writer.emit(argument)?;
                    if i < arguments.len() - 1 {
                        writer.string(", ")?;
                    }
                }
                writer.string(")")?;
                Ok(())
            }
        }?;
        Ok(())
    }
}

impl Function {
    fn emit_c_header<W: Write>(&self, writer: &mut LineWriter<W>) -> EmitResult {
        self.returns.emit_c(writer)?;
        writer.space()?;
        writer.string(&self.name)?;
        writer.string("(")?;
        for (i, pair) in self.parameters.iter().enumerate() {
            let type_ = &pair.0;
            let name = &pair.1;
            writer.emit(type_)?;
            writer.space()?;
            writer.string(name)?;
            if i < self.parameters.len() - 1 {
                writer.string(", ")?;
            }
        }
        writer.string(")")?;
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
        writer.label(self.index)?;
        writer.string(":")?;
        writer.newline()?;
        // Block consists of assignments without control flow
        for assignment in self.assignments.iter() {
            writer.emit(&assignment.type_)?;
            writer.space()?;
            writer.variable(assignment.lhs)?;
            writer.string(" = ")?;
            writer.emit(&assignment.rhs)?;
            writer.string(";")?;
            writer.newline()?;
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