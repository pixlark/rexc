//! Rexc is an experimental language that compiles to C. It is a playground,
//! essentially, for testing out interesting language features. Don't take it
//! too seriously.
//!
//! The compiler follows a linear pipeline from source to emitted C:
//!   1. Parsing
//!     - Converts source file to abstract syntax tree.
//!   2. Desugaring (Not yet implemented)
//!     - Transforms complex elements of the abstract syntax tree into simple
//!       combinations of more fundamental elements.
//!   3. Typechecking
//!     - Validates the type system, ensuring that there are no type-level
//!       errors in the source.
//!   4. Validation
//!     - Performs analysis on the typed, desugared AST to catch errors before
//!       construction starts (at which point any issues become Internal Compiler
//!       Errors which ideally should not be user-facing)
//!   5. Construction
//!     - Transforms the abstract syntax tree into intermediate representation,
//!       which is linearized into groups of basic blocks connected by gotos.
//!   6. Emitting
//!     - The C backend (or in the future possible another backend) takes the
//!       intermediate representation and uses it to write out a .c file, which
//!       can then be compiled into our final executable.

mod ast;
mod backend;
mod construct;
mod internal_error;
mod ir;
mod parse;
mod typecheck;
mod validation;

// TODO(Brooke): Move this out of here!!!
#[cfg(test)]
mod test {
    //! These tests all ultimately do a string compare on the emitted
    //! C code. This means that the tests are dependent - i.e. if the C
    //! emitter breaks, all other tests will break. If the AST->IR
    //! construction breaks, the parser will break, and so on.
    //!
    //! Eventually it'll be better to completely isolate, but for now that's
    //! just a hassle because then you're either doing huge recursive matches
    //! or implementing Eq for everything in the program...

    use super::ast;
    use super::backend::c::EmitC;
    use super::ir;
    use super::parse;
    use super::typecheck::{TypeError, TypeErrorKind};
    use std::io;

    #[test]
    fn test_full_pipeline() {
        let source = "\
function foo(x: int, b: bool) -> int {
    if b {
        var y : int = x + 1
        return y
    }
    return x
}
";
        let (_, mut ast) = parse::function(source).unwrap();
        ast.typecheck().unwrap();
        ast.validate().unwrap();

        let ir_ = ast.to_ir();

        let mut w = io::LineWriter::new(Vec::new());

        ir_.emit_c_prototype(&mut w).unwrap();
        ir_.emit_c(&mut w).unwrap();

        let buffer = w.into_inner().unwrap();
        let s = String::from_utf8(buffer).unwrap();

        println!("\n\n* Emitted code:\n```\n{}```\n", s);
    }

    #[test]
    fn test_typecheck_failure() {
        //! One test for each of the possible TypeErrorKind errors
        {
            // BadVariableDeclarationType

            let mut f = ast::Function {
                name: String::from("foo"),
                parameters: Vec::new(),
                returns: ast::Type::Bool,
                body: vec![ast::Statement::MakeVariable(ast::MakeVariable {
                    type_: ast::Type::Bool,
                    lhs: String::from("x"),
                    rhs: ast::Expression::Literal(ast::Literal::Int(0)),
                })],
            };

            assert!(matches!(
                f.typecheck(),
                Err(TypeError{ kind: TypeErrorKind::BadVariableDeclarationType(v) })
                    if &v == "x"
            ));
        }

        {
            // BadReturnType

            let mut f = ast::Function {
                name: String::from("foo"),
                parameters: Vec::new(),
                returns: ast::Type::Bool,
                body: vec![ast::Statement::Return((
                    None,
                    ast::Expression::Literal(ast::Literal::Int(0)),
                ))],
            };

            assert!(matches!(
                f.typecheck(),
                Err(TypeError {
                    kind: TypeErrorKind::BadReturnType
                })
            ));
        }

        {
            // IncompatibleOperands

            let mut f = ast::Function {
                name: String::from("foo"),
                parameters: Vec::new(),
                returns: ast::Type::Int,
                body: vec![ast::Statement::Return((
                    None,
                    ast::Expression::Operation(
                        ir::Operation::Add,
                        Box::new((None, ast::Expression::Literal(ast::Literal::Int(1)))),
                        Box::new((None, ast::Expression::Literal(ast::Literal::Bool(false)))),
                    ),
                ))],
            };

            assert!(matches!(
                f.typecheck(),
                Err(TypeError {
                    kind: TypeErrorKind::IncompatibleOperands(ir::Operation::Add)
                })
            ));
        }

        {
            // UnboundVariable

            let mut f = ast::Function {
                name: String::from("foo"),
                parameters: Vec::new(),
                returns: ast::Type::Int,
                body: vec![ast::Statement::Return((
                    None,
                    ast::Expression::Variable(String::from("x")),
                ))],
            };

            assert!(matches!(
                f.typecheck(),
                Err(TypeError{ kind: TypeErrorKind::UnboundVariable(v) })
                    if &v == "x"
            ));
        }
    }

    #[test]
    fn test_typecheck() {
        // AST of the following code:
        //  function foo(z: int) -> bool {
        //      var x: int = z
        //      if x + 1 {
        //          return true
        //      }
        //      return false
        //  }
        let mut f = ast::Function {
            name: String::from("foo"),
            parameters: vec![(ast::Type::Int, String::from("z"))],
            returns: ast::Type::Bool,
            body: vec![
                ast::Statement::MakeVariable(ast::MakeVariable {
                    type_: ast::Type::Int,
                    lhs: String::from("x"),
                    rhs: ast::Expression::Variable(String::from("z")),
                }),
                ast::Statement::If(ast::If {
                    condition: (
                        None,
                        ast::Expression::Operation(
                            ir::Operation::Add,
                            Box::new((None, ast::Expression::Variable(String::from("x")))),
                            Box::new((None, ast::Expression::Literal(ast::Literal::Int(1)))),
                        ),
                    ),
                    body: vec![ast::Statement::Return((
                        None,
                        ast::Expression::Literal(ast::Literal::Bool(true)),
                    ))],
                }),
                ast::Statement::Return((None, ast::Expression::Literal(ast::Literal::Bool(false)))),
            ],
        };

        let typecheck_result = f.typecheck();
        if let Err(err) = typecheck_result {
            println!("\n\n* Rexc TYPE ERROR:\n  {}\n", err);
            panic!();
        }
        f.validate().unwrap();

        let ir_f = f.to_ir();

        let mut w = io::LineWriter::new(Vec::new());

        ir_f.emit_c_prototype(&mut w).unwrap();
        ir_f.emit_c(&mut w).unwrap();

        let buffer = w.into_inner().unwrap();
        let s = String::from_utf8(buffer).unwrap();

        println!("\n\n* Emitted code:\n```\n{}```\n", s);

        // TODO(Brooke): Make this an actual test...
    }

    #[test]
    fn test_ast_to_ir() {
        // AST of the following code:
        //  function foo(z: int) -> bool {
        //      var x: int = z
        //      if x + 1 {
        //          return true
        //      }
        //      return false
        //  }
        let f = ast::Function {
            name: String::from("foo"),
            parameters: vec![(ast::Type::Int, String::from("z"))],
            returns: ast::Type::Bool,
            body: vec![
                ast::Statement::MakeVariable(ast::MakeVariable {
                    type_: ast::Type::Int,
                    lhs: String::from("x"),
                    rhs: ast::Expression::Variable(String::from("z")),
                }),
                ast::Statement::If(ast::If {
                    condition: (
                        Some(ast::Type::Int),
                        ast::Expression::Operation(
                            ir::Operation::Add,
                            Box::new((
                                Some(ast::Type::Int),
                                ast::Expression::Variable(String::from("x")),
                            )),
                            Box::new((
                                Some(ast::Type::Int),
                                ast::Expression::Literal(ast::Literal::Int(1)),
                            )),
                        ),
                    ),
                    body: vec![ast::Statement::Return((
                        Some(ast::Type::Int),
                        ast::Expression::Literal(ast::Literal::Bool(true)),
                    ))],
                }),
                ast::Statement::Return((
                    Some(ast::Type::Int),
                    ast::Expression::Literal(ast::Literal::Bool(false)),
                )),
            ],
        };
        let ir_f = f.to_ir();

        let mut w = io::LineWriter::new(Vec::new());

        ir_f.emit_c_prototype(&mut w).unwrap();
        ir_f.emit_c(&mut w).unwrap();

        let buffer = w.into_inner().unwrap();
        let s = String::from_utf8(buffer).unwrap();

        println!("\n\n* Emitted code:\n```\n{}```\n", s);
    }
}

use bitflags::bitflags;

use backend::c::EmitC;

bitflags! {
    struct CompileFlags: u32 {
        const SHOW_EMITTED = 0b00000001;
        const EMIT_ONLY    = 0b00000010;
    }
}

fn compile(path: std::path::PathBuf, gcc_path: Option<String>, flags: CompileFlags) {
    let mut path_ok = false;
    if let Some(ext) = path.extension() {
        if ext == "rx" {
            path_ok = true
        }
    }
    if !path_ok {
        panic!("Bad source file extension (expected .rx)")
    }

    let source = std::fs::read_to_string(&path).unwrap();

    // 1. Parse (Rexc source -> Sugared Untyped AST)
    let mut ast = match parse::function(&source) {
        Ok((_, ast)) => ast,
        Err(err) => {
            println!("Parse Error!\n  {}", err);
            return;
        }
    };

    // 2. Desugaring (Sugared Untyped AST -> Desugared Untyped AST)
    // Not implemented (not yet needed for any constructs)

    // 3. Typecheck (Desugared Untyped AST -> Desugared Typed AST)
    match ast.typecheck() {
        Ok(()) => {}
        Err(err) => {
            println!("Type Error!\n  {}", err);
            return;
        }
    }

    // 4. Validation (Desugared Typed AST -> Desugared Typed AST)
    match ast.validate() {
        Ok(()) => {}
        Err(err) => {
            println!("Error!\n  {}", err);
            return;
        }
    }

    // 5. Construct (Desugared Typed AST -> IR)
    let ir_ = ast.to_ir();

    // 6. Emit (IR -> C source)
    let mut writer = std::io::LineWriter::new(Vec::new());

    ir_.emit_c_prototype(&mut writer).unwrap();
    ir_.emit_c(&mut writer).unwrap();

    let buffer = writer.into_inner().unwrap();
    let emitted_c = String::from_utf8(buffer).unwrap();

    // TODO(Brooke): Only emit to string if we're printing the emitted code, otherwise
    //               emit directly to the `emit_path` file.
    if flags.contains(CompileFlags::SHOW_EMITTED) {
        println!("Emitted Code:\n```\n{}\n```", emitted_c);
    }

    let emit_path = path.with_extension("rx.c");
    std::fs::write(&emit_path, emitted_c).unwrap();

    if flags.contains(CompileFlags::EMIT_ONLY) {
        return;
    }

    // 7. Invoke GCC (C source -> Executable)
    // TODO(Brooke): This is all very specific to windows msys2...
    // TODO(Brooke): So much .unwrap here omg please make this good and not a hack!
    let gcc_path = gcc_path.unwrap();

    let executable_path = path.with_extension("exe");

    let mut command = std::process::Command::new(&gcc_path);

    let gcc_dir = std::path::Path::new(&gcc_path).ancestors().nth(1).unwrap();

    let mut path = std::env::var("PATH").unwrap();
    path.extend(format!(";{}", gcc_dir.to_str().unwrap()).chars());

    command
        .env("PATH", path)
        .arg(emit_path.to_str().unwrap())
        .arg("-o")
        .arg(executable_path.to_str().unwrap());

    println!("Invoking gcc...\n    {:?}", command);

    command.output().unwrap();
}

fn main() {
    let mut cmd = clap::Command::new("rexc")
        .about("An experimental language")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            clap::Command::new("compile")
                .about("Invoke the compiler directly on a source file")
                .arg(clap::arg!(--source <PATH> "The source file to be compiled"))
                .arg(clap::arg!(--gcc <PATH> "Path to GCC"))
                .arg(clap::arg!(--"show-emitted" "Print emitted C code to stdout"))
                .arg(clap::arg!(--"emit-only" "Only emit C code, don't call GCC")),
        );
    let gcc_path_err = cmd.error(
        clap::error::ErrorKind::ArgumentConflict,
        "must provide '--gcc_path'",
    );
    let matches = cmd.get_matches();
    match matches.subcommand() {
        Some(("compile", sub_matches)) => {
            let source = sub_matches.get_one::<String>("source");
            let mut flags = CompileFlags::empty();

            flags.set(
                CompileFlags::SHOW_EMITTED,
                sub_matches
                    .get_one::<bool>("show-emitted")
                    .map_or(false, |b| *b),
            );
            flags.set(
                CompileFlags::EMIT_ONLY,
                sub_matches
                    .get_one::<bool>("emit-only")
                    .map_or(false, |b| *b),
            );

            let gcc_path = sub_matches.get_one::<String>("gcc");

            if !flags.contains(CompileFlags::EMIT_ONLY) && gcc_path.is_none() {
                gcc_path_err.exit();
            }

            compile(
                std::path::PathBuf::from(source.unwrap()),
                gcc_path.map(|s| s.clone()),
                flags,
            );
        }
        Some((_, _)) => unreachable!(),
        None => unreachable!(),
    }
}
