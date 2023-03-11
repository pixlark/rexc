//! Rexc is an experimental language that compiles to C. It is a playground,
//! essentially, for testing out interesting language features. Don't take it
//! too seriously.
//!
//! The compiler follows a linear pipeline from source to emitted C:
//!   1. Parsing
//!     - Converts source file to abstract syntax tree.
//!   2. Desugaring
//!     - Transforms complex elements of the abstract syntax tree into simple
//!       combinations of more fundamental elements.
//!   3. Typechecking
//!     - Validates the type system, ensuring that there are no type-level
//!       errors in the source.
//!   4. Construction
//!     - Transforms the abstract syntax tree into intermediate representation,
//!       which is linearized into groups of basic blocks connected by gotos.
//!   5. Emitting
//!     - The C backend (or in the future possible another backend) takes the
//!       intermediate representation and uses it to write out a .c file, which
//!       can then be compiled into our final executable.

mod ast;
mod backend;
mod construct;
mod ir;
mod parse;
mod typecheck;

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
                        Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
                        Box::new(ast::Expression::Literal(ast::Literal::Bool(false))),
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
                            Box::new(ast::Expression::Variable(String::from("x"))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
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
                            Box::new(ast::Expression::Variable(String::from("x"))),
                            Box::new(ast::Expression::Literal(ast::Literal::Int(1))),
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

    #[test]
    fn test_c_emitter() {
        let f = ir::Function {
            name: String::from("foo"),
            returns: ir::Type::Int,
            parameters: vec![
                (ir::Type::Int, String::from("x")),
                (ir::Type::Int, String::from("y")),
                (ir::Type::Int, String::from("z")),
            ],
            body: vec![
                ir::Block {
                    index: 0,
                    assignments: vec![ir::Assignment {
                        type_: ir::Type::Int,
                        lhs: 0,
                        rhs: ir::Rhs::Parameter(String::from("y")),
                    }],
                    block_terminator: ir::BlockTerminator::ConditionalBranch(
                        1,
                        2,
                        (ir::Type::Int, 0),
                    ),
                },
                ir::Block {
                    index: 1,
                    assignments: vec![
                        ir::Assignment {
                            type_: ir::Type::Int,
                            lhs: 1,
                            rhs: ir::Rhs::Parameter(String::from("x")),
                        },
                        ir::Assignment {
                            type_: ir::Type::Int,
                            lhs: 2,
                            rhs: ir::Rhs::Literal(ir::Literal::Int(10)),
                        },
                        ir::Assignment {
                            type_: ir::Type::Int,
                            lhs: 3,
                            rhs: ir::Rhs::Operation(
                                ir::Operation::Add,
                                Box::new(ir::Rhs::Variable(1)),
                                Box::new(ir::Rhs::Variable(2)),
                            ),
                        },
                        ir::Assignment {
                            type_: ir::Type::Int,
                            lhs: 4,
                            rhs: ir::Rhs::Operation(
                                ir::Operation::Add,
                                Box::new(ir::Rhs::FunctionCall(
                                    String::from("bar"),
                                    vec![ir::Rhs::Operation(
                                        ir::Operation::Add,
                                        Box::new(ir::Rhs::Variable(1)),
                                        Box::new(ir::Rhs::Literal(ir::Literal::Int(1))),
                                    )],
                                )),
                                Box::new(ir::Rhs::Variable(3)),
                            ),
                        },
                    ],
                    block_terminator: ir::BlockTerminator::Return(4, ir::Type::Int),
                },
                ir::Block {
                    index: 2,
                    assignments: Vec::new(),
                    block_terminator: ir::BlockTerminator::Return(0, ir::Type::Int),
                },
            ],
        };

        let mut w = io::LineWriter::new(Vec::new());

        f.emit_c_prototype(&mut w).unwrap();
        f.emit_c(&mut w).unwrap();

        let buffer = w.into_inner().unwrap();
        let s = String::from_utf8(buffer).unwrap();

        println!("\n\n* Emitted code:\n```\n{}```\n", s);
        assert_eq!(
            s,
            "\
int foo(int x, int y, int z);
int foo(int x, int y, int z) {
_b0:
int _0 = y;
if (_0) { goto _b1; } else { goto _b2; }
_b1:
int _1 = x;
int _2 = 10;
int _3 = (_1 + _2);
int _4 = (bar((_1 + 1)) + _3);
return _4;
_b2:
return _0;
}
"
        );
    }
}

use backend::c::EmitC;

// TODO(Brooke): Make a CompileFlags instead of passing in bool
fn compile(
    path: std::path::PathBuf,
    gcc_path: Option<String>,
    show_emitted: bool,
    emit_only: bool,
) {
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

    // 1. Parse (Rexc source -> Untyped AST)
    let (_, mut ast) = parse::function(&source).unwrap();

    // 2. Typecheck (Untyped AST -> Typed AST)
    ast.typecheck().unwrap();

    // 3. Construct (Typed AST -> IR)
    let ir_ = ast.to_ir();

    // 4. Emit (IR -> C source)
    let mut writer = std::io::LineWriter::new(Vec::new());

    ir_.emit_c_prototype(&mut writer).unwrap();
    ir_.emit_c(&mut writer).unwrap();

    let buffer = writer.into_inner().unwrap();
    let emitted_c = String::from_utf8(buffer).unwrap();

    // TODO(Brooke): Only emit to string if we're printing the emitted code, otherwise
    //               emit directly to the `emit_path` file.
    if show_emitted {
        println!("```\n{}\n```\n", emitted_c);
    }

    let emit_path = path.with_extension("rx.c");
    std::fs::write(&emit_path, emitted_c).unwrap();

    if emit_only {
        return;
    }

    // 5. Invoke GCC (C source -> Executable)
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
            let show_emitted = sub_matches
                .get_one::<bool>("show-emitted")
                .map_or(false, |b| *b);
            let emit_only = sub_matches
                .get_one::<bool>("emit-only")
                .map_or(false, |b| *b);

            let gcc_path = sub_matches.get_one::<String>("gcc");

            if !emit_only && gcc_path.is_none() {
                gcc_path_err.exit();
            }

            compile(
                std::path::PathBuf::from(source.unwrap()),
                gcc_path.map(|s| s.clone()),
                show_emitted,
                emit_only,
            );
        }
        Some((_, _)) => unreachable!(),
        None => unreachable!(),
    }
}
