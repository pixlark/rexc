//! Rexc is an experimental language that compiles to C. It is a playground,
//! essentially, for testing out interesting language features. Don't take it
//! too seriously.
//!
//! The compiler follows a linear pipeline from source to emitted C:
//!   1. Parsing
//!     - Converts source file to abstract syntax tree.
//!   2. Pre-Typecheck Desugaring
//!     - Transforms complex elements of the abstract syntax tree into simple
//!       combinations of more fundamental elements. Doesn't require any complex
//!       type information.
//!   3. Typechecking
//!     - Validates the type system, ensuring that there are no type-level
//!       errors in the source.
//!   4. Post-Typecheck Desugaring
//!     - Transforms complex elements of the abstract syntax tree into simple
//!       combinations of more fundamental elements. Requires some type information
//!       that has been filled in by the typechecker.
//!   5. Validation
//!     - Performs analysis on the typed, desugared AST to catch errors before
//!       construction starts (at which point any issues become Internal Compiler
//!       Errors which ideally should not be user-facing)
//!   6. Construction
//!     - Transforms the abstract syntax tree into intermediate representation,
//!       which is linearized into groups of basic blocks connected by gotos.
//!   7. Emitting
//!     - The C backend (or in the future possible another backend) takes the
//!       intermediate representation and uses it to write out a .c file, which
//!       can then be compiled into our final executable.

mod ast;
mod backend;
mod construct;
mod desugar;
mod error;
mod internal_error;
mod ir;
mod parse;
mod typecheck;
mod validation;
mod visitor;

use std::rc::Rc;

use bitflags::bitflags;

use backend::c::EmitC;
use internal_error::*;

bitflags! {
    struct CompileFlags: u32 {
        const SHOW_EMITTED = 0b00000001;
        const EMIT_ONLY    = 0b00000010;
    }
}

fn exit(msg: &str) -> ! {
    eprintln!("Error!\n  {}", msg);
    std::process::exit(1);
}

trait OrExit<T> {
    fn or_exit(self, msg: &str) -> T;
}

impl<T> OrExit<T> for Option<T> {
    fn or_exit(self, msg: &str) -> T {
        match self {
            Some(t) => t,
            None => exit(msg),
        }
    }
}

impl<T, E> OrExit<T> for Result<T, E> {
    fn or_exit(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(..) => exit(msg),
        }
    }
}

fn invoke_gcc(
    file_path: std::path::PathBuf,
    emit_path: std::path::PathBuf,
    gcc_path: Option<String>,
    _flags: CompileFlags,
) {
    // TODO(Brooke): This is all very specific to windows msys2...
    // TODO(Brooke): So much .unwrap here omg please make this good and not a hack!

    // Assume `invoke_gcc` only called when EMIT_ONLY is not enabled.
    let gcc_path = gcc_path.unwrap();

    let executable_path = file_path.with_extension("exe");

    let mut command = std::process::Command::new(&gcc_path);

    let gcc_dir = std::path::Path::new(&gcc_path)
        .ancestors()
        .nth(1)
        .or_exit("Malformed GCC path.");

    let mut path = std::env::var("PATH").or_exit("No PATH variable defined in environment.");

    // TODO(Brooke): Technically we should be supporting non-utf8 paths.
    path.extend(format!(";{}", gcc_dir.to_str().or_exit("Malformed GCC path.")).chars());

    //TODO(Brooke): Technically we should also be supporting UNC paths.
    let libgc = {
        let mut path = std::env::current_exe().or_exit("Unable to get path to rexc executable.");
        path.pop();
        path.push("libgc.a");
        dunce::canonicalize(path).or_exit("UNC paths are not currently supported.")
    };

    if !libgc.exists() {
        exit("libgc.a not found! (Should be beside rexc executable).");
    }

    command
        .env("PATH", path)
        .arg(emit_path.to_str().or_exit("Malformed source path."))
        .arg(libgc)
        .arg("-o")
        .arg(
            executable_path
                .to_str()
                .or_exit("Path to rexc executable is malformed."),
        )
        .arg("-I")
        .arg(".");

    println!("Invoking gcc...\n    {:?}", command);

    let output = command.output().or_exit("Failed to run GCC.");
    if output.status.code() != Some(0) {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        panic!("Aborted compilation!")
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

    let source = Rc::new(std::fs::read_to_string(&path).or_exit("Unable to read source file."));

    // 1. Parse (Rexc source -> Sugared Untyped AST)
    let filename = Rc::new(String::from("<main>"));
    //let source_locate = parse::Location::new_extra(&source, filename.clone());
    let lexer = match parse::Lexer::new(source, filename) {
        Ok(lexer) => lexer,
        Err(err) => {
            eprintln!("Parse error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err.kind);
            return;
        }
    };
    let mut parser = parse::Parser::new(lexer);
    let mut ast = match parser.file() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err.kind);
            return;
        }
    };
    //dbg!(&ast);

    // 2. Pre-Typecheck Desugaring (Sugared Untyped AST -> Partially Desugared Untyped AST)
    ast.desugar_pre_typecheck();

    // 3. Typecheck (Partially Desguared Untyped AST -> Partial Desugared Typed AST)
    match ast.typecheck() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("Type error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err);
            return;
        }
    }

    // 4. Post-Typecheck Desugaring (Partially Desugared Typed AST -> Desugared Typed AST)
    ast.desugar_post_typecheck();

    // 5. Validation (Desugared Typed AST -> Desugared Typed AST)
    match ast.validate() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("Error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err);
            return;
        }
    }

    // 6. Construct (Desugared Typed AST -> IR)
    let ir_ = ast.into_ir();

    // 7. Emit (IR -> C source)
    let mut writer = std::io::LineWriter::new(Vec::new());

    ir_.emit_c(&mut writer)
        .rexc_unwrap("There was an error with the C backend!");

    let buffer = writer
        .into_inner()
        .rexc_unwrap("There was an error with the C backend!");
    let emitted_c =
        String::from_utf8(buffer).rexc_unwrap("Somehow the C backend emitted invalid UTF-8!");

    // TODO(Brooke): Only emit to string if we're printing the emitted code, otherwise
    //               emit directly to the `emit_path` file.
    if flags.contains(CompileFlags::SHOW_EMITTED) {
        println!("Emitted Code:\n```\n{}\n```", emitted_c);
    }

    let emit_path = path.with_extension("rx.c");
    std::fs::write(&emit_path, &emitted_c).or_exit("Couldn't emit C to intermediate file.");

    if flags.contains(CompileFlags::EMIT_ONLY) {
        return;
    }

    // 8. Invoke GCC (C source -> Executable)
    invoke_gcc(path, emit_path, gcc_path, flags);
}

fn main() {
    let mut cmd = clap::Command::new("rexc")
        .about("An experimental language")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            clap::Command::new("compile")
                .about("Invoke the compiler directly on a source file")
                .arg(clap::arg!(--source <PATH> "The source file to be compiled").required(true))
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
                gcc_path.cloned(),
                flags,
            );
        }
        Some((_, _)) => unreachable!(),
        None => unreachable!(),
    }
}
