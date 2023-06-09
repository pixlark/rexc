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

#![allow(clippy::wrong_self_convention)]

mod ast;
mod backend;
mod construct;
mod delayed;
mod desugar;
mod error;
mod ice;
mod ir;
mod parse;
mod typecheck;
mod validation;
mod visitor;

use std::rc::Rc;

use bitflags::bitflags;

use backend::c::EmitC;
use ice::{InternalCompilerError::*, *};

bitflags! {
    struct CompileFlags: u32 {
        const SHOW_EMITTED = 0b00000001;
        const EMIT_ONLY    = 0b00000010;
        const EMIT_DEBUG   = 0b00000100;
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

fn invoke_gcc(file_path: std::path::PathBuf, emit_path: std::path::PathBuf, flags: CompileFlags) {
    // Assume `invoke_gcc` only called when EMIT_ONLY is not enabled.
    let gcc_path = which::which("gcc").or_exit("Could not find gcc on the PATH.");

    let executable_path = file_path.with_extension("exe");

    let mut command = std::process::Command::new(&gcc_path);

    let gcc_dir = std::path::Path::new(&gcc_path)
        .ancestors()
        .nth(1)
        .or_exit("Malformed GCC path.");

    let mut path = std::env::var("PATH").or_exit("No PATH variable defined in environment.");

    // TODO(Brooke): Technically we should be supporting non-utf8 paths.
    path.extend(format!(";{}", gcc_dir.to_str().or_exit("Malformed GCC path.")).chars());

    let project_dir = directories::ProjectDirs::from("", "", "rexc")
        .or_exit("Could not generate project directory data path.");
    let mut libgc = std::path::PathBuf::from(project_dir.data_dir());
    libgc.push("libgc.a");

    if !libgc.exists() {
        exit("libgc.a not found! (Have you run `./build-from-scratch.bash`?");
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
        .arg(".")
        .arg("-l")
        .arg("pthread");

    if flags.contains(CompileFlags::EMIT_DEBUG) {
        command.arg("-g");
    }

    println!("Invoking gcc...\n    {:?}", command);

    let output = command.output().or_exit("Failed to run GCC.");
    if output.status.code() != Some(0) {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        panic!("Aborted compilation!")
    }
}

fn compile(path: std::path::PathBuf, flags: CompileFlags) -> Result<(), ()> {
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
            return Err(());
        }
    };
    let mut parser = parse::Parser::new(lexer);
    let mut ast = match parser.file() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Parse error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err.kind);
            return Err(());
        }
    };

    // 2. Pre-Typecheck Desugaring (Sugared Untyped AST -> Partially Desugared Untyped AST)
    ast.desugar_pre_typecheck();

    // 3. Typecheck (Partially Desguared Untyped AST -> Partial Desugared Typed AST)
    match ast.typecheck() {
        Ok(()) => {}
        Err(err) => {
            eprintln!("Type error! Originated at:");
            err.span.error_message();
            eprintln!("  {}", err);
            return Err(());
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
            return Err(());
        }
    }

    // 6. Construct (Desugared Typed AST -> IR)
    let ir_ = ast.into_ir();

    // 7. Emit (IR -> C source)
    let mut writer = std::io::LineWriter::new(Vec::new());

    ir_.emit_c(&mut writer)
        .or_exit("There was an IO error when emitting via the C backend.");

    let buffer = writer
        .into_inner()
        .or_exit("There was an IO error when emitting via the C backend.");
    let emitted_c = String::from_utf8(buffer).unwrap_with_ice(EmittedInvalidUTF8);

    // TODO(Brooke): Only emit to string if we're printing the emitted code, otherwise
    //               emit directly to the `emit_path` file.
    if flags.contains(CompileFlags::SHOW_EMITTED) {
        println!("Emitted Code:\n```\n{}\n```", emitted_c);
    }

    let emit_path = path.with_extension("rx.c");
    std::fs::write(&emit_path, &emitted_c).or_exit("Couldn't emit C to intermediate file.");

    if flags.contains(CompileFlags::EMIT_ONLY) {
        return Ok(());
    }

    // 8. Invoke GCC (C source -> Executable)
    invoke_gcc(path, emit_path, flags);

    Ok(())
}

fn main() {
    let cmd = clap::Command::new("rexc")
        .about("An experimental language")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(
            clap::Command::new("compile")
                .about("Invoke the compiler directly on a source file")
                .arg(clap::arg!(--source <PATH> "The source file to be compiled").required(true))
                .arg(clap::arg!(--"show-emitted" "Print emitted C code to stdout"))
                .arg(clap::arg!(--"emit-only" "Only emit C code, don't call GCC"))
                .arg(clap::arg!(--"debug-symbols" "Compile the generated code with debug symbols (does not actually refer to .rx file, only to generated .c!)")),
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
            flags.set(
                CompileFlags::EMIT_DEBUG,
                sub_matches
                    .get_one::<bool>("debug-symbols")
                    .map_or(false, |b| *b),
            );

            match compile(std::path::PathBuf::from(source.unwrap()), flags) {
                Ok(()) => {}
                Err(()) => std::process::exit(1),
            }
        }
        Some((_, _)) => ice_unreachable!(),
        None => ice_unreachable!(),
    }
}
