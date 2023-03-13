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
mod desugar;
mod internal_error;
mod ir;
mod parse;
mod typecheck;
mod validation;

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
    let mut ast = match parse::file(&source) {
        Ok((_, ast)) => ast,
        Err(err) => {
            println!("Parse Error!\n  {}", err);
            return;
        }
    };

    // 2. Desugaring (Sugared Untyped AST -> Desugared Untyped AST)
    ast.desugar();

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
