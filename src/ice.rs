use std::io::Write;

use crate::delayed::*;

#[derive(Debug)]
pub enum InternalCompilerError {
    // General-use
    Unreachable,
    AssertionFailure,
    DelayedAlreadyFilled,
    // Desugarer
    ShouldHaveBeenDesugared,
    // Typechecker
    IncompleteTypingPass,
    ComparedUntypedDataTypeReferences,
    UnfilledTypeInference,
    InvalidTypeInference,
    // - These two are for non-`Type` data that the typechecker either
    //   doesn't fill in, or fills with invalid data.
    //   Example: `needs_dereference` on `FieldAccess`.
    UnfilledTypecheckerData,
    InvalidTypecheckerData,
    // Constructor
    ConstructedFromIncompleteConstructor,
    UnboundVariableInConstructPhase,
    InvalidBlockTerminator,
    // Backend
    EmittedInvalidUTF8,
}

impl std::fmt::Display for InternalCompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "\
******************************
** Internal Compiler Error! **
******************************
THIS IS NOT YOUR FAULT - there is a bug in the compiler.
Please submit a bug report issue at https://github.com/pixlark/rexc."
        )?;
        writeln!(f, ">> Error: {:?}", self)?;
        Ok(())
    }
}

impl InternalCompilerError {
    #[allow(unused_must_use)]
    pub fn abort(&self) -> ! {
        eprint!("{}", self);
        std::io::stderr().flush();
        if cfg!(debug_assertions) {
            // In debug mode, we panic! for our abort because that
            // gives us automatic backtraces.
            panic!("InternalCompilerError");
        } else {
            // Otherwise we do a normal abort.
            std::process::abort();
        }
    }
}

pub trait InternalCompilerErrorable<T> {
    fn unwrap_with_ice(self, err: InternalCompilerError) -> T;
}

impl<T> InternalCompilerErrorable<T> for Option<T> {
    fn unwrap_with_ice(self, err: InternalCompilerError) -> T {
        match self {
            Some(t) => t,
            None => err.abort(),
        }
    }
}

impl<T> InternalCompilerErrorable<T> for Delayed<T> {
    fn unwrap_with_ice(self, err: InternalCompilerError) -> T {
        match self {
            Filled(t) => t,
            Empty => err.abort(),
        }
    }
}

impl<T, E> InternalCompilerErrorable<T> for Result<T, E> {
    fn unwrap_with_ice(self, err: InternalCompilerError) -> T {
        match self {
            Ok(t) => t,
            Err(..) => err.abort(),
        }
    }
}

macro_rules! ice_unreachable {
    () => {
        $crate::ice::InternalCompilerError::Unreachable.abort()
    };
}

pub(crate) use ice_unreachable;

macro_rules! ice_error {
    ($err:expr) => {
        $err.abort()
    };
}

pub(crate) use ice_error;

macro_rules! ice_assert {
    ($expr:expr) => {
        if (!$expr) {
            $crate::ice::InternalCompilerError::AssertionFailure.abort()
        }
    };
}

pub(crate) use ice_assert;
