#![allow(dead_code)]

pub trait InternalCompilerErrorable<T> {
    fn rexc_unwrap(self, msg: &str) -> T;
}

impl<T> InternalCompilerErrorable<T> for Option<T> {
    fn rexc_unwrap(self, msg: &str) -> T {
        match self {
            Some(t) => t,
            None => {
                eprintln!("** Internal Compiler Error! **\n{}", msg);
                panic!("Internal Compiler Error");
            }
        }
    }
}

/// Unwraps an Option<T> that should have been "filled in" during the
/// typechecking phase (usually an inferred type).
pub trait ExpectedFromTypechecker<T> {
    fn expected_from_typechecker(self) -> T;
}

impl<T> ExpectedFromTypechecker<T> for Option<T> {
    fn expected_from_typechecker(self) -> T {
        match self {
            Some(t) => t,
            None => {
                eprintln!(
                    "** Internal Compiler Error! **\nSomehow a {:?} got past the typechecker without being filled in!",
                    std::any::type_name::<Self>(),
                );
                panic!("Internal Compiler Error");
            }
        }
    }
}

impl<T, E> InternalCompilerErrorable<T> for Result<T, E> {
    fn rexc_unwrap(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(..) => {
                eprintln!("** Internal Compiler Error! **\n{}", msg);
                panic!("Internal Compiler Error")
            }
        }
    }
}

pub fn rexc_panic(msg: &str) -> ! {
    eprintln!("** Internal Compiler Error! **\n{}", msg);
    panic!("Internal Compiler Error");
}

pub fn rexc_assert(pred: bool) {
    if !pred {
        eprintln!("** Internal Compiler Error: Assertion Failed! **");
    }
    assert!(pred);
}
