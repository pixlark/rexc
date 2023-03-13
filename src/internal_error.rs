pub trait InternalCompilerErrorable<T> {
    fn rexc_unwrap(self, msg: &str) -> T;
}

impl<T> InternalCompilerErrorable<T> for Option<T> {
    fn rexc_unwrap(self, msg: &str) -> T {
        match self {
            Some(t) => t,
            None => panic!("** Internal Compiler Error! **\n{}", msg),
        }
    }
}

impl<T, E> InternalCompilerErrorable<T> for Result<T, E> {
    fn rexc_unwrap(self, msg: &str) -> T {
        match self {
            Ok(t) => t,
            Err(..) => panic!("** Internal Compiler Error! **\n{}", msg),
        }
    }
}

pub fn rexc_panic(msg: &str) -> ! {
    eprintln!("** Internal Compiler Error! **\n");
    panic!("{}", msg);
}

pub fn rexc_assert(pred: bool) {
    if !pred {
        eprintln!("** Internal Compiler Error: Assertion Failed! **");
    }
    assert!(pred);
}
