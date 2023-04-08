use crate::ice::{InternalCompilerError::*, *};

/// Delayed is a type that's practically identical to `Option`, but is
/// used widely in the compiler to specify information that will be "filled in"
/// later on in the compilation process. This is such a common pattern that
/// we create our own type for it rather than use `Option`, just for the sake
/// of clarity.
#[derive(Copy, Clone, Debug)]
pub enum Delayed<T> {
    Filled(T),
    Empty,
}

pub(crate) use Delayed::*;

impl<T> std::convert::From<Delayed<T>> for Option<T> {
    fn from(value: Delayed<T>) -> Self {
        match value {
            Delayed::Filled(t) => Self::Some(t),
            Delayed::Empty => Self::None,
        }
    }
}

impl<T> Delayed<T> {
    pub fn as_ref(&self) -> Delayed<&T> {
        match *self {
            Filled(ref t) => Filled(t),
            Empty => Empty,
        }
    }
    pub fn as_mut(&mut self) -> Delayed<&mut T> {
        match *self {
            Filled(ref mut t) => Filled(t),
            Empty => Empty,
        }
    }
    /// Will throw an internal compiler error if the `Delayed<T>`
    /// is already filled!
    pub fn assert_fill(&mut self, t: T) {
        match self {
            Filled(..) => ice_error!(DelayedAlreadyFilled),
            Empty => *self = Filled(t),
        }
    }
    /// Ignores whether the `Delayed<T>` is already filled.
    pub fn overwrite(&mut self, t: T) {
        *self = Filled(t);
    }
}
