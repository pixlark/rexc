# `for` loops

A `for` loop is syntactic sugar for getting the `IntoIterator` through `.into_iter()` and successively calling `.next()` on that iterator.

The function `into_iter()` takes `self` without borrowing, meaning the value is **moved** to create the iterator. Therefore, naively iterating over a collection will *consume* that collection.

To iterate by *reference*, use:

 - `.iter()` which returns an iterator that produces immutable borrows.
 - `.iter_mut()` which returns an iterator that produces mutable borrows.

*Sidenote:* `into_iter()` is a function that implements the `IntoIterator` trait. `iter()` and `iter_mut()`, however, are just convention. A collection is not guaranteed to have them - however, if it can produce an iterator, it *will* have `into_iter()`.

# `rustdoc`

To generate documentation, including private items (which you want):

 * `rustdoc src/main.rs --document-private-items`

In rustdoc, `///` is an "external" comment that modifies the thing that comes right after it. `//!` is an "internal" comment that modifies whatever it's within. Place a `//!` comment at the beginning of a module to add a module description.
