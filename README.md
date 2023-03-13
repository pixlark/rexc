## `rexc` is an experimental systems programming language written in Rust

I've been making random little programming languages [for](https://github.com/pixlark/Badge) [a](https://github.com/pixlark/march-lang) [long](https://github.com/pixlark/winter-lang) [time](https://github.com/pixlark/sync).

It's been a while since I've worked on one, however (college ramped up, and then I worked at Tesla for a while which took up a lot of time and energy). And in the meantime I've developed some ideas.

So `rexc` is an attempt to divulge those ideas in the form of one frankenstinean programming language, in which I plan to cram every cool little trinket and doodad that I've thought up in the past couple years.

### What features are planned?

I have a cornucopia of nebulous ideas floating around my head, but here are a couple concrete ones that I know I want to implement:

#### Explicit exceptions

Exceptions get a bad rap from both sides: low-level C people love to hate on the overhead and overall bloaty OOP-ness that exceptions add to a C++ codebase. And functional Haskellers/Rustaceans/etc love to hate on exceptions because they've found a strictly better solution (`Either` in Haskell and `Result` in Rust).

But I think exceptions are better than that - they provide an incredibly useful paradigm in which the "happy path" code that you're really trying to focus on takes up 99% of the space, whereas "error path" code that tends to be very boilerplate and samey takes up far less space.

Compare this to Go, the modern language with, in my opinion, the absolute *worst* take on error handling. In Go, errors are just values passed back in the return position. So *all of your Go code* is absolutely **filled** with this:

```
func foo() Data, ErrorType {
    data, err := callLibraryFunction()
    if err != nil {
        return nil, err
    }
    more_data, err := callAnotherLibraryFunction(data)
    if err != nil {
        return nil, err
    }
    // I think you get the point...
}
```

Notice here that the error path takes up 75% of the code, and the happy path only 25%. And what do we get with that 75%? An ugly, uncomposable mess - and a mess in which we *must* define our `Data` and `ErrorType` types as pointers, because otherwise there's no `nil` value to return!

Now of course, Rust/Haskell/others have solved this problem with `Either`/`Result`, as I mentioned above. But let's not forget that exceptions solve this problem too! This is how the same code would look if `ErrorType` were an *exception*:

```
func foo() Data {
    data := callAnotherLibraryFunction(callLibraryFunction())
}
```

But we've introduced another problem now: the error path is completely *implicit* - it takes up quite literally 0% of the code! So how is somebody calling `foo()` meant to know that this exception can even happen? How is somebody reading the definition of `foo` meant to know that an exception might be thrown at that line, but not at another?

This is what leads to "exception madness" in languages like C#, Python, Java, etc... it's so difficult to figure out at any point what exceptions might be thrown that people end up writing godawful junk like this:

```
try {
    libraryFunction()
} catch (e: Exception) {
    log("Exception thrown:", e)
    // continue
}
```

But this is *such* an easy problem to fix, and one that doesn't at all require you to introduce algebraic data types and `Either`/`Result` monads to your language. Simply add these rules:

 1. All functions must be notated with every type of exception they can throw.
 2. Require that exception-throwing function calls be explicitly notated with what to do when an exception occurs.

That's it! Here's an example of what I mean, in terms of what I imagine `rexc` to eventually look like:

```
function foo(arg: int) throws FooException, LibraryException {
    // We can throw new exceptions (where exceptions are just any data type!)
    if isBad(arg) {
        throw FooException { data: arg }
    }
    // We can automatically propagate exceptions (this is the implicit case in C#, Python, etc)
    var result = try libraryCall();
    // We can subsitute default values in case of a certain exception type
    var result_or_default = libraryCall() catch (LibraryException) default_value;
    // And of course, we can use a traditional try/catch to handle the error path
    try {
        var result_try_catch = libraryCall();
    } catch (LibraryException e) {
        logException("Exception thrown in foo:", e);
        throw e;
    }
    // Perhaps we could even define a `rethrow` keyword
    var result_or_rethrow = libraryCall() rethrow (LibraryException) FooException { data: arg }
}
```

Now obviously, this is really just a *language-based* recreation of what languages like Rust and Haskell provide via their *standard library*, which in some sense is just worse (for one, you can't expand error-handling capabilities without changing the language spec). So why expand your language spec when you can expand your stdlib?

But I think that perspective is missing the fact that languages which have exceptions (again C#, Python, Java, etc to name a few) **don't** have these templatized algebraic type systems that are really required to support such an error-handling scheme - and it's unreasonable to expect that *new* languages going forward will support that either.

So I think this is a good middle ground - instead of traditional, implicit, mistake-prone exception systems, something like what I've outlined here gives you all the benefits without requiring a super complex type system a-la Haskell/Rust.

### Why Rust?

In the past I've mostly used C++ to write my lanugages. This is for a few reasons: one, it's easy to reason about performance; two, it's "easy" to manage lots and lots of complex state (if you don't care about memory safety, that is); and three, it was just what I knew best.

Then after working at Tesla for a while and being indoctrinated into the cult of Haskell, I made some attempts at programming languages written in Haskell. I still think that it's really the best tool for that job (Haskell is the best language for any job honestly, I love it), but it has a downside in that you need to be *really* good at it to have any kind of development speed on large-scale projects like a compiler. And I just don't yet have that speed, so writing a compiler in Haskell is painfully slow.

So I've landed on the **best of both worlds** - Rust is imperative, stateful, and good for reasoning about performance like C++, but it's also full of functional features and idioms like Haskell. And as a plus, it doesn't run into *any* of the rampant memory safety issues that C++ does.

So far, I've been very happy with my choice - I love Rust!
