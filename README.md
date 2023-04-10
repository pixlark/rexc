## `rexc` is an experimental systems programming language written in Rust

I've been making random little programming languages [for](https://github.com/pixlark/Badge) [a](https://github.com/pixlark/march-lang) [long](https://github.com/pixlark/winter-lang) [time](https://github.com/pixlark/sync).

It's been a while since I've worked on one, however (college ramped up, and then I worked at Tesla for a while which took up a lot of time and energy). And in the meantime I've come up with some ideas.

So `rexc` is an attempt at those ideas in the form of one big programming language, in which I plan to cram every little trinket and doodad that I can think of.

### Some example code

This code builds up a linked list data structure, then traverses it to output a sum:

```
data Link {
    value: int
    next: *Link
}

function main() -> int {
    var list: *Link = nil
    list = appendList(list, 1)
    list = appendList(list, 2)
    list = appendList(list, 3)

    # Outputs 6 (1 + 2 + 3)
    print(sumOverList(list))

    return 0
}

function sumOverList(list: *Link) -> int {
    if list == nil {
        return 0
    }
    return list.value + sumOverList(list.next)
}

function appendList(list: *Link, to_append: int) -> *Link {
    if list == nil {
        return alloc(new Link { value = to_append, next = nil })
    }
    var iter: *Link = list
    while iter.next != nil {
        iter = iter.next
    }
    iter.next = alloc(new Link { value = to_append, next = nil })
    return list
}
```

### How to build?

Currently the following platforms are supported:

 - *nix
 - Windows via MSYS2

For any of these, simply run the `build-from-scratch.bash` script.

### What features are planned?

I have a bunch of nebulous ideas floating around my head, but here are a couple concrete ones that I know I want to implement:

#### My ideal error/exceptions system

Here is the very basic, barebones plan for exceptions. The system should get more expressive as features are added on, but this is what I consider essential.

An error is just a piece of data. An integer, a string, a data structure... just a piece of data.

Errors can exist in two states. The checked state and the unwind state.

##### The checked state

```
function readFromFile(filename: string) throws IOError {
    var file: *File = openFile(filename, "r")
    # ...
}
```

In this case, the checked error is defined in the header type itself, and so we don't have to do anything in particular when calling `openFile` - if it returns an errors, that automatically gets returned from us.

```
function readFromFile(filename: string) {
    try {
        var file: *File = openFile(filename, "r")
        # ...
    } catch e: IOError {
        # do something with e
    }
}
```

In this case, the error doesn't need to be a part of the header type, because we're dealing with it explicitly via a `try`/`catch` block.

```
function mightError() throws MightError {
    throw new MightError {
        # ...
    }
}
```

And to throw a new error, we just use the `throw` statement.

##### The unchecked state

```
function readFromFile(filename: string) {
    try {
        var file: *File = openFile(filename, "r")
    } catch e: IOError {
        throw unwind e
    }
}
```

Here, we convert our IOError from the checked state to the unchecked state via the `unwind` keyword. This will unwind the callstack until either it hits the language runtime before `main`, or hits a handler as we'll see below.

```
function ioHandler() {
    try {
        readFromFile("bar.txt")
    } catch unwind e: IOError {
        # do something with e
    }
}
```

Our handler can catch an unchecked exception just like a checked exception, again using the `unwind` keyword. But the trick here is that none of the functions in the middle had to be annotated with the error type.

I think both checked and unchecked exceptions have a place in most programs. In this compiler, for example, basic, expected errors that the programmer might make (say with syntax) as represented as checked exceptions in Rust (i.e. `Result`), whereas internal compiler errors that represent truly exceptional circumstances are represented as unchecked exceptions (i.e. `panic!`).

#### Simple C interop

Because we compile to C as a first-class target, it shouldn't be too hard to figure out a very clean way to interoperate with C code. This way, rexc programs get the benefit of having access to any of the tens of thousands of C libraries out there. Personally, I plan to use this feature to make games with rexc.

#### Simple but powerful generics system

I love Haskell/Rust and their exceptionally complex but elegantly beautiful generics systems. I have this perhaps-crazy idea, however, that for 90% of use-cases, a very simple templating system would work wonderfully instead.

I need to do more research on this. I believe D has a similar system to what I'm thinking of, so I want to tinker with that and see what I get from it.

#### Some sort of interface/typeclass/protocol system but this is very nebulous

#### First-class support for monads?

Monads are the single "killer feature" that place Haskell leagues ahead of its competitors. Rust is a little bit on the Monad train, but is limited by its borrow checker. A simple, garbage-collected, otherwise imperative programming language that somehow takes advantage of the Monad pattern in a non-clumsy, first-class way would be a godsend.

Much like my other ideas, this is gonna take a lot more refinement before I know how it'll look.

### Why Rust?

In the past I've mostly used C++ to write my lanugages. This is for a few reasons: one, it's easy to reason about performance; two, it's "easy" to manage lots and lots of complex state (if you don't care about memory safety, that is); and three, it was just what I knew best.

Then after working at Tesla for a while and being indoctrinated into the cult of Haskell, I made some attempts at programming languages written in Haskell. I still think that it's really the best tool for that job (Haskell is the best language for any job honestly, I love it), but it has a downside in that you need to be *really* good at it to have any kind of development speed on large-scale projects like a compiler. And I just don't yet have that speed, so writing a compiler in Haskell is painfully slow.

So I've landed on the **best of both worlds** - Rust is imperative, stateful, and good for reasoning about performance like C++, but it's also full of functional features and idioms like Haskell. And as a plus, it doesn't run into *any* of the rampant memory safety issues that C++ does.

So far, I've been very happy with my choice - I love Rust!
