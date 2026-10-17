# sumtype crate [![Latest Version]][crates.io] [![Documentation]][docs.rs] [![GitHub Actions]][actions]

[Latest Version]: https://img.shields.io/crates/v/sumtype.svg
[crates.io]: https://crates.io/crates/sumtype
[Documentation]: https://img.shields.io/docsrs/sumtype
[docs.rs]: https://docs.rs/sumtype/latest/sumtype/
[GitHub Actions]: https://github.com/yasuo-ozu/sumtype/actions/workflows/rust.yml/badge.svg
[actions]: https://github.com/yasuo-ozu/sumtype/actions/workflows/rust.yml

In Rust, returning one of several different types from a function depending on its arguments is awkward: even when the types share the same interface, they remain distinct concrete types, so you cannot return them directly from a single function. The usual workaround is `Box<dyn Trait>`, but it has drawbacks — it allocates on the heap, it is not a zero-cost abstraction, and the boxed type often has to be `'static`.

For example:

```rust
// Example with Iterator trait
fn conditional_iterator(flag: bool) -> Box<dyn Iterator<Item = i32>> {
    if flag {
        Box::new(0..10) as Box<dyn Iterator<Item = i32>>
    } else {
        Box::new(vec![1, 2, 3].into_iter()) as Box<dyn Iterator<Item = i32>>
    }
}

// Example with Read trait  
fn conditional_reader(use_file: bool) -> Box<dyn std::io::Read> {
    if use_file {
        Box::new(std::fs::File::open("data.txt").unwrap()) as Box<dyn std::io::Read>
    } else {
        Box::new(std::io::Cursor::new(b"hello world")) as Box<dyn std::io::Read>
    }
}
```

Each function returns one of two different concrete types that implement the same trait (`Iterator` or `Read`). `Box<dyn Trait>` lets them share a return type, but only at the cost of the drawbacks above.

This crate solves the problem by generating a single anonymous sum type for each context annotated with the `#[sumtype]` attribute. The `sumtype!(expr)` macro wraps an expression in that sum type, so the otherwise-distinct types all become one type within the same `#[sumtype]` context — which is what lets a single function return any of them. The sum type is a plain enum, so it needs no heap allocation, and it remains a zero-cost abstraction: when the branch is known at compile time, the compiler can collapse the enum away and leave only the concrete type.

The same functions written with `sumtype`:

```rust
use sumtype::sumtype;

// Iterator example
#[sumtype(::announce_std::iter::Iterator)]
fn conditional_iterator(flag: bool) -> impl Iterator<Item = i32> {
    if flag {
        sumtype!(0..10) // Wraps the range iterator
    } else {
        sumtype!(vec![1, 2, 3].into_iter()) // Wraps the vector iterator
    }
}

// Read example
#[sumtype(::announce_std::io::Read)]
fn conditional_reader(use_cursor: bool) -> impl std::io::Read {
    if use_cursor {
        sumtype!(std::io::Cursor::new(b"hello world"))
    } else {
        sumtype!(std::io::Cursor::new(vec![1, 2, 3, 4, 5]))
    }
}
```

The traits of `std` are named through [`announce-std`](https://docs.rs/announce-std), which you
add to your `Cargo.toml` next to `sumtype`:

```toml
[dependencies]
sumtype = "0.5"
announce-std = "0.2"
```

Here `#[sumtype]` generates a sum type that can hold any of the concrete types implementing the specified trait, and `sumtype!` wraps each expression so they share that type within the function. Because the sum type is a plain enum, it avoids heap allocation, and when the branch is known at compile time the compiler optimizes the abstraction away.

## Zero-Cost Abstraction vs `Box<dyn Trait>`

A key advantage of `sumtype` over `Box<dyn Trait>` is its **zero-cost abstraction**.

**Benefits of `sumtype`:**
1. **No heap allocation** - Uses stack-allocated enum variants
2. **Static dispatch** - Direct method calls, no vtable lookup
3. **Compile-time optimization** - When conditions are known statically, the compiler can eliminate the enum entirely
4. **Zero runtime cost** - In the best case, compiles down to the concrete type with no abstraction overhead

When the branch is known at compile time, the `sumtype` version can be far faster than a boxed trait object, because it compiles down to direct use of the concrete type with no abstraction overhead.

## Difference from `std::any::Any`

`std::any::Any` can also hold a value that is one of many concrete types, but it solves a different problem. `Any` is about *type erasure and recovery*: you store a value as `Box<dyn Any>` (or `&dyn Any`) and later try to recover its original type with `downcast_ref::<T>()`. `sumtype` is about *unifying a fixed set of types behind a shared trait*, so that you keep calling that trait's methods directly.

|                              | `sumtype`                                  | `dyn Any`                                                   |
| ---------------------------- | ------------------------------------------ | ----------------------------------------------------------- |
| Set of types                 | closed — known per `#[sumtype]` context    | open — any `'static` type                                   |
| Calling the shared trait     | direct; the sum type implements it         | not possible — you must `downcast` to a concrete type first |
| Recovering the concrete type | secondary — via [`Downcast`](#recovering-a-concrete-type) when `'static` | `downcast`, which can fail at runtime |
| Allocation                   | none (stack-allocated enum)                | usually `Box` (heap)                                        |
| Dispatch                     | static, and often optimized away entirely  | runtime `TypeId` comparison                                 |
| `'static` requirement        | only for `Downcast`; not for the core type | required (`Any: 'static`)                                   |

In short, reach for `dyn Any` when you genuinely need runtime reflection — to stash values of an unknown type and recover them later. Reach for `sumtype` when the set of types is known and you simply want them to share a trait, without the cost (or the `'static` requirement) of `Box<dyn Trait>` or `dyn Any`.

## Recovering a concrete type

When you *do* need to recover the original type — `dyn Any`'s use case — `sumtype` offers the
[`Downcast`](https://docs.rs/sumtype/latest/sumtype/trait.Downcast.html) trait, implemented
automatically for every generated sum type. Name the target type(s) in the return bound
(`+ Downcast<To>`), and it works with the ordinary type-less `sumtype!(expr)` form:

```rust
use sumtype::{sumtype, Downcast};

#[sumtype(::announce_std::fmt::Debug)]
fn make(b: bool) -> impl std::fmt::Debug + Downcast<u32> + Downcast<String> {
    if b { sumtype!(1u32) } else { sumtype!(String::from("hi")) }
}

let v = make(true);
assert_eq!(Downcast::<u32>::downcast_ref(&v), Some(&1u32)); // the active variant
assert_eq!(Downcast::<String>::downcast_ref(&v), None);     // wrong type

// Move the value out, or get the sum type back unchanged on a miss:
assert_eq!(Downcast::<u32>::downcast(make(true)).ok(), Some(1u32));
```

Because downcasting checks the wrapped type at runtime (via `TypeId`), both the target and the
wrapped types must be `'static` (so it is unavailable for sum types that wrap a borrowed value).
`downcast_ref`/`downcast_mut` are allocation-free; the owning `downcast` boxes the value briefly to
move it out. `To` is a trait parameter, so it is chosen by a type annotation or a
`Downcast::<To>::…` call (not a method turbofish), which also lets `Downcast<To>` be used as a
generic bound: `fn f<E: Downcast<u32>>(e: E) { … }`. (Exposing the named sum type — a `-> sumtype!()`
return or a field — also works and makes every target type available at once.)

## Using `#[sumtype]` in other contexts

`#[sumtype]` is not limited to functions. Applied to an expression block, for instance, it lets you initialize one of several trait-implementing types based on a condition and bind the result to a variable:

```ignore
# use sumtype::sumtype;
# let some_condition = true;
#[sumtype(::announce_std::iter::Iterator)]
let mut iter =  {
    if some_condition {
        sumtype!((0..5)) // Wraps the range iterator
    } else {
        sumtype!(vec![10, 20, 30].into_iter()) // Wraps the vector iterator
    }
};

// Now `iter` can be used as a unified iterator type in the rest of the code
for value in iter {
    println!("{}", value);
}
```

Here `#[sumtype]` is applied to an expression block: each branch is wrapped with `sumtype!` and bound to `iter`, which can then be used uniformly in the rest of the code. Note that this form requires nightly Rust and `#![feature(proc_macro_hygiene)]` — see the [tracking issue for procedural macros and "hygiene 2.0"](https://github.com/rust-lang/rust/issues/54727).

`#[sumtype]` can also be applied when defining a trait and when implementing one. Here is an example of each.

Using `#[sumtype]` with a trait definition:

```
# use sumtype::sumtype;
#[sumtype(::announce_std::iter::Iterator)]
trait MyTrait {
    fn get_iterator(&self, flag: bool) -> impl Iterator<Item = i32> {
        if flag {
            sumtype!((0..5)) // Wraps the range iterator
        } else {
            sumtype!(vec![10, 20, 30].into_iter()) // Wraps the vector iterator
        }
    }
}
```

Here `#[sumtype]` is applied to a trait definition — useful when a trait's default method body needs to return a sum type.

Using `#[sumtype]` on a trait and on an implementation of it for a concrete type:

```
# use sumtype::sumtype;
#[sumtype(::announce_std::iter::Iterator)]
trait MyTrait {
    fn get_iterator(&self, flag: bool) -> impl Iterator<Item = i32> {
        if flag {
            sumtype!((0..5)) // Wraps the range iterator
        } else {
            sumtype!(vec![10, 20, 30].into_iter()) // Wraps the vector iterator
        }
    }
}
struct StructA;

#[sumtype(::announce_std::iter::Iterator)]
impl MyTrait for StructA {
    fn get_iterator(&self, _flag: bool) -> impl Iterator<Item = i32> {
        sumtype!((0..5)) // Wraps a range iterator
    }
}
```

Here, `StructA` implements `MyTrait`, wrapping a range iterator with `sumtype!`.

Using `#[sumtype]` with a module definition:

```ignore
# use sumtype::sumtype;
#[sumtype(::announce_std::iter::Iterator)]
mod my_module {
    pub struct MyStruct {
        iter: sumtype!(),
    }

    impl MyStruct {
        pub fn new(flag: bool) -> Self {
            let iter = if flag {
                sumtype!(0..5, std::ops::Range<u32>) // Wraps a range iterator
            } else {
                sumtype!(vec![10, 20, 30].into_iter(), std::vec::IntoIter<u32>) // Wraps a vector iterator
            };
            MyStruct { iter }
        }

        pub fn iterate(self) {
            for value in self.iter {
                println!("{}", value);
            }
        }
    }
}
```

## Supported Traits

`#[sumtype(Trait)]` reads the definition of `Trait` from its [announce](https://docs.rs/announce)
trampoline. The traits of `std` are announced by [`announce-std`](https://docs.rs/announce-std), so
you name them by their `announce_std` path (in the type namespace, `announce_std::iter::Iterator` is
the real `std::iter::Iterator`):

- **`::announce_std::iter::Iterator`** - For types implementing [`std::iter::Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html)
- **`::announce_std::io::Read`** - For types implementing [`std::io::Read`](https://doc.rust-lang.org/std/io/trait.Read.html)
- **`::announce_std::clone::Clone`** - For types implementing [`std::clone::Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html)
- **`::announce_std::marker::Copy`** - For types implementing [`std::marker::Copy`](https://doc.rust-lang.org/std/marker/trait.Copy.html)
- **`::announce_std::hash::Hash`** - For types implementing [`std::hash::Hash`](https://doc.rust-lang.org/std/hash/trait.Hash.html)
- **`::announce_std::fmt::Debug`** - For types implementing [`std::fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html)
- **`::announce_std::fmt::Display`** - For types implementing [`std::fmt::Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html)
- **`::announce_std::error::Error`** - For types implementing [`std::error::Error`](https://doc.rust-lang.org/std/error/trait.Error.html)

Other traits of `announce-std` work the same way, as long as the generated sum type can implement
them (see [Custom Traits](#custom-traits)). Several traits are joined with `+`:
`#[sumtype(::announce_std::iter::Iterator + ::announce_std::clone::Clone)]`.

The module `sumtype::traits` is gone: see [Changes in 0.5](#changes-in-05).

### More Examples

#### Working with different Read implementations

```rust
use sumtype::sumtype;

#[sumtype(::announce_std::io::Read)]
fn get_reader(source: &str) -> impl std::io::Read {
    match source {
        "memory" => sumtype!(std::io::Cursor::new(b"Hello from memory")),
        "empty" => sumtype!(std::io::empty()),
        _ => sumtype!(std::io::Cursor::new(vec![0u8; 1024])),
    }
}
```

#### Working with cloneable types

```rust
use sumtype::sumtype;

#[sumtype(::announce_std::clone::Clone)]
fn get_cloneable(use_string: bool) -> impl Clone {
    if use_string {
        sumtype!(String::from("Hello"))
    } else {
        sumtype!(vec![1, 2, 3])
    }
}
```

#### Working with Debug and Display traits

```rust
use sumtype::sumtype;

#[sumtype(::announce_std::fmt::Debug)]
fn get_debuggable(use_int: bool) -> impl std::fmt::Debug {
    if use_int {
        sumtype!(42i32)
    } else {
        sumtype!(String::from("hello"))
    }
}

#[sumtype(::announce_std::fmt::Display)]
fn get_displayable(use_int: bool) -> impl std::fmt::Display {
    if use_int {
        sumtype!(42i32)
    } else {
        sumtype!("Static string")
    }
}

// Usage
let debug_item = get_debuggable(true);
println!("Debug: {:?}", debug_item); // "Debug: 42"

let display_item = get_displayable(false);
println!("Display: {}", display_item); // "Display: Static string"
```

#### Working with Error types

```rust
use sumtype::sumtype;
use std::fmt;

// Define custom error types
#[derive(Debug)]
struct IoError(String);

impl fmt::Display for IoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IO Error: {}", self.0)
    }
}

impl std::error::Error for IoError {}

#[derive(Debug)]
struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse Error: {}", self.0)
    }
}

impl std::error::Error for ParseError {}

#[sumtype(::announce_std::error::Error)]
fn get_error(is_io_error: bool) -> impl std::error::Error {
    if is_io_error {
        sumtype!(IoError("Failed to read file".to_string()))
    } else {
        sumtype!(ParseError("Invalid format".to_string()))
    }
}

// Usage
let error = get_error(true);
println!("Error: {}", error); // "Error: IO Error: Failed to read file"
println!("Debug: {:?}", error); // Prints debug representation

// Can be used where std::error::Error is expected
fn handle_error(e: impl std::error::Error) {
    println!("Handling error: {}", e);
}

handle_error(get_error(false));
```


## Custom Traits

A trait is made usable with `#[sumtype]` by announcing it with
[`#[announce]`](https://docs.rs/announce): `sumtype` re-exports the `announce` crate as
`sumtype::announce`, so a crate that depends only on `sumtype` writes
`#[sumtype::announce::announce(crate = ::sumtype::announce)]` (the `crate = ..` option names the
re-export; it can be left out when your crate depends on `announce` itself). List `sumtype::check`
among the options to have the trait validated at its definition instead of at each `#[sumtype]`:

```rust
use sumtype::{announce::announce, sumtype};

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait MyCustomTrait {
    fn process(&self) -> ::std::string::String;
}

// Implement the trait for different types
struct TypeA;
impl MyCustomTrait for TypeA {
    fn process(&self) -> String {
        "Processing with TypeA".to_string()
    }
}

struct TypeB;
impl MyCustomTrait for TypeB {
    fn process(&self) -> String {
        "Processing with TypeB".to_string()
    }
}

// Use sumtype with your custom trait
#[sumtype(MyCustomTrait)]
fn get_processor(use_a: bool) -> impl MyCustomTrait {
    if use_a {
        sumtype!(TypeA)
    } else {
        sumtype!(TypeB)
    }
}
```

The trait can come from another crate: `#[sumtype(other_crate::MyCustomTrait)]`.

### What the trait may contain

`sumtype::check` rejects, at the item that causes it, what `#[sumtype]` cannot dispatch (and
`#[sumtype]` rejects it too when the trait was not checked):

- items other than methods and associated types (no associated constants, no associated type
  defaults, no generic associated types);
- a method without a receiver (`self`, `&self`, `&mut self`) as its first parameter;
- a `Self` that cannot be dispatched: a parameter other than the receiver whose type contains
  `Self`, or a return type that contains `Self` other than `Self` itself;
- a supertrait that is generic (`Foo<T>`), is `Eq` or `PartialEq`, or is not a trait bound.

A supertrait is either a built-in trait that `sumtype` recognizes (`Copy`, `Clone`, `Hash`,
`Debug`, `Display`, `Error`, `Iterator`, `Read`, written as a `::std`/`::core` path) or another
announced trait.

### Paths in an announced trait

`#[announce]` passes the trait to other crates, so announce requires every path in it to mean the
same thing there. In practice:

- Trait paths are absolute: `trait Sub: crate::Base`, `fn f<W: ::core::fmt::Write>(..)`. A bare
  `Base` or `Write` is an error.
- Non-primitive types are absolute too (`::std::string::String`, `crate::Local`), or the trait is
  announced with `allow_std_prelude = true`, which lets the prelude names `String`, `Vec<u8>`,
  `Option<T>`, `Clone`, `IntoIterator`, ... stay bare, or with a
  [`repeater`](https://docs.rs/announce/latest/announce/attr.announce.html#type-context), which
  carries the relative types of the defining crate to the user.
- A library that wants the old behavior for a trait announces a copy of it, wrapped in an inline
  module of the same name with `emit_item = false`. Only the trampoline is emitted, and the paths
  in the copy resolve where `#[sumtype]` is used, so they must be in scope there:

```rust
use sumtype::{announce::announce, sumtype};
use std::fmt::Write;

pub trait Greet {
    fn greet<W: Write>(&self, w: &mut W);
}
// The copy: `Greet!` is the trampoline, and `Write` resolves at the `#[sumtype]`.
#[announce(crate = ::sumtype::announce, emit_item = false)]
pub mod Greet {
    pub trait Greet {
        fn greet<W: Write>(&self, w: &mut W);
    }
}
```

### Implementing a `std` trait with a local mirror

`#[announce(mock = path)]` on a trait that mirrors a real trait makes `#[sumtype]` implement the
real trait at `path`. Only the required methods
of the mirror are forwarded; the provided methods of the real trait are not. `announce-std` is a
collection of such mirrors for `std`.

```rust
use sumtype::{announce::announce, sumtype};

#[announce(crate = ::sumtype::announce, mock = ::core::fmt::Display)]
pub trait MyDisplay {
    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result;
}

#[sumtype(MyDisplay)]
fn show(a: bool) -> impl std::fmt::Display {
    if a { sumtype!(1u8) } else { sumtype!("one") }
}
assert_eq!(show(false).to_string(), "one");
```

## The `announce` crate

`sumtype` re-exports [`announce`](https://docs.rs/announce) as `sumtype::announce`: the attribute
`#[sumtype::announce::announce]`, the helpers of `announce::helper` for writing your own callbacks
and the like are available without a dependency on `announce` itself. Pass
`crate = ::sumtype::announce` to the attribute in that case, as in the examples above.

See the [`sumtype::check` documentation](https://docs.rs/sumtype/latest/sumtype/macro.check.html)
for the full requirements and more advanced patterns for building custom sumtype-compatible traits.

## Changes in 0.5

`sumtype` now passes trait definitions with [announce](https://docs.rs/announce) trampolines. This
is a breaking change for the users of 0.4:

- **`sumtype::traits` is removed.** Name the `std` traits through `announce-std` (add
  `announce-std = "0.2"` to your `Cargo.toml`): `#[sumtype(::announce_std::iter::Iterator)]` instead
  of `#[sumtype(sumtype::traits::Iterator)]`, and likewise `::announce_std::io::Read`,
  `::announce_std::fmt::Debug`, `::announce_std::fmt::Display`, `::announce_std::error::Error`,
  `::announce_std::hash::Hash`, `::announce_std::clone::Clone`, `::announce_std::marker::Copy`.
- **`#[sumtrait]` is removed.** Announce the trait with `#[sumtype::announce::announce(crate =
  ::sumtype::announce)]` (or `#[announce]` when your crate depends on `announce`). The options map
  as follows:

  | `#[sumtrait(..)]` | now |
  | --- | --- |
  | `marker = Path` | dropped (it was already unused) |
  | `krate = path` | dropped (`#[sumtype]` finds `sumtype` itself); announce's `crate = path` names the announce runtime |
  | `implement = path` | `#[announce(mock = path)]` |
  | validation at the definition | `#[announce(sumtype::check)]` (new), and `#[sumtype]` validates the traits that were not checked |
  | paths resolved where `#[sumtype]` is used | announce requires paths that resolve everywhere: absolute trait paths, `allow_std_prelude = true` or a `repeater` for types, or the module copy shown in [Custom Traits](#custom-traits) |

- **New:** `sumtype::check` (validate a trait at its definition) and `sumtype::announce` (the
  re-exported `announce` crate).
