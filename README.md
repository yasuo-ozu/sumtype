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
#[sumtype(sumtype::traits::Iterator)]
fn conditional_iterator(flag: bool) -> impl Iterator<Item = i32> {
    if flag {
        sumtype!(0..10) // Wraps the range iterator
    } else {
        sumtype!(vec![1, 2, 3].into_iter()) // Wraps the vector iterator
    }
}

// Read example
#[sumtype(sumtype::traits::Read)]
fn conditional_reader(use_cursor: bool) -> impl std::io::Read {
    if use_cursor {
        sumtype!(std::io::Cursor::new(b"hello world"))
    } else {
        sumtype!(std::io::Cursor::new(vec![1, 2, 3, 4, 5]))
    }
}
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

#[sumtype(sumtype::traits::Debug)]
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
#[sumtype(sumtype::traits::Iterator)]
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
#[sumtype(sumtype::traits::Iterator)]
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
#[sumtype(sumtype::traits::Iterator)]
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

#[sumtype(sumtype::traits::Iterator)]
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
#[sumtype(sumtype::traits::Iterator)]
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

The `sumtype` crate provides built-in support for several common traits:

- **[`sumtype::traits::Iterator`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Iterator.html)** - For types implementing [`std::iter::Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html)
- **[`sumtype::traits::Read`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Read.html)** - For types implementing [`std::io::Read`](https://doc.rust-lang.org/std/io/trait.Read.html)  
- **[`sumtype::traits::Clone`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Clone.html)** - For types implementing [`std::clone::Clone`](https://doc.rust-lang.org/std/clone/trait.Clone.html)
- **[`sumtype::traits::Copy`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Copy.html)** - For types implementing [`std::marker::Copy`](https://doc.rust-lang.org/std/marker/trait.Copy.html)
- **[`sumtype::traits::Hash`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Hash.html)** - For types implementing [`std::hash::Hash`](https://doc.rust-lang.org/std/hash/trait.Hash.html)
- **[`sumtype::traits::Debug`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Debug.html)** - For types implementing [`std::fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html)
- **[`sumtype::traits::Display`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Display.html)** - For types implementing [`std::fmt::Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html)
- **[`sumtype::traits::Error`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Error.html)** - For types implementing [`std::error::Error`](https://doc.rust-lang.org/std/error/trait.Error.html)

### More Examples

#### Working with different Read implementations

```rust
use sumtype::sumtype;

#[sumtype(sumtype::traits::Read)]
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

#[sumtype(sumtype::traits::Clone)]
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

#[sumtype(sumtype::traits::Debug)]
fn get_debuggable(use_int: bool) -> impl std::fmt::Debug {
    if use_int {
        sumtype!(42i32)
    } else {
        sumtype!(String::from("hello"))
    }
}

#[sumtype(sumtype::traits::Display)]
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

#[sumtype(sumtype::traits::Error)]
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

You can make your own traits work with `sumtype` using the [`#[sumtrait]`](https://docs.rs/sumtype/latest/sumtype/attr.sumtrait.html) attribute, which extends support to any trait that satisfies the sumtrait-safety requirements.

```rust
use sumtype::{sumtype, sumtrait};

// Define a marker type (required for sumtrait)
pub struct MyTraitMarker(std::convert::Infallible);

// Define your custom trait
#[sumtrait(marker = MyTraitMarker)]
pub trait MyCustomTrait {
    fn process(&self) -> String;
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

See the [`#[sumtrait]` documentation](https://docs.rs/sumtype/latest/sumtype/attr.sumtrait.html) for the full sumtrait-safety requirements and more advanced patterns for building custom sumtype-compatible traits.
