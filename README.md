# sumtype crate [![Latest Version]][crates.io] [![Documentation]][docs.rs] [![GitHub Actions]][actions]

[Latest Version]: https://img.shields.io/crates/v/sumtype.svg
[crates.io]: https://crates.io/crates/sumtype
[Documentation]: https://img.shields.io/docsrs/sumtype
[docs.rs]: https://docs.rs/sumtype/latest/sumtype/
[GitHub Actions]: https://github.com/yasuo-ozu/sumtype/actions/workflows/rust.yml/badge.svg
[actions]: https://github.com/yasuo-ozu/sumtype/actions/workflows/rust.yml

In Rust, when a function needs to return different types that implement the same trait based on its arguments, it can be challenging because even if the types provide the same interface, they are considered to have different concrete types. This makes it impossible to simply use a return statement to return them directly. A common solution to this problem is to use `Box<dyn Trait>`, but this approach has two drawbacks: it incurs extra heap memory usage, and it does not provide zero-cost abstraction. Additionally, the types must often have a `'static` lifetime.

Here’s an example to illustrate this:

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

In these examples, the functions need to return different concrete types that implement the same trait (`Iterator` or `Read`). Using `Box<dyn Trait>` allows us to return them from the same function, but it introduces the aforementioned drawbacks.


This crate addresses these issues by generating a single anonymous sum type for contexts decorated with the `#[sumtype]` attribute. By using the `sumtype!([expr])` macro, you can wrap the given expression in this sum type. Since the wrapped types become the same type within the same `#[sumtype]` context, it enables scenarios like returning different types that implement the same trait from a single function. Internally, this sum type uses a simple enum, which means it does not consume additional heap memory. Furthermore, it provides zero-cost abstraction. For instance, in the previous examples, if the conditions can be determined statically by the compiler, the returned types can also be determined, potentially eliminating any additional abstraction cost introduced by the sum type.

Here's how it looks with `sumtype`:

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

In these examples, the `#[sumtype]` attribute generates a sum type that can wrap different concrete types implementing the specified trait. The `sumtype!` macro is used to wrap the expressions, allowing them to be treated as the same type within the function. Because the sum type uses a simple enum internally, it avoids additional heap allocations. If the conditions are known at compile time, the compiler can optimize the returned types without incurring extra abstraction costs.

## Zero-Cost Abstraction vs `Box<dyn Trait>`

One of the key advantages of `sumtype` is its **zero-cost abstraction** when compared to `Box<dyn Trait>`. 

**Benefits of `sumtype`:**
1. **No heap allocation** - Uses stack-allocated enum variants
2. **Static dispatch** - Direct method calls, no vtable lookup
3. **Compile-time optimization** - When conditions are known statically, the compiler can eliminate the enum entirely
4. **Zero runtime cost** - In the best case, compiles down to the concrete type with no abstraction overhead

When conditions are statically known at compile time, the `sumtype` version can be orders of magnitude faster because it compiles down to direct usage of the concrete type with zero abstraction overhead.

Additionally, sumtype can be used not only in functions but also in other contexts. For example, by using `#[sumtype]` in an expression block, you can initialize different types that implement the same trait based on certain conditions and assign them to a specific variable.

Here's an example to illustrate this:

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

In this example, the `#[sumtype]` attribute is applied to an expression block, allowing different types that implement the same trait to be initialized based on the condition. These are then wrapped using the `sumtype!` macro and assigned to the `iter` variable, making it possible to work with them uniformly throughout the code. Unfortunately, this feature requires nightly Rust and `#![feature(proc_macro_hygiene)]`, see [Tracking issue for procedural macros and "hygiene 2.0"](https://github.com/rust-lang/rust/issues/54727).

Additionally, `#[sumtype]` can be used when defining traits as well as when implementing them. Here are examples for each case:

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

In this example, `#[sumtype]` is applied to a trait definition. This could be useful for scenarios where you want to create sum types that can represent different implementations of a trait.

Using `#[sumtype]` with a trait implementation:

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

Here, `StructA` implements `MyTrait` and uses `sumtype!` to wrap a range iterator.

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
- **[`sumtype::traits::Debug`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Debug.html)** - For types implementing [`std::fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html)
- **[`sumtype::traits::Display`](https://docs.rs/sumtype/latest/sumtype/traits/trait.Display.html)** - For types implementing [`std::fmt::Display`](https://doc.rust-lang.org/std/fmt/trait.Display.html)

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


## Custom Traits

You can also define your own traits to work with `sumtype` using the [`#[sumtrait]`](https://docs.rs/sumtype/latest/sumtype/attr.sumtrait.html) attribute. This allows you to extend sumtype support to any trait that meets the sumtrait-safety requirements.

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

See the [`#[sumtrait]` documentation](https://docs.rs/sumtype/latest/sumtype/attr.sumtrait.html) for more details on sumtrait-safety requirements and advanced usage patterns for creating custom sumtype-compatible traits.
