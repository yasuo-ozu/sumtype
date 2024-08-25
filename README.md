# sumtype crate

In Rust, when a function needs to return different types of iterators based on its arguments, it can be challenging because even if the iterators return the same type of elements, they are considered to have different types. This makes it impossible to simply use a return statement to return them directly. A common solution to this problem is to use `Box<dyn Iterator>`, but this approach has two drawbacks: it incurs extra heap memory usage, and it does not provide zero-cost abstraction. Additionally, the elements returned by the iterator must have a 'static lifetime.

Here’s an example to illustrate this:

```
fn conditional_iterator(flag: bool) -> Box<dyn Iterator<Item = i32>> {
    if flag {
        Box::new(0..10) as Box<dyn Iterator<Item = i32>> // Returns an iterator over the range 0 to 10
    } else {
        Box::new(vec![1, 2, 3].into_iter()) as Box<dyn Iterator<Item = i32>> // Returns an iterator over a vector
    }
}
```
In this code, depending on the value of flag, the function returns either an iterator over a range `(0..10)` or an iterator over a vector (`vec![1, 2, 3]`). Both iterators yield `i32` elements, but their types are different (`std::ops::Range<i32>` and `std::vec::IntoIter<i32>` respectively). Using `Box<dyn Iterator<Item = i32>>` allows us to return them from the same function, but it introduces the aforementioned drawbacks.


This crate addresses the aforementioned issues by generating a single anonymous sum type for contexts decorated with the `#[sumtype]` attribute. By using the macro `sumtype!([expr])`, you can wrap the given expression in this sum type. Since the wrapped types become the same type within the same `#[sumtype]` context, it enables scenarios like returning different Iterators from a single function. Internally, this sum type uses a simple enum, which means it does not consume additional heap memory. Furthermore, it provides zero-cost abstraction. For instance, in the previous example, if the `flag` can be determined statically by the compiler, the returned iterator can also be determined, potentially eliminating any additional abstraction cost introduced by the sum type.

Here's how it might look:

```
use sumtype::sumtype;
#[sumtype]
fn conditional_iterator(flag: bool) -> impl Iterator<Item = i32> {
    if flag {
        sumtype!((0..10)) // Wraps the range iterator
    } else {
        sumtype!(vec![1, 2, 3].into_iter()) // Wraps the vector iterator
    }
}
```

In this example, the `#[sumtype]` attribute generates a sum type that can wrap both iterator types. The sumtype! macro is used to wrap the expressions, allowing them to be treated as the same type within the function. Because the sum type uses a simple enum internally, it avoids additional heap allocations. If the `flag` is known at compile time, the compiler can optimize the returned iterator without incurring extra abstraction costs.

Additionally, sumtype can be used not only in functions but also in other contexts. For example, by using #[sumtype] in an expression block within a mathematical formula, you can initialize different types of Iterators based on certain conditions and assign them to a specific variable.

Here's an example to illustrate this:

```ignore
# use sumtype::sumtype;
# let some_condition = true;
#[sumtype]
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

In this example, the #[sumtype] attribute is applied to an expression block, allowing different types of iterators to be initialized based on the condition. These are then wrapped using the sumtype! macro and assigned to the iter variable, making it possible to work with them uniformly throughout the code. Unfortunately, this feature requires nightly Rust and `#![feature(proc_macro_hygiene)]`, see [Tracking issue for procedural macros and "hygiene 2.0"](https://github.com/rust-lang/rust/issues/54727).
