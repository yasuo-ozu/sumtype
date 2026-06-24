# Compile-fail expectations

This file is an executable test artifact: every block below is a ` ```compile_fail ` doctest that
**must fail to compile**. It is wired into the crate via a `#[cfg(doctest)]` item in `lib.rs`, so
each block runs under `cargo test` (as part of *Doc-tests sumtype*) but is invisible to `cargo doc`
and to normal builds.

The cases fall into two groups:

- **Intentional rejections** — unsupported usage that the macros now reject with a *clear* error
  (these guard against regressing the diagnostic back into a confusing one or a panic).
- **Known limitations** — usage that still produces an opaque error and is not yet supported.

(`compile_fail` only checks that compilation fails, not the message — which is what we want here,
because several diagnostics embed the per-compile-random `__Sumtype_Enum_<random>` name and so
cannot be pinned with a `trybuild` `.stderr`. The exact message observed on rustc 1.90 is quoted
in prose.)

---

## Intentional rejections

### An `Eq` supertrait on a `#[sumtrait]` trait is rejected

As above, via the `Eq` arm: `error: `Eq` is not supported as a `#[sumtrait]` supertrait …`
(previously the nonexistent `sumtype::traits::Eq`).

```compile_fail
use sumtype::{sumtrait, sumtype};
struct Marker;
#[sumtrait(marker = Marker)]
trait MyEq: Eq {}
impl MyEq for u8 {}
impl MyEq for u16 {}
#[sumtype(MyEq)]
fn f(b: bool) -> impl MyEq {
    if b { sumtype!(1u8) } else { sumtype!(1u16) }
}
```

### A `PartialEq` supertrait on a `#[sumtrait]` trait is rejected

`PartialEq` needs cross-variant logic that the dispatch mechanism cannot provide, so it is rejected
with a clear error (previously it was silently dropped, leaving the generated enum without a
`PartialEq` impl — `E0277` at the use site).

```compile_fail
use sumtype::{sumtrait, sumtype};
struct Marker;
#[sumtrait(marker = Marker)]
trait MyComparable: PartialEq {}
impl MyComparable for u8 {}
impl MyComparable for u16 {}
#[sumtype(MyComparable)]
fn f(b: bool) -> impl MyComparable {
    if b { sumtype!(1u8) } else { sumtype!(1u16) }
}
```

### `#[sumtype()]` with no trait bounds is rejected

`error: `#[sumtype]` requires at least one trait bound …` (previously the enum implemented nothing
and the user saw `E0277: __Sumtype_Enum_<random> is not an iterator`, leaking the internal name).

```compile_fail
use sumtype::sumtype;
#[sumtype()]
fn f(b: bool) -> impl Iterator<Item = i32> {
    if b { sumtype!(0..1) } else { sumtype!(std::iter::once(2)) }
}
```

### `#[sumtrait]` without a `marker` is rejected cleanly (no panic)

`error: invalid `#[sumtrait]` arguments: Missing field `marker` …` (previously this *panicked*:
`custom attribute panicked … called Result::unwrap() on an Err value`).

```compile_fail
use sumtype::sumtrait;
#[sumtrait]
trait Foo {
    fn g(&self) -> u32;
}
```

---

## Known limitations (not yet supported)

### A supertrait that is neither a built-in nor a `#[sumtrait]` trait is invoked as a macro

Supertraits must be either a recognized built-in (`Copy`, `Clone`, `Hash`, `Debug`, `Display`,
`Error`, `Iterator`, `Read` — bare or fully qualified) or another `#[sumtrait]` trait. A plain
user trait is emitted as a macro invocation and fails with `error: cannot find macro Plain …`. (A
clearer diagnostic would require resolving whether the path is a `#[sumtrait]` at expansion time,
which is not currently possible.)

```compile_fail
use sumtype::{sumtrait, sumtype};
struct Marker;
trait Plain {}
impl Plain for u8 {}
impl Plain for u16 {}
#[sumtrait(marker = Marker)]
trait MyTrait: Plain {}
impl MyTrait for u8 {}
impl MyTrait for u16 {}
#[sumtype(MyTrait)]
fn f(b: bool) -> impl MyTrait {
    if b { sumtype!(1u8) } else { sumtype!(1u16) }
}
```

### A nested `sumtype!(sumtype!(..))` leaves the inner macro unexpanded

The visitor replaces the outer `sumtype!` without recursing into the replacement, so the inner
`sumtype!` survives and fails with `error: cannot find macro sumtype in this scope`. Nesting is
meaningless (you cannot wrap a wrapped value), so it is left as an error.

```compile_fail
use sumtype::sumtype;
#[sumtype(sumtype::traits::Iterator)]
fn f() -> impl Iterator<Item = i32> {
    sumtype!(sumtype!(std::iter::once(1)))
}
```

### A sumtrait method generic that shares a name with the enclosing context's generic (E0403)

The dispatch impl is `impl<..enum generics..> Trait for Enum { fn m<..method generics..>(..) {..} }`.
If the enclosing `#[sumtype]` item is generic over `T` (so the enum is too) **and** a trait method
also declares a generic `T`, the method re-declares a name already bound by the impl:
`error[E0403]: the name `T` is already used`. (This is also illegal in hand-written Rust; sumtype
does not alpha-rename to avoid it. Renaming either generic resolves it.)

```compile_fail
use sumtype::{sumtrait, sumtype};
struct Marker;
#[sumtrait(marker = Marker)]
trait Foo {
    fn foo<T: std::fmt::Display>(&self, t: T) -> String;
}
struct A;
struct B<T>(T);
impl Foo for A {
    fn foo<T: std::fmt::Display>(&self, t: T) -> String { format!("A:{t}") }
}
impl<U: std::fmt::Display> Foo for B<U> {
    fn foo<T: std::fmt::Display>(&self, t: T) -> String { format!("B:{}:{t}", self.0) }
}
// `make`'s generic `T` collides with `Foo::foo`'s generic `T`:
#[sumtype(Foo)]
fn make<T: std::fmt::Display + Copy>(b: bool, v: T) -> impl Foo {
    if b { sumtype!(A) } else { sumtype!(B(v)) }
}
```

### A sumtrait method that returns `impl Trait` (RPIT) cannot be dispatched (E0308)

The dispatch body is a `match self { A(x) => <A>::m(x), C(x) => <C>::m(x) }`. When the method returns
`impl Trait`, each arm yields a *distinct* opaque type, so the match arms are incompatible
(`error[E0308]: match arms have incompatible types`). Unifying them would need the TypeRef/leaked-type
machinery used for associated types; it is not currently supported.

```compile_fail
use sumtype::{sumtrait, sumtype};
struct Marker;
#[sumtrait(marker = Marker)]
trait Foo {
    fn it(&self) -> impl Iterator<Item = u32>;
}
struct A;
struct C;
impl Foo for A {
    fn it(&self) -> impl Iterator<Item = u32> { std::iter::once(1) }
}
impl Foo for C {
    fn it(&self) -> impl Iterator<Item = u32> { std::iter::repeat(2).take(2) }
}
#[sumtype(Foo)]
fn make(b: bool) -> impl Foo {
    if b { sumtype!(A) } else { sumtype!(C) }
}
```

### A const generic cannot be used in an explicit type-position `sumtype![..]`

A bare const-param ident in `sumtype![..]` parses as a *type* argument, while the matching
`for<.. const N ..>` expr generic is a *const* argument; the kind-compatibility check then aborts
with "The generic arguments are incompatible with generic params in expression." (The `impl Trait`
return form with const generics works — see `tests/fixed_bugs.rs`; only the explicit type-macro form
is affected.)

```compile_fail
use sumtype::sumtype;
#[sumtype(sumtype::traits::Iterator)]
fn ca<'a, const N: usize>(x: &'a u32, b: bool) -> sumtype!['a, N] {
    if b {
        sumtype!(std::iter::repeat(*x).take(N), for<'a, const N: usize> std::iter::Take<std::iter::Repeat<u32>>)
    } else {
        sumtype!(std::iter::empty(), for<'a, const N: usize> std::iter::Empty<u32>)
    }
}
```
