#![doc = include_str!("README.md")]

// Audit: cases that must fail to compile (intentional rejections + known limitations) are
// reproduced as `compile_fail` doctests in `tests/compile_fail.md`. Gated on `cfg(doctest)` so
// this neither appears in `cargo doc` output nor affects normal builds; it runs under
// `cargo test` (Doc-tests sumtype).
#[cfg(doctest)]
#[doc = include_str!("tests/compile_fail.md")]
struct CompileFailCases;

#[doc(hidden)]
pub use sumtype_macro::_sumtype_internal;

/// Re-export of the [`announce`](https://docs.rs/announce) crate, so that a crate can announce its
/// traits for `#[sumtype]` without depending on `announce` itself:
/// `#[sumtype::announce::announce(crate = ::sumtype::announce, sumtype::check)]`. The option
/// `crate = ..` tells `#[announce]` where its runtime is; it is optional for a crate that depends on
/// `announce` directly. See [`check`].
pub use announce;

#[doc(hidden)]
pub mod __private {
    // The announced `std` traits that `#[sumtype]` maps built-in supertraits (`trait Foo: Debug`)
    // to, reachable without the user depending on `announce-std`.
    pub use ::announce_std;
}

/// Checks a trait at its definition: it can be used with `#[sumtype]`.
///
/// A trait is made usable with `#[sumtype]` by announcing it with the
/// [`announce`](https://docs.rs/announce) attribute (re-exported as [`announce`]). `#[sumtype]`
/// then reads the trait through its trampoline, so nothing else is needed. Without this check
/// the trait is validated only when a `#[sumtype]` receives it, and the error appears there.
/// List `sumtype::check` among the options of `#[announce]` to get the error at the trait:
///
/// ```
/// use sumtype::{announce::announce, sumtype};
///
/// // `crate = ..` is needed when the crate does not depend on `announce` itself.
/// #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
/// pub trait Greet {
///     fn greet(&self) -> String;
/// }
///
/// struct English;
/// struct French;
/// impl Greet for English {
///     fn greet(&self) -> String { "hello".into() }
/// }
/// impl Greet for French {
///     fn greet(&self) -> String { "bonjour".into() }
/// }
///
/// #[sumtype(Greet)]
/// fn pick(english: bool) -> impl Greet {
///     if english { sumtype!(English) } else { sumtype!(French) }
/// }
/// assert_eq!(pick(false).greet(), "bonjour");
/// ```
///
/// A trait that does not satisfy the rules is rejected at its definition:
///
/// ```compile_fail
/// use sumtype::announce::announce;
/// #[announce(crate = ::sumtype::announce, sumtype::check)]
/// pub trait Bad {
///     fn new() -> u8; // no receiver
/// }
/// ```
///
/// ## What `#[sumtype]` supports in a trait
///
/// The trait is *sumtype-safe* when it satisfies all of the following (this is similar to object
/// safety, but not identical). `sumtype::check` reports a violation at the item that causes it,
/// and `#[sumtype]` reports it too when the trait was not checked:
///
/// - All trait items are either associated types or associated functions. Associated constants,
///   associated type defaults and generic associated types are not supported.
/// - All associated functions take a receiver (`self`, `&self`, or `&mut self`) as their first
///   parameter. Additional parameters are allowed, but their types must not contain `Self`.
/// - All associated functions return `Self`, or a type that does not contain `Self`.
/// - Each supertrait is either a built-in trait that `sumtype` recognizes (`Copy`, `Clone`,
///   `Hash`, `Debug`, `Display`, `Error`, `Iterator`, `Read`, written as a `std`/`core` path or,
///   for `Copy` and `Clone`, bare) or another announced trait, written as an absolute path. A
///   supertrait cannot have generic arguments, and `Eq`/`PartialEq` are rejected.
///
/// ## Paths in an announced trait
///
/// `#[announce]` hands the trait to other crates, so it requires every path in it to mean the same
/// there. This is the rule of announce, not of `sumtype`; the consequences for a trait are:
///
/// - Traits (supertraits, bounds of generics) are written with absolute paths: `crate::Base`,
///   `::core::fmt::Debug`. `trait Sub: Base` and `fn f<W: Write>(..)` are errors.
/// - Types that are not primitive are absolute (`::std::string::String`, `crate::Local`) too,
///   unless `allow_std_prelude = true` is given (then `String`, `Vec<u8>`, `Option<T>`, `Clone`
///   and the like are fine) or the trait is announced with a `repeater` (which carries relative
///   types to the other crate).
///
/// ## Making a std (or other third-party) trait usable
///
/// `#[announce(mock = ::core::fmt::Display)]` on a local trait that mirrors the real one makes
/// `#[sumtype]` implement the real trait. The mirror lists the real trait's required methods;
/// provided methods are not forwarded. [`announce-std`](https://docs.rs/announce-std) provides
/// these mocks for the traits of `std`.
pub use sumtype_macro::check;

/// Enables the `sumtype!(..)` macro within the annotated context.
///
/// The bounds are announced traits, joined with `+`: the traits of `std` through `announce-std`
/// (`::announce_std::iter::Iterator`, ...) and your own traits, announced with `#[announce]` (see
/// [`check`]).
///
/// For each context marked with `#[sumtype]`, the macro creates a single anonymous sum type that
/// implements the trait(s) you specify. Wrap an expression with `sumtype!(expr)` to store it in
/// that sum type. For example, to return a unified iterator:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// #[sumtype(::announce_std::iter::Iterator)]
/// fn return_iter(a: bool) -> impl Iterator<Item = ()> {
///     if a {
///         sumtype!(std::iter::once(()))
///     } else {
///         sumtype!(vec![()].into_iter())
///     }
/// }
/// ```
///
/// Depending on `a`, this returns a [`std::iter::Once`] or a [`std::vec::IntoIter`]. `#[sumtype]`
/// creates an anonymous sum type that also implements [`std::iter::Iterator`] and wraps each
/// `sumtype!(..)` expression in it. The abstraction is zero-cost when `a` is known at compile time.
///
/// You can also name the sum type explicitly with `sumtype!()` in type position. When you do, give
/// each wrapped expression's concrete type using the `sumtype!(expr, Type)` form:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// #[sumtype(::announce_std::iter::Iterator)]
/// fn return_iter_explicit(a: bool) -> sumtype!() {
///     if a {
///         sumtype!(std::iter::once(()), std::iter::Once<()>)
///     } else {
///         sumtype!(vec![()].into_iter(), std::vec::IntoIter<()>)
///     }
/// }
/// ```
pub use sumtype_macro::sumtype;

/// Recovers a concrete type `To` from a `#[sumtype]` sum type.
///
/// An implementation is generated automatically for every sum type produced by `#[sumtype]`.
/// Because it inspects the active variant's type at runtime (via [`core::any::TypeId`]), both `To`
/// and every wrapped type must be `'static`; the methods are unavailable for sum types that wrap a
/// borrowed type. `downcast_ref`/`downcast_mut` are allocation-free; the owning `downcast` boxes the
/// value briefly to move it out.
///
/// To call `Downcast` on a function's result, name the target type(s) in the return bound:
/// `-> impl Trait + Downcast<To>` (add one `+ Downcast<T>` per type you want to recover). This works
/// with the ordinary type-less `sumtype!(expr)` form. A bare `impl Trait` return exposes only
/// `Trait`, so `Downcast` would not be reachable through it. (Exposing the **named** sum type — a
/// `-> sumtype!()` return or a field — also works and makes every `To` available at once.)
///
/// `To` is the trait's type parameter, so it is chosen by context (a type annotation or a
/// `Downcast::<To>::…` qualified call), not by a method turbofish — which is what lets `Downcast<To>`
/// serve as a generic bound, e.g. `fn f<E: Downcast<u32>>(e: E) -> Option<u32> { e.downcast().ok() }`.
///
/// ```
/// # use sumtype::{sumtype, Downcast};
/// #[sumtype(::announce_std::fmt::Debug)]
/// fn make(b: bool) -> impl std::fmt::Debug + Downcast<u32> + Downcast<String> {
///     if b { sumtype!(1u32) } else { sumtype!(String::from("hi")) }
/// }
/// let v = make(true);
/// assert_eq!(Downcast::<u32>::downcast_ref(&v), Some(&1u32));
/// assert_eq!(Downcast::<String>::downcast_ref(&v), None);
/// assert_eq!(Downcast::<String>::downcast(make(false)).ok(), Some(String::from("hi")));
/// ```
pub trait Downcast<To: 'static>: Sized {
    /// Returns the wrapped value if the active variant holds a `To`; otherwise returns `self`
    /// unchanged.
    fn downcast(self) -> Result<To, Self>;
    /// Returns a shared reference to the wrapped value if the active variant holds a `To`.
    fn downcast_ref(&self) -> Option<&To>;
    /// Returns a mutable reference to the wrapped value if the active variant holds a `To`.
    fn downcast_mut(&mut self) -> Option<&mut To>;
}
