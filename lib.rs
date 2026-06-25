#![doc = include_str!("README.md")]

// Audit: cases that must fail to compile (intentional rejections + known limitations) are
// reproduced as `compile_fail` doctests in `tests/compile_fail.md`. Gated on `cfg(doctest)` so
// this neither appears in `cargo doc` output nor affects normal builds; it runs under
// `cargo test` (Doc-tests sumtype).
#[cfg(doctest)]
#[doc = include_str!("tests/compile_fail.md")]
struct CompileFailCases;

#[doc(hidden)]
pub use sumtype_macro::_sumtrait_internal;

/// Makes a user-defined trait usable with `#[sumtype]`.
///
/// ## Arguments
///
/// - `implement` (optional) — the real trait that the generated sum type should implement. Use it
///   to make a `std` (or other third-party) trait compatible with `sumtype` by declaring a local
///   mock trait that mirrors it.
/// - `krate` (optional) — the path to the `sumtype` crate (default: `::sumtype`).
/// - `marker` — the absolute path of an empty type, defined in the same crate as the trait and
///   visible to the crates that will use the trait with `#[sumtype]`.
///
/// ## Supertraits
///
/// Supertraits are declared with the usual `trait Foo: Bar { ... }` syntax. Each supertrait must be
/// either a built-in trait that `sumtype` recognizes (`Copy`, `Clone`, `Hash`, `Debug`, `Display`,
/// `Error`, `Iterator`, `Read` — bare or via a `std`/`core` path) or another `#[sumtrait]` trait;
/// referring to a custom trait by an absolute path is recommended.
///
/// ## Sumtrait-safety
///
/// Every trait annotated with `#[sumtrait]` must be *sumtrait-safe*. This is similar to object
/// safety, but not identical.
///
/// A trait is called sumtrait-safe when it satisfies all of the following:
///
/// - The trait should not accept any generic arguments.
/// - All supertraits of the trait are also sumtrait-safe and annotated with `#[sumtrait]`.
/// - All trait items are either associated types or associated functions.
/// - All associated functions must take a receiver (`self`, `&self`, or `&mut self`) as their
///   first parameter. Additional parameters are allowed, but their types must not contain `Self`.
/// - All associated functions should return the `Self` type, or a return type that does not
///   contain `Self`.
/// ```
/// # use sumtype::sumtrait;
/// pub struct Marker(::core::convert::Infallible);
/// #[sumtrait(marker = Marker)] // In practice, `Marker` must be an absolute path beginning with `::`
/// trait MyTrait {}
/// ```
pub use sumtype_macro::sumtrait;

/// Enables the `sumtype!(..)` macro within the annotated context.
///
/// For each context marked with `#[sumtype]`, the macro creates a single anonymous sum type that
/// implements the trait(s) you specify. Wrap an expression with `sumtype!(expr)` to store it in
/// that sum type. For example, to return a unified iterator:
///
/// ```
/// # use sumtype::sumtype;
/// # use std::iter::Iterator;
/// #[sumtype(sumtype::traits::Iterator)]
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
/// #[sumtype(sumtype::traits::Iterator)]
/// fn return_iter_explicit(a: bool) -> sumtype!() {
///     if a {
///         sumtype!(std::iter::once(()), std::iter::Once<()>)
///     } else {
///         sumtype!(vec![()].into_iter(), std::vec::IntoIter<()>)
///     }
/// }
/// ```
pub use sumtype_macro::sumtype;

#[doc(hidden)]
pub trait TypeRef<const RANDOM: usize, const N: usize> {
    type Type: ?Sized;
}

/// Mock traits targeted by the `#[sumtype]` macro. They make the corresponding `std`/`core` traits
/// usable with `#[sumtype]`.
pub mod traits {
    use super::sumtrait;

    #[doc(hidden)]
    #[allow(non_camel_case_types)]
    trait __SumTrait_Sealed {}

    macro_rules! emit_traits {
        () => {
            #[doc(hidden)]
            pub struct Marker(::core::convert::Infallible);

            /// Target of [`crate::sumtype`] macro, which implements [`std::io::Read`].
            #[sumtrait(implement = ::std::io::Read, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Read {
                fn read(&mut self, buf: &mut [::core::primitive::u8]) -> ::std::io::Result<::core::primitive::usize>;
            }

            impl<T: ::std::io::Read> Read for T {
                fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                    T::read(self, buf)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::iter::Iterator`].
            #[sumtrait(implement = ::core::iter::Iterator, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Iterator {
                type Item;
                fn next(&mut self) -> ::core::option::Option<Self::Item>;
            }

            impl<T: ::core::iter::Iterator> Iterator for T {
                type Item = T::Item;
                fn next(&mut self) -> Option<Self::Item> {
                    T::next(self)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::marker::Copy`].
            #[sumtrait(implement = ::core::marker::Copy, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Copy: $crate::traits::Clone {
            }

            impl<T: ::core::marker::Copy> Copy for T {}

            /// Target of [`crate::sumtype`] macro, which implements [`std::clone::Clone`].
            #[sumtrait(implement = ::core::clone::Clone, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Clone {
                fn clone(&self) -> Self;
            }

            impl<T: ::core::clone::Clone> Clone for T {
                fn clone(&self) -> Self {
                    T::clone(self)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::hash::Hash`].
            ///
            /// The generated sum type forwards `hash` to the active variant (no discriminant is
            /// mixed in). The enum does not implement `Eq`/`PartialEq`, so there is no equality
            /// relation for the hash to be inconsistent with.
            #[sumtrait(implement = ::core::hash::Hash, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Hash {
                fn hash<H: ::core::hash::Hasher>(&self, state: &mut H);
            }

            impl<T: ::core::hash::Hash> Hash for T {
                fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                    T::hash(self, state)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::fmt::Display`].
            #[sumtrait(implement = ::core::fmt::Display, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Display {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result;
            }

            impl<T: ::core::fmt::Display> Display for T {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    T::fmt(self, f)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::fmt::Debug`].
            #[sumtrait(implement = ::core::fmt::Debug, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Debug {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result;
            }

            impl<T: ::core::fmt::Debug> Debug for T {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    T::fmt(self, f)
                }
            }

            /// Target of [`crate::sumtype`] macro, which implements [`std::error::Error`].
            #[sumtrait(implement = ::std::error::Error, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Error: $crate::traits::Debug + $crate::traits::Display {
                fn source(&self) -> ::core::option::Option<&(dyn ::std::error::Error + 'static)>;
            }

            impl<T: ::std::error::Error> Error for T {
                fn source(&self) -> ::core::option::Option<&(dyn ::std::error::Error + 'static)> {
                    T::source(self)
                }
            }

        };
    }
    emit_traits!();
}
