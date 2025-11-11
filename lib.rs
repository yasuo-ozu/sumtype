#![doc = include_str!("README.md")]

#[doc(hidden)]
pub use sumtype_macro::_sumtrait_internal;

/// Makes a user-defined trait usable by sumtype.
///
/// ## Arguments
///
/// - `implement` (optional) - The actual implemented trait with `#[sumtype]`. This argument
///   is used for traits from `std` (or other 3rd-party libraries) to be compatible with sumtype.
/// - `krate` (optional) - The path to the `sumtype` crate. Default is `::sumtype`.
/// - `marker` - You should specify the absolute path of an empty type (defined in the same
///   crate as the trait), which is visible from user crates (who will implement the trait
///   with `#[sumtype]`).
///
/// ## Supertraits
///
/// You can specify supertraits using `trait ... : <supertraits> { ... }` syntax. All supertraits
/// should also be annotated with `#[sumtrait]` and specified with absolute paths for best practice.
///
/// ## Sumtrait-safe
///
/// All traits annotated with `#[sumtrait]` must satisfy sumtrait-safety. This is similar to object safety,
/// but slightly different.
///
/// A trait is called sumtrait-safe when it satisfies all of the following:
///
/// - The trait should not accept any generic arguments.
/// - All supertraits of the trait are also sumtrait-safe and annotated with `#[sumtrait]`.
/// - All trait items are either associated types or associated functions.
/// - All associated functions have zero or one input parameter including the receiver, whose
///   type is `Self`, `&Self`, or `&mut Self`. Other parameters should not contain `Self` types.
/// - All associated functions should return the `Self` type, or return types that do not
///   contain `Self` types.
/// ```
/// # use sumtype::sumtrait;
/// pub struct Marker(::core::convert::Infallible);
/// #[sumtrait(marker = Marker)] // In practice, `Marker` must be an absolute path beginning with `::`
/// trait MyTrait {}
/// ```
pub use sumtype_macro::sumtrait;

/// Enables `sumtype!(..)` macro in the context.
///
/// For each context marked by `#[sumtype]`, sumtype creates a union type of several
/// [`std::iter::Iterator`] types. To intern an expression of `Iterator` into the union type, you
/// can use `sumtype!([expr])` syntax. This is an example of returning a unified `Iterator`:
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
/// This function returns [`std::iter::Once`] or [`std::vec::IntoIter`] depending on the `a` value. The
/// `#[sumtype]` system creates an anonymous union type that is also [`std::iter::Iterator`], and wraps
/// each `sumtype!(..)` expression with the union type. The mechanism is zero-cost when `a` is fixed
/// at compile time.
///
/// You can specify the exact (non-anonymous) type using `sumtype!()` macro in type context. In this
/// case, you should specify the type using `sumtype!([expr], [type])` format like:
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

/// Provides mock traits which are targeted by `#[sumtype]` macros. This is to make
/// traits from other crates compatible with `#[sumtype]`.
pub mod traits {
    use super::sumtrait;

    #[doc(hidden)]
    #[allow(non_camel_case_types)]
    trait __SumTrait_Sealed {}

    macro_rules! emit_traits {
        () => {
            #[doc(hidden)]
            pub struct Marker(::core::convert::Infallible);

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::io::Read`].
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

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::iter::Iterator`].
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

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::marker::Copy`].
            #[sumtrait(implement = ::core::marker::Copy, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Copy: $crate::traits::Clone {
            }

            impl<T: ::core::marker::Copy> Copy for T {}

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::marker::Clone`].
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

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::fmt::Display`].
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

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::fmt::Debug`].
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

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::error::Error`].
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
