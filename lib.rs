#![doc = include_str!("README.md")]

#[doc(hidden)]
pub use sumtype_macro::_sumtrait_internal;

/// Make an user-defined trait usable by sumtype.
///
/// ## Arguments
///
/// - `implement` (optional) ... actual implemented trait with `#[sumtype]`. This argument
///   is used for traits of `std` (or other 3rd-party libs) to be compatible with sumtype.
/// - `krate` (optional) ... the path to `sumtype` crate. Default is `::sumtype`.
/// - `marker` ... You should specify as absolute path of an empty type (defined in the same
///   crate with the trait), which is visible from user crates (who will implement the trait
///   with `#[sumtype]`).
///
/// ```
/// # use sumtype::sumtrait;
/// pub struct Marker(::core::convert::Infallible);
/// #[sumtrait(marker = Marker)] // In practice, `Marker` must be absolute path begins with `::`
/// trait MyTrait {}
/// ```
pub use sumtype_macro::sumtrait;

/// Enabling `sumtype!(..)` macro in the context.
///
/// For each context marked by `#[sumtype]`, sumtype makes an union type of several
/// [`std::iter::Iterator`] types. To intern an expression of `Iterator` into the union type, you
/// can use `sumtype!([expr])` syntax. This is an example of returning unified `Iterator`:
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
/// This function returns [`std::iter::Once`] or [`std::vec::IntoIter`] depends on `a` value. The
/// `#[sumtype]` system make annonymous union type that is also [`std::iter::Iterator`], and wrap
/// each `sumtype!(..)` expression with the union type. The mechanism is zerocost when `a` is fixed
/// in the compile process.
///
/// You can place exact (non-annonymous) type using `sumtype!()` macro in type context. In this
/// way, you should specify type using `sumtype!([expr], [type])` format like:
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

/// Supplies mock traits which are targetted by `#[sumtype]` macros. This is to make
/// traits in other crates be compatible with `#[sumtype]`.
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
            pub trait Read: __SumTrait_Sealed {
                fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
            }

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::iter::Iterator`].
            #[sumtrait(implement = ::core::iter::Iterator, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Iterator: __SumTrait_Sealed {
                type Item;
                fn next(&mut self) -> Option<Self::Item>;
            }

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::marker::Copy`].
            #[sumtrait(implement = ::core::marker::Copy, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Copy: __SumTrait_Sealed {
            }

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::marker::Clone`].
            #[sumtrait(implement = ::core::marker::Clone, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Clone: __SumTrait_Sealed {
                fn clone(&self) -> Self;
            }

            /// Target of [`sumtype::sumtype`] macro, which implements [`std::cmp::Ord`].
            #[sumtrait(implement = ::core::cmp::Ord, krate = $crate, marker = $crate::traits::Marker)]
            #[allow(private_bounds)]
            pub trait Ord: __SumTrait_Sealed {
                fn cmp(&self, other: &Self) -> core::cmp::Ordering;
            }
        };
    }
    emit_traits!();
}
