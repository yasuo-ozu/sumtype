#![doc = include_str!("README.md")]

pub use sumtype_macro::{_sumtrait_internal, sumtrait, sumtype};

#[doc(hidden)]
pub trait TypeRef<const RANDOM: usize, const N: usize> {
    type Type: ?Sized;
}

pub mod traits {
    use super::sumtrait;
    #[doc(hidden)]
    #[allow(non_camel_case_types)]
    trait __SumTrait_Sealed_Read {}

    #[sumtrait(implement = ::std::io::Read, krate = crate)]
    #[allow(private_bounds)]
    pub trait Read: __SumTrait_Sealed_Read {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
    }

    impl<T: std::io::Read> __SumTrait_Sealed_Read for T {}
    impl<T: std::io::Read> Read for T {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            T::read(self, buf)
        }
    }

    #[doc(hidden)]
    #[allow(non_camel_case_types)]
    trait __SumTrait_Sealed_Iterator {}

    #[sumtrait(implement = ::core::iter::Iterator, krate = crate)]
    #[allow(private_bounds)]
    pub trait Iterator: __SumTrait_Sealed_Iterator {
        type Item;
        fn next(&mut self) -> Option<Self::Item>;
    }

    impl<T: core::iter::Iterator> __SumTrait_Sealed_Iterator for T {}
    impl<T: core::iter::Iterator> Iterator for T {
        type Item = T::Item;
        fn next(&mut self) -> Option<Self::Item> {
            T::next(self)
        }
    }
}
