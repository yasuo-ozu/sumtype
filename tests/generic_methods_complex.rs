//! Generic methods of announced traits with *complex* generic parameter lists: multiple type params,
//! where-clauses with associated-type bounds, higher-ranked trait bounds, dependent bounds with a
//! turbofish-only return generic, and mixed lifetime/type/const generics (including const in the
//! return type). All exercise the turbofish-forwarding dispatch.

#![allow(dead_code)]

use sumtype::{announce::announce, sumtype};

mod multiple_type_params {
    use super::*;
    use std::fmt::Display;
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Combine {
        fn combine<T: ::core::fmt::Display, U: ::core::fmt::Display>(&self, t: T, u: U) -> String;
    }
    struct A;
    struct B;
    impl Combine for A {
        fn combine<T: Display, U: Display>(&self, t: T, u: U) -> String {
            format!("A({t},{u})")
        }
    }
    impl Combine for B {
        fn combine<T: Display, U: Display>(&self, t: T, u: U) -> String {
            format!("B({t},{u})")
        }
    }
    #[sumtype(Combine)]
    fn make(b: bool) -> impl Combine {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).combine(1, "x"), "A(1,x)");
        assert_eq!(make(false).combine(2.5, 'z'), "B(2.5,z)");
    }
}

mod where_clause_assoc_type {
    use super::*;
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Total {
        fn total<I>(&self, it: I) -> u32
        where
            I: IntoIterator<Item = u32>;
    }
    struct A;
    struct B;
    impl Total for A {
        fn total<I>(&self, it: I) -> u32
        where
            I: IntoIterator<Item = u32>,
        {
            it.into_iter().sum()
        }
    }
    impl Total for B {
        fn total<I>(&self, it: I) -> u32
        where
            I: IntoIterator<Item = u32>,
        {
            it.into_iter().sum::<u32>() + 100
        }
    }
    #[sumtype(Total)]
    fn make(b: bool) -> impl Total {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).total(vec![1, 2, 3]), 6);
        assert_eq!(make(false).total(vec![1, 2, 3]), 106);
    }
}

mod higher_ranked_bound {
    use super::*;
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Apply {
        fn apply<F>(&self, f: F) -> usize
        where
            F: for<'a> Fn(&'a str) -> usize;
    }
    struct A;
    struct B;
    impl Apply for A {
        fn apply<F>(&self, f: F) -> usize
        where
            F: for<'a> Fn(&'a str) -> usize,
        {
            f("hello")
        }
    }
    impl Apply for B {
        fn apply<F>(&self, f: F) -> usize
        where
            F: for<'a> Fn(&'a str) -> usize,
        {
            f("hi") + 1
        }
    }
    #[sumtype(Apply)]
    fn make(b: bool) -> impl Apply {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).apply(|s: &str| s.len()), 5);
        assert_eq!(make(false).apply(|s: &str| s.len()), 3);
    }
}

mod dependent_turbofish_return {
    use super::*;
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Convert {
        fn convert<T, U: From<T>>(&self, t: T) -> U;
    }
    struct A;
    struct B;
    impl Convert for A {
        fn convert<T, U: From<T>>(&self, t: T) -> U {
            U::from(t)
        }
    }
    impl Convert for B {
        fn convert<T, U: From<T>>(&self, t: T) -> U {
            U::from(t)
        }
    }
    #[sumtype(Convert)]
    fn make(b: bool) -> impl Convert {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        let x: i64 = make(true).convert::<i32, i64>(5);
        assert_eq!(x, 5i64);
    }
}

mod lifetime_type_const_mix {
    use super::*;
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Arr {
        fn arr<'a, T: Copy, const N: usize>(&self, x: T, tag: &'a str) -> ([T; N], &'a str);
    }
    struct A;
    struct B;
    impl Arr for A {
        fn arr<'a, T: Copy, const N: usize>(&self, x: T, tag: &'a str) -> ([T; N], &'a str) {
            ([x; N], tag)
        }
    }
    impl Arr for B {
        fn arr<'a, T: Copy, const N: usize>(&self, x: T, tag: &'a str) -> ([T; N], &'a str) {
            ([x; N], tag)
        }
    }
    #[sumtype(Arr)]
    fn make(b: bool) -> impl Arr {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        let (arr, tag) = make(true).arr::<u8, 3>(7, "t");
        assert_eq!(arr, [7u8, 7, 7]);
        assert_eq!(tag, "t");
    }
}
