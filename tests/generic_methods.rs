//! Coverage for announced traits with *generic* methods. The dispatch codegen forwards the
//! method's own generics to both the signature and (as a turbofish) the dispatched call, so a
//! generic that is inferable from the arguments/return OR supplied only via turbofish (including a
//! const generic) all resolve. The const-generic case used to fail with `E0284`.

#![allow(dead_code)]

use sumtype::{announce::announce, sumtype};

mod arg_position {
    use super::*;
    use std::fmt::Write;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Greet {
        fn greet<W: ::core::fmt::Write>(&self, w: &mut W);
    }
    struct A;
    struct B;
    impl Greet for A {
        fn greet<W: Write>(&self, w: &mut W) {
            write!(w, "A").unwrap();
        }
    }
    impl Greet for B {
        fn greet<W: Write>(&self, w: &mut W) {
            write!(w, "B").unwrap();
        }
    }
    #[sumtype(Greet)]
    fn make(b: bool) -> impl Greet {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        let mut s = String::new();
        make(true).greet(&mut s);
        make(false).greet(&mut s);
        assert_eq!(s, "AB");
    }
}

mod by_value {
    use super::*;
    use std::fmt::Debug;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Foo {
        fn foo<T: ::core::fmt::Debug>(&self, x: T) -> ::std::string::String;
    }
    struct A;
    struct B;
    impl Foo for A {
        fn foo<T: Debug>(&self, x: T) -> String {
            format!("A:{:?}", x)
        }
    }
    impl Foo for B {
        fn foo<T: Debug>(&self, x: T) -> String {
            format!("B:{:?}", x)
        }
    }
    #[sumtype(Foo)]
    fn make(b: bool) -> impl Foo {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).foo(42), "A:42");
        assert_eq!(make(false).foo("x"), "B:\"x\"");
    }
}

mod return_only {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Make {
        fn make_default<T: ::core::default::Default>(&self) -> T;
    }
    struct A;
    struct B;
    impl Make for A {
        fn make_default<T: Default>(&self) -> T {
            T::default()
        }
    }
    impl Make for B {
        fn make_default<T: Default>(&self) -> T {
            T::default()
        }
    }
    #[sumtype(Make)]
    fn make(b: bool) -> impl Make {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        let x: i32 = make(true).make_default();
        assert_eq!(x, 0);
    }
}

mod const_generic {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Idx {
        fn val<const N: usize>(&self) -> usize;
    }
    struct A;
    struct B;
    impl Idx for A {
        fn val<const N: usize>(&self) -> usize {
            N
        }
    }
    impl Idx for B {
        fn val<const N: usize>(&self) -> usize {
            N * 2
        }
    }
    #[sumtype(Idx)]
    fn make(b: bool) -> impl Idx {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).val::<3>(), 3);
        assert_eq!(make(false).val::<3>(), 6);
    }
}

mod lifetime_generic {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Pick {
        fn pick<'x>(&self, s: &'x str) -> &'x str;
    }
    struct A;
    struct B;
    impl Pick for A {
        fn pick<'x>(&self, s: &'x str) -> &'x str {
            s
        }
    }
    impl Pick for B {
        fn pick<'x>(&self, _s: &'x str) -> &'x str {
            "B"
        }
    }
    #[sumtype(Pick)]
    fn make(b: bool) -> impl Pick {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(make(true).pick("hi"), "hi");
        assert_eq!(make(false).pick("hi"), "B");
    }
}
