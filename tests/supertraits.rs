//! Coverage for the supertrait analysis of announced traits. A supertrait that is a built-in std/core trait
//! is recognized and mapped to its `announce-std` mock, whether written as a fully-qualified
//! path (`::std::fmt::Debug`) or — for traits reachable by name (e.g. the prelude `Clone`, or an
//! imported trait) — a bare ident. (Bare `Debug`/`Hash` are intentionally not exercised here: in
//! bound position an unqualified `Debug`/`Hash` resolves to the prelude *derive macro*, a Rust
//! scoping rule independent of sumtype, so users qualify them — which works, below.)

#![allow(dead_code)]

use sumtype::{announce::announce, sumtype};

mod qualified_debug {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Named: ::std::fmt::Debug {
        fn name(&self) -> &'static str;
    }
    #[derive(Debug)]
    struct A;
    #[derive(Debug)]
    struct B;
    impl Named for A {
        fn name(&self) -> &'static str {
            "A"
        }
    }
    impl Named for B {
        fn name(&self) -> &'static str {
            "B"
        }
    }
    #[sumtype(Named)]
    fn make(b: bool) -> impl Named {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(format!("{:?}", make(true)), "A");
        assert_eq!(make(false).name(), "B");
    }
}

mod qualified_display {
    use super::*;
    use std::fmt;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Labeled: ::std::fmt::Display {}
    struct A;
    struct B;
    impl fmt::Display for A {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "A")
        }
    }
    impl fmt::Display for B {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "B")
        }
    }
    impl Labeled for A {}
    impl Labeled for B {}
    #[sumtype(Labeled)]
    fn make(b: bool) -> impl Labeled {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        assert_eq!(format!("{}", make(true)), "A");
        assert_eq!(format!("{}", make(false)), "B");
    }
}

mod qualified_hash_and_debug {
    use super::*;
    use std::hash::{Hash, Hasher};
    // Two qualified built-in supertraits at once.
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Keyish: ::std::fmt::Debug + ::std::hash::Hash {}
    #[derive(Debug, Hash)]
    struct A(u32);
    #[derive(Debug, Hash)]
    struct B(u64);
    impl Keyish for A {}
    impl Keyish for B {}
    #[sumtype(Keyish)]
    fn make(b: bool) -> impl Keyish {
        if b {
            sumtype!(A(1))
        } else {
            sumtype!(B(2))
        }
    }
    fn hash_of<T: Hash>(t: &T) -> u64 {
        let mut h = std::collections::hash_map::DefaultHasher::new();
        t.hash(&mut h);
        h.finish()
    }
    #[test]
    fn works() {
        assert_eq!(format!("{:?}", make(true)), "A(1)");
        assert_eq!(hash_of(&make(true)), hash_of(&A(1)));
    }
}

mod bare_prelude_clone {
    use super::*;
    // `Clone` is a prelude trait, so the bare ident (allowed by `allow_std_prelude`) resolves and
    // maps to `announce_std::clone::Clone`.
    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    trait Dup: Clone {
        fn tag(&self) -> u8;
    }
    #[derive(Clone)]
    struct A;
    #[derive(Clone)]
    struct B;
    impl Dup for A {
        fn tag(&self) -> u8 {
            1
        }
    }
    impl Dup for B {
        fn tag(&self) -> u8 {
            2
        }
    }
    #[sumtype(Dup)]
    fn make(b: bool) -> impl Dup {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn works() {
        let x = make(true);
        let y = x.clone(); // `Clone` supertrait honored
        assert_eq!(x.tag(), 1);
        assert_eq!(y.tag(), 1);
    }
}
