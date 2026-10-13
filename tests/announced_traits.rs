//! Traits announced with `#[announce]` are used by `#[sumtype]` through their trampoline: the
//! options of announce that replace what `#[sumtrait]` did (`sumtype::check`, `mock`,
//! `allow_std_prelude`, and the module copy that resolves relative paths at the `#[sumtype]`).

#![allow(dead_code)]

use sumtype::{announce::announce, sumtype};

// A checked trait with an absolute supertrait and bare prelude types.
mod checked {
    use super::*;

    #[announce(crate = ::sumtype::announce, allow_std_prelude = true, sumtype::check)]
    pub trait Named: ::core::fmt::Debug {
        fn name(&self) -> String;
        fn rename(&mut self, name: Option<&str>);
    }

    #[derive(Debug)]
    struct A(String);
    #[derive(Debug)]
    struct B;
    impl Named for A {
        fn name(&self) -> String {
            self.0.clone()
        }
        fn rename(&mut self, name: Option<&str>) {
            self.0 = name.unwrap_or("none").into();
        }
    }
    impl Named for B {
        fn name(&self) -> String {
            "B".into()
        }
        fn rename(&mut self, _: Option<&str>) {}
    }

    #[sumtype(Named)]
    fn make(a: bool) -> impl Named {
        if a {
            sumtype!(A("a".into()))
        } else {
            sumtype!(B)
        }
    }

    #[test]
    fn dispatches() {
        let mut x = make(true);
        assert_eq!(x.name(), "a");
        x.rename(None);
        assert_eq!(x.name(), "none");
        assert_eq!(format!("{:?}", make(false)), "B");
    }
}

// `#[announce(mock = path)]` on a local trait that mirrors a real trait makes `#[sumtype]`
// implement the real one (what `#[sumtrait(implement = path)]` did).
mod mock {
    use super::*;
    use std::fmt;

    #[announce(crate = ::sumtype::announce, mock = ::core::fmt::Display)]
    pub trait MyDisplay {
        fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result;
    }

    struct A;
    struct B;
    impl fmt::Display for A {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "a")
        }
    }
    impl fmt::Display for B {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "b")
        }
    }

    #[sumtype(MyDisplay)]
    fn make(a: bool) -> impl fmt::Display {
        if a {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }

    #[test]
    fn implements_the_real_trait() {
        assert_eq!(make(true).to_string(), "a");
        assert_eq!(make(false).to_string(), "b");
    }
}

// Relative paths are an error in an announced trait. A copy wrapped in an inline module of the same
// name (announce's `emit_item = false` pattern) resolves them at the `#[sumtype]` instead.
mod module_copy {
    use super::*;
    use std::fmt::Write;

    pub struct Local(pub u8);

    pub trait Greet {
        fn greet<W: Write>(&self, w: &mut W) -> Local;
    }
    #[announce(crate = ::sumtype::announce, emit_item = false)]
    pub mod Greet {
        pub trait Greet {
            fn greet<W: Write>(&self, w: &mut W) -> Local;
        }
    }

    struct A;
    struct B;
    impl Greet for A {
        fn greet<W: Write>(&self, w: &mut W) -> Local {
            write!(w, "A").unwrap();
            Local(1)
        }
    }
    impl Greet for B {
        fn greet<W: Write>(&self, w: &mut W) -> Local {
            write!(w, "B").unwrap();
            Local(2)
        }
    }

    #[sumtype(Greet)]
    fn make(a: bool) -> impl Greet {
        if a {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }

    #[test]
    fn relative_paths_resolve_at_the_sumtype() {
        let mut s = String::new();
        assert_eq!(make(true).greet(&mut s).0, 1);
        assert_eq!(make(false).greet(&mut s).0, 2);
        assert_eq!(s, "AB");
    }
}
