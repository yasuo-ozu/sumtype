//! Regression tests for the supertrait fixes found in the re-audit:
//!  - user trait chaining (`Derived: Base`) now generates `impl Base`, not `impl Derived`
//!    (was E0407 "method not a member of trait");
//!  - a user trait whose *name* collides with a built-in (e.g. a user `Iterator`) used bare as a
//!    supertrait is forwarded to the user's dispatch macro, not mis-mapped to an `announce-std` mock.
//!
//! A user supertrait is written with an absolute path (`crate::mymod::Foo`), as announce requires.

#![allow(dead_code)]

use sumtype::{announce::announce, sumtype};

// A user trait chained as the supertrait of another one.
mod user_chain {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Base {
        fn base(&self) -> u32;
    }
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Derived: crate::user_chain::Base {
        fn derived(&self) -> u32;
    }
    struct A;
    struct B;
    impl Base for A {
        fn base(&self) -> u32 {
            1
        }
    }
    impl Base for B {
        fn base(&self) -> u32 {
            2
        }
    }
    impl Derived for A {
        fn derived(&self) -> u32 {
            10
        }
    }
    impl Derived for B {
        fn derived(&self) -> u32 {
            20
        }
    }
    #[sumtype(Derived)]
    fn make(b: bool) -> impl Derived {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn supertrait_method_is_dispatched() {
        let x = make(true);
        assert_eq!(x.derived(), 10);
        assert_eq!(x.base(), 1); // inherited (supertrait) method dispatched
        let y = make(false);
        assert_eq!(y.base(), 2);
        assert_eq!(y.derived(), 20);
    }
}

// A user trait named `Iterator` used as a supertrait must be forwarded to its own trampoline, not
// mapped to `::announce_std::iter::Iterator`.
mod bare_name_collision {
    use super::*;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Iterator {
        fn my_step(&self) -> u8;
    }
    struct A;
    struct B;
    impl Iterator for A {
        fn my_step(&self) -> u8 {
            7
        }
    }
    impl Iterator for B {
        fn my_step(&self) -> u8 {
            9
        }
    }
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait Stepper: crate::bare_name_collision::Iterator {
        fn label(&self) -> &'static str;
    }
    impl Stepper for A {
        fn label(&self) -> &'static str {
            "A"
        }
    }
    impl Stepper for B {
        fn label(&self) -> &'static str {
            "B"
        }
    }
    #[sumtype(Stepper)]
    fn make(b: bool) -> impl Stepper {
        if b {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
    #[test]
    fn user_trait_named_like_builtin_is_forwarded() {
        let x = make(true);
        assert_eq!(x.my_step(), 7); // user `Iterator::my_step`, NOT std Iterator
        assert_eq!(x.label(), "A");
        assert_eq!(make(false).my_step(), 9);
    }
}

// A qualified std supertrait (`std::error::Error`, which itself requires `Debug + Display`) on a
// user trait still maps to the mock and chains correctly.
mod std_error_supertrait {
    use super::*;
    use std::fmt;
    #[announce(crate = ::sumtype::announce, sumtype::check)]
    trait AppError: ::std::error::Error {
        fn code(&self) -> u32;
    }
    #[derive(Debug)]
    struct E1;
    #[derive(Debug)]
    struct E2;
    impl fmt::Display for E1 {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "E1")
        }
    }
    impl fmt::Display for E2 {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "E2")
        }
    }
    impl std::error::Error for E1 {}
    impl std::error::Error for E2 {}
    impl AppError for E1 {
        fn code(&self) -> u32 {
            1
        }
    }
    impl AppError for E2 {
        fn code(&self) -> u32 {
            2
        }
    }
    #[sumtype(AppError)]
    fn make(b: bool) -> impl AppError {
        if b {
            sumtype!(E1)
        } else {
            sumtype!(E2)
        }
    }
    #[test]
    fn error_supertrait_chains_debug_and_display() {
        let e = make(true);
        assert_eq!(e.code(), 1);
        assert_eq!(format!("{}", e), "E1"); // Display (via Error: Display)
        assert_eq!(format!("{:?}", e), "E1"); // Debug (via Error: Debug)
        let boxed: Box<dyn std::error::Error> = Box::new(make(false));
        assert_eq!(format!("{}", boxed), "E2");
    }
}
