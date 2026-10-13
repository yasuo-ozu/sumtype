//! Two different `#[sumtrait]` traits with the same name are two nodes (keyed by the trampoline
//! path, not the bare ident), and a qualified bound is implemented by that path.

#![allow(dead_code)]

use sumtype::{sumtrait, sumtype};

pub struct M;

mod a {
    #[sumtype::sumtrait(marker = crate::M)]
    pub trait Named {
        fn a_name(&self) -> &'static str;
    }
}
mod b {
    #[sumtype::sumtrait(marker = crate::M)]
    pub trait Named {
        fn b_name(&self) -> &'static str;
    }
}

struct X;
struct Y;
impl a::Named for X {
    fn a_name(&self) -> &'static str {
        "ax"
    }
}
impl a::Named for Y {
    fn a_name(&self) -> &'static str {
        "ay"
    }
}
impl b::Named for X {
    fn b_name(&self) -> &'static str {
        "bx"
    }
}
impl b::Named for Y {
    fn b_name(&self) -> &'static str {
        "by"
    }
}

#[sumtype(a::Named + b::Named)]
fn make(x: bool) -> impl a::Named + b::Named {
    if x {
        sumtype!(X)
    } else {
        sumtype!(Y)
    }
}

// A user supertrait reached through a qualified path.
mod base {
    #[sumtype::sumtrait(marker = crate::M)]
    pub trait Base {
        fn base(&self) -> u8;
    }
}
#[sumtrait(marker = M)]
trait Derived: base::Base {
    fn derived(&self) -> u8;
}
impl base::Base for X {
    fn base(&self) -> u8 {
        1
    }
}
impl base::Base for Y {
    fn base(&self) -> u8 {
        2
    }
}
impl Derived for X {
    fn derived(&self) -> u8 {
        10
    }
}
impl Derived for Y {
    fn derived(&self) -> u8 {
        20
    }
}

#[sumtype(Derived)]
fn make_derived(x: bool) -> impl Derived {
    if x {
        sumtype!(X)
    } else {
        sumtype!(Y)
    }
}

#[test]
fn same_name_traits_are_distinct() {
    use a::Named as _;
    use b::Named as _;
    let (x, y) = (make(true), make(false));
    assert_eq!((x.a_name(), x.b_name()), ("ax", "bx"));
    assert_eq!((y.a_name(), y.b_name()), ("ay", "by"));
}

#[test]
fn qualified_supertrait() {
    use base::Base as _;
    let v = make_derived(false);
    assert_eq!((v.base(), v.derived()), (2, 20));
}
