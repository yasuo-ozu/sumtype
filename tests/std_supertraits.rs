//! An `announce-std` path written as the supertrait of an announced trait is invoked as it is (it is
//! not one of the `std` paths that `#[sumtype]` maps to the mocks).

use sumtype::{announce::announce, sumtype};

#[announce(crate = ::sumtype::announce, sumtype::check)]
trait MySumTrait: ::announce_std::clone::Clone {}

#[allow(unused, clippy::implied_bounds_in_impls)]
#[sumtype(MySumTrait)]
fn f1(a: bool) -> impl MySumTrait + Clone {
    #[derive(Clone)]
    struct S1;
    #[derive(Clone)]
    struct S2;
    impl MySumTrait for S1 {}
    impl MySumTrait for S2 {}
    if a {
        sumtype!(S1)
    } else {
        sumtype!(S2)
    }
}

#[announce(crate = ::sumtype::announce, sumtype::check)]
trait MyCopy: ::announce_std::marker::Copy {}

impl MyCopy for usize {}
impl MyCopy for i8 {}

#[allow(unused)]
#[sumtype(MyCopy)]
fn f2(a: bool) -> impl Copy {
    if a {
        sumtype!(1i8)
    } else {
        sumtype!(1usize)
    }
}
