use sumtype::{sumtrait, sumtype};

#[allow(unused)]
struct Marker;

#[sumtrait(marker = Marker)]
trait MySumTrait: sumtype::traits::Clone {}

#[allow(unused)]
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

#[sumtrait(marker = Marker)]
trait MyCopy: sumtype::traits::Copy {}

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
