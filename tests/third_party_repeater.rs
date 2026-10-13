//! A third-party `#[announce(repeater = ..)]` trait used as a `#[sumtype]` bound from another
//! module: its relative types are rewritten with the entry's `type_leak` META.

sumtype::__private::announce::define_repeater!(pub Repeater);

pub mod defs {
    pub struct Local(pub u32);

    #[sumtype::__private::announce::announce(
        crate = sumtype::__private::announce,
        repeater = crate::Repeater
    )]
    pub trait Value {
        fn value(&self) -> Local;
    }
}

mod user {
    // `Local` is deliberately not in scope here.
    use crate::defs::Value;
    use sumtype::sumtype;

    pub struct A;
    pub struct B;
    impl Value for A {
        fn value(&self) -> crate::defs::Local {
            crate::defs::Local(1)
        }
    }
    impl Value for B {
        fn value(&self) -> crate::defs::Local {
            crate::defs::Local(2)
        }
    }

    #[sumtype(crate::defs::Value)]
    pub fn pick(a: bool) -> impl Value {
        if a {
            sumtype!(A)
        } else {
            sumtype!(B)
        }
    }
}

#[test]
fn third_party_repeater_trait() {
    use defs::Value as _;
    assert_eq!(user::pick(true).value().0, 1);
    assert_eq!(user::pick(false).value().0, 2);
}
