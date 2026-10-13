//! Coverage for the auto-generated `Downcast` impl on `#[sumtype]` sum types, exercising both ways
//! to make `Downcast` reachable on a function result:
//!
//!  - [`via_return_bound`]: name the target type(s) in an `impl Trait + Downcast<To>` return — this
//!    works with the type-less `sumtype!(expr)` form (no explicit variant type needed).
//!  - [`via_named_type`]: return the *named* `sumtype!()` type with explicit `sumtype!(expr, Type)`
//!    branches — this exposes `Downcast<To>` for *every* wrapped type at once, without listing them.

use sumtype::{sumtype, Downcast};

/// Reaching `Downcast` by naming the targets in the `impl Trait` return bound (type-less branches).
mod via_return_bound {
    use super::*;

    // Two downcast targets of distinct variant types; type-less `sumtype!(expr)` branches.
    #[sumtype(::announce_std::fmt::Debug)]
    fn two_types(b: bool) -> impl std::fmt::Debug + Downcast<u32> + Downcast<String> {
        if b {
            sumtype!(1u32)
        } else {
            sumtype!(String::from("hi"))
        }
    }

    #[test]
    fn downcast_ref_mut_owned() {
        let v = two_types(true);
        assert_eq!(Downcast::<u32>::downcast_ref(&v), Some(&1u32));
        assert_eq!(Downcast::<String>::downcast_ref(&v), None);

        let mut v = two_types(false);
        if let Some(s) = Downcast::<String>::downcast_mut(&mut v) {
            s.push('!');
        }
        assert_eq!(Downcast::<String>::downcast_ref(&v), Some(&String::from("hi!")));

        // Owned: recover the value on a match...
        assert_eq!(Downcast::<u32>::downcast(two_types(true)).ok(), Some(1u32));
        // ...or get the value back (as the opaque result type) on a miss, still usable.
        let back = Downcast::<u32>::downcast(two_types(false)).unwrap_err();
        assert_eq!(Downcast::<String>::downcast_ref(&back), Some(&String::from("hi")));
    }

    fn extract_u32<E: Downcast<u32>>(e: E) -> Option<u32> {
        e.downcast().ok()
    }

    #[test]
    fn downcast_via_generic_bound() {
        assert_eq!(extract_u32(two_types(true)), Some(1u32));
        assert_eq!(extract_u32(two_types(false)), None);
    }

    // Both branches use *literally the same* type-less `sumtype!(..)` args.
    #[sumtype(::announce_std::fmt::Debug)]
    fn identical_branches(b: bool) -> impl std::fmt::Debug + Downcast<u32> {
        if b {
            sumtype!(1u32)
        } else {
            sumtype!(1u32)
        }
    }

    // Branches differ but resolve to values of the same type.
    #[sumtype(::announce_std::fmt::Debug)]
    fn same_type_different_value(b: bool) -> impl std::fmt::Debug + Downcast<u32> {
        if b {
            sumtype!(10u32)
        } else {
            sumtype!(2 * 10u32)
        }
    }

    #[test]
    fn downcast_recovers_value_across_duplicate_typed_variants() {
        assert_eq!(Downcast::<u32>::downcast(identical_branches(true)).ok(), Some(1u32));
        assert_eq!(Downcast::<u32>::downcast(identical_branches(false)).ok(), Some(1u32));
        assert_eq!(Downcast::<u32>::downcast_ref(&identical_branches(false)), Some(&1u32));

        assert_eq!(Downcast::<u32>::downcast(same_type_different_value(true)).ok(), Some(10u32));
        assert_eq!(Downcast::<u32>::downcast(same_type_different_value(false)).ok(), Some(20u32));
    }
}

/// Reaching `Downcast` via the named `sumtype!()` return type (explicit `sumtype!(expr, Type)`
/// branches). A single function then exposes `Downcast<To>` for every wrapped type.
mod via_named_type {
    use super::*;

    #[sumtype(::announce_std::fmt::Debug)]
    fn make(which: u8) -> sumtype!() {
        match which {
            0 => sumtype!(1u32, u32),
            1 => sumtype!(String::from("hi"), String),
            _ => sumtype!(vec![1i64, 2, 3], Vec<i64>),
        }
    }

    #[test]
    fn downcast_ref_matches_active_variant_only() {
        let a = make(0);
        assert_eq!(Downcast::<u32>::downcast_ref(&a), Some(&1u32));
        assert_eq!(Downcast::<String>::downcast_ref(&a), None);
        assert_eq!(Downcast::<Vec<i64>>::downcast_ref(&a), None);

        let b = make(1);
        assert_eq!(Downcast::<String>::downcast_ref(&b), Some(&String::from("hi")));
        assert_eq!(Downcast::<u32>::downcast_ref(&b), None);

        let c = make(2);
        assert_eq!(Downcast::<Vec<i64>>::downcast_ref(&c), Some(&vec![1i64, 2, 3]));
    }

    #[test]
    fn downcast_mut_allows_in_place_edit() {
        let mut b = make(1);
        if let Some(s) = Downcast::<String>::downcast_mut(&mut b) {
            s.push('!');
        }
        assert_eq!(Downcast::<String>::downcast_ref(&b), Some(&String::from("hi!")));
        assert!(Downcast::<u32>::downcast_mut(&mut b).is_none());
    }

    #[test]
    fn downcast_owned_returns_value_or_self() {
        assert_eq!(Downcast::<u32>::downcast(make(0)).ok(), Some(1u32));
        assert_eq!(Downcast::<String>::downcast(make(1)).ok(), Some(String::from("hi")));

        let recovered = Downcast::<String>::downcast(make(0));
        assert!(recovered.is_err());
        let back = recovered.unwrap_err();
        assert_eq!(Downcast::<u32>::downcast_ref(&back), Some(&1u32));
    }

    fn extract_u8<E: Downcast<u8>>(e: E) -> Option<u8> {
        e.downcast().ok()
    }

    #[sumtype(::announce_std::fmt::Debug)]
    fn small(b: bool) -> sumtype!() {
        if b {
            sumtype!(10u8, u8)
        } else {
            sumtype!(20i16, i16)
        }
    }

    #[test]
    fn downcast_via_generic_bound() {
        assert_eq!(extract_u8(small(true)), Some(10u8));
        assert_eq!(extract_u8(small(false)), None);
    }

    // Duplicate-typed variants, named form: literally identical branches.
    #[sumtype(::announce_std::fmt::Debug)]
    fn identical_branches(b: bool) -> sumtype!() {
        if b {
            sumtype!(1u32, u32)
        } else {
            sumtype!(1u32, u32)
        }
    }

    // Distinct expressions resolving to the same type.
    #[sumtype(::announce_std::fmt::Debug)]
    fn same_type_different_value(b: bool) -> sumtype!() {
        if b {
            sumtype!(10u32, u32)
        } else {
            sumtype!(2 * 10u32, u32)
        }
    }

    // Two same-typed variants plus a distinct one.
    #[sumtype(::announce_std::fmt::Debug)]
    fn mixed(which: u8) -> sumtype!() {
        match which {
            0 => sumtype!(1u32, u32),
            1 => sumtype!(2u32, u32),
            _ => sumtype!(String::from("s"), String),
        }
    }

    #[test]
    fn downcast_recovers_value_across_duplicate_typed_variants() {
        assert_eq!(Downcast::<u32>::downcast(identical_branches(true)).ok(), Some(1u32));
        assert_eq!(Downcast::<u32>::downcast(identical_branches(false)).ok(), Some(1u32));

        assert_eq!(Downcast::<u32>::downcast(same_type_different_value(true)).ok(), Some(10u32));
        assert_eq!(Downcast::<u32>::downcast(same_type_different_value(false)).ok(), Some(20u32));
    }

    #[test]
    fn downcast_distinguishes_shared_and_distinct_variants() {
        assert_eq!(Downcast::<u32>::downcast(mixed(0)).ok(), Some(1u32));
        assert_eq!(Downcast::<u32>::downcast(mixed(1)).ok(), Some(2u32));
        assert_eq!(Downcast::<String>::downcast_ref(&mixed(2)), Some(&String::from("s")));
        assert_eq!(Downcast::<u32>::downcast_ref(&mixed(2)), None);
        assert_eq!(Downcast::<String>::downcast_ref(&mixed(0)), None);
    }
}
