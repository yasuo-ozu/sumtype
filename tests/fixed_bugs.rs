//! Regression tests for bugs found in the audit and since fixed. Each used to fail to compile;
//! now it compiles and produces the correct result.

use sumtype::sumtype;

// Was: const generics produced `PhantomData<>` in the `__Uninhabited` variant (E0107).
#[sumtype(sumtype::traits::Iterator)]
fn const_generic<const N: usize>(b: bool) -> impl Iterator<Item = usize> {
    if b {
        sumtype!(0..N)
    } else {
        sumtype!(std::iter::once(N))
    }
}

// Was: a `for<'a, T>` that repeats the enclosing free function's own generics produced duplicate
// parameter names (E0403) and a double-counted enum instantiation (E0107).
#[sumtype(sumtype::traits::Iterator)]
fn repeated_generics<'a, T: 'a>(t: &'a T, b: bool) -> sumtype!['a, T] {
    if b {
        sumtype!(std::iter::once(t), for<'a, T: 'a> std::iter::Once<&'a T>)
    } else {
        sumtype!(std::iter::empty(), for<'a, T: 'a> std::iter::Empty<&'a T>)
    }
}

#[test]
fn const_generics_work() {
    assert_eq!(const_generic::<3>(true).collect::<Vec<_>>(), vec![0, 1, 2]);
    assert_eq!(const_generic::<3>(false).collect::<Vec<_>>(), vec![3]);
}

#[test]
fn for_generics_repeating_fn_generics_work() {
    let x = 7u32;
    assert_eq!(repeated_generics(&x, true).collect::<Vec<_>>(), vec![&7]);
    assert_eq!(
        repeated_generics(&x, false).collect::<Vec<_>>(),
        Vec::<&u32>::new()
    );
}

// Coverage: const generic combined with a lifetime on the enclosing item (impl-Trait return form).
#[sumtype(sumtype::traits::Iterator)]
fn const_and_lifetime<'a, const N: usize>(x: &'a u32, b: bool) -> impl Iterator<Item = u32> + 'a {
    if b {
        sumtype!(std::iter::repeat(*x).take(N))
    } else {
        sumtype!(std::iter::empty())
    }
}

#[test]
fn const_plus_lifetime_generics_work() {
    let v = 5u32;
    assert_eq!(const_and_lifetime::<3>(&v, true).collect::<Vec<_>>(), vec![5, 5, 5]);
    assert_eq!(const_and_lifetime::<3>(&v, false).count(), 0);
}

// Coverage: a 3-bound `#[sumtype(A + B + C)]` (the per-bound constraint-trait/dispatch loop with N>2).
#[sumtype(sumtype::traits::Iterator + sumtype::traits::Clone + sumtype::traits::Debug)]
fn three_bounds(a: u8) -> impl Iterator<Item = u8> + Clone + std::fmt::Debug {
    match a {
        0 => sumtype!(std::iter::empty::<u8>()),
        1 => sumtype!(std::iter::once(7u8)),
        _ => sumtype!(vec![1u8, 2, 3].into_iter()),
    }
}

#[test]
fn three_bound_sumtype_works() {
    let it = three_bounds(2);
    // Debug + Clone + Iterator all available on the one sum type.
    assert!(!format!("{:?}", it.clone()).is_empty());
    assert_eq!(it.collect::<Vec<_>>(), vec![1, 2, 3]);
    assert_eq!(three_bounds(1).collect::<Vec<_>>(), vec![7]);
}
