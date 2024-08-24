use sumtype::sumtype;

#[sumtype]
fn generate_iter<'a, T>(t: &'a T, count: usize) -> impl Iterator<Item = &'a T> {
    match count {
        0 => sumtype!(std::iter::empty()),
        1 => sumtype!(std::iter::once(t)),
        n => sumtype!(std::iter::repeat(t).take(n)),
    }
}

#[sumtype]
fn generate_iter_explicit<'a, T: 'a>(t: &'a T, count: usize) -> sumtype!() {
    match count {
        0 => sumtype!(std::iter::empty(), std::iter::Empty<&'a T>),
        1 => sumtype!(std::iter::once(t), std::iter::Once<&'a T>),
        n => sumtype!(
            std::iter::repeat(t).take(n),
            std::iter::Take<std::iter::Repeat<&'a T>>
        ),
    }
}

#[test]
fn test1() {
    assert_eq!(
        generate_iter(&123usize, 0).collect::<Vec<_>>(),
        vec![] as Vec<&usize>
    );
    assert_eq!(generate_iter(&123usize, 1).collect::<Vec<_>>(), vec![&123]);
    assert_eq!(
        generate_iter(&123usize, 2).collect::<Vec<_>>(),
        vec![&123, &123]
    );
}

#[test]
fn test2() {
    assert_eq!(
        generate_iter_explicit(&123usize, 0).collect::<Vec<_>>(),
        vec![] as Vec<&usize>
    );
    assert_eq!(
        generate_iter_explicit(&123usize, 1).collect::<Vec<_>>(),
        vec![&123]
    );
    assert_eq!(
        generate_iter_explicit(&123usize, 2).collect::<Vec<_>>(),
        vec![&123, &123]
    );
}
