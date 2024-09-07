use sumtype::sumtype;

#[allow(unused)]
#[sumtype]
fn with_generics<'a, T>(t: &'a T, count: usize) -> sumtype!() {
    match count {
        0 => sumtype!(std::iter::empty(), std::iter::Empty<&'a T>),
        1 => sumtype!(std::iter::once(t), std::iter::Once<&'a T>),
        n => sumtype!(
            std::iter::repeat(t).take(n),
            std::iter::Take<std::iter::Repeat<&'a T>>
        ),
    }
}
