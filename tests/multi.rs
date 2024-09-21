use sumtype::sumtype;

#[sumtype(sumtype::traits::Iterator + sumtype::traits::Clone)]
fn f(a: usize) -> impl Iterator<Item = usize> + Clone {
    match a {
        0 => sumtype!(std::iter::empty::<usize>()),
        1 => sumtype!(std::iter::once(a)),
        _ => sumtype!(std::iter::repeat(a).take(a)),
    }
}
