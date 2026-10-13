#![allow(clippy::manual_repeat_n)]
use sumtype::sumtype;

#[allow(unused)]
#[sumtype(::announce_std::iter::Iterator + ::announce_std::clone::Clone)]
fn f(a: usize) -> impl Iterator<Item = usize> + Clone {
    match a {
        0 => sumtype!(std::iter::empty::<usize>()),
        1 => sumtype!(std::iter::once(a)),
        _ => sumtype!(std::iter::repeat(a).take(a)),
    }
}
