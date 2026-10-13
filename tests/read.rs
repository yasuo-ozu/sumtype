use std::io::Read;
use sumtype::sumtype;

#[allow(unused)]
#[sumtype(::announce_std::io::Read)]
fn f1(a: bool) -> impl Read {
    if a {
        sumtype!(std::io::empty())
    } else {
        sumtype!(std::io::Cursor::new([1, 2, 3]))
    }
}
