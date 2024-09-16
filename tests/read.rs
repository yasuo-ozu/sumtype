use std::io::Read;
use sumtype::sumtype;

#[sumtype(sumtype::traits::Read)]
fn f1(a: bool) -> impl Read {
    if a {
        sumtype!(std::io::empty())
    } else {
        sumtype!(std::io::Cursor::new([1, 2, 3]))
    }
}
