use sumtype::sumtype;

#[sumtype(sumtype::traits::Iterator)]
fn generic_mismatch(flag: bool) -> sumtype!['a] {
    if flag {
        // Missing lifetime parameter 'a
        sumtype!(std::iter::once(42), std::iter::Once<u32>)
    } else {
        sumtype!(vec![1, 2, 3].into_iter(), std::vec::IntoIter<u32>)
    }
}

fn main() {}