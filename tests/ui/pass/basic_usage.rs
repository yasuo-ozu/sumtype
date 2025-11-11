use sumtype::sumtype;

#[sumtype(sumtype::traits::Iterator)]
fn iter_example(flag: bool) -> impl Iterator<Item = u32> {
    if flag {
        sumtype!(std::iter::once(42))
    } else {
        sumtype!(vec![1, 2, 3].into_iter())
    }
}

#[sumtype(sumtype::traits::Iterator)]
fn explicit_types(flag: bool) -> sumtype!() {
    if flag {
        sumtype!(std::iter::once(42), std::iter::Once<u32>)
    } else {
        sumtype!(vec![1, 2, 3].into_iter(), std::vec::IntoIter<u32>)
    }
}

fn main() {
    let iter1 = iter_example(true);
    let iter2 = explicit_types(false);
    
    assert_eq!(iter1.collect::<Vec<_>>(), vec![42]);
    assert_eq!(iter2.collect::<Vec<_>>(), vec![1, 2, 3]);
}