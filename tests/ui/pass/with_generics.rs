use sumtype::sumtype;

struct S;

#[sumtype(::announce_std::iter::Iterator)]
impl S {
    fn with_lifetime<'a, T>(t: &'a T, count: usize) -> sumtype!['a, T] {
        if count == 0 {
            sumtype!(std::iter::empty(), for<'a, T: 'a> std::iter::Empty<&'a T>)
        } else {
            sumtype!(
                std::iter::repeat(t).take(count),
                for<'a, T: 'a> std::iter::Take<std::iter::Repeat<&'a T>>
            )
        }
    }
}

#[sumtype(::announce_std::iter::Iterator)]
fn generic_fn<T: Clone>(value: T, repeat: bool) -> impl Iterator<Item = T> {
    if repeat {
        sumtype!(std::iter::repeat(value).take(3))
    } else {
        sumtype!(std::iter::once(value))
    }
}

fn main() {
    let value = 42u32;
    let iter1 = S::with_lifetime(&value, 0);
    let iter2 = S::with_lifetime(&value, 2);
    
    assert_eq!(iter1.collect::<Vec<_>>(), vec![] as Vec<&u32>);
    assert_eq!(iter2.collect::<Vec<_>>(), vec![&42, &42]);
    
    let iter3 = generic_fn(String::from("hello"), true);
    let iter4 = generic_fn(String::from("world"), false);
    
    assert_eq!(iter3.collect::<Vec<_>>(), vec!["hello"; 3]);
    assert_eq!(iter4.collect::<Vec<_>>(), vec!["world"]);
}