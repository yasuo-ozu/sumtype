use sumtype::sumtype;

#[allow(unused)]
trait MyTrait {
    type Ty<'a, T>
    where
        T: 'a;

    fn f<'a, T>(i: usize, _: &'a T) -> Self::Ty<'a, T>;
}

#[sumtype]
impl MyTrait for () {
    type Ty<'a, T> = sumtype!['a, T] where T: 'a;
    fn f<'a, T>(i: usize, t: &'a T) -> Self::Ty<'a, T> {
        if i == 0 {
            sumtype!(std::iter::empty(), for<'a, T: 'a> std::iter::Empty<&'a T>)
        } else {
            sumtype!(
                std::iter::repeat(t).take(i),
                for<'a, T: 'a> std::iter::Take<std::iter::Repeat<&'a T>>
            )
        }
    }
}
