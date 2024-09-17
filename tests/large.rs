use sumtype::sumtype;

#[allow(unused)]
trait MyTrait {
    type Ty<'a, T>
    where
        T: 'a;

    fn f<'a, T>(i: usize, _: &'a T) -> Self::Ty<'a, T>;
}

// #[sumtype(sumtype::traits::Iterator)]
// impl MyTrait for () {
//     type Ty<'a, T> = sumtype!['a, T] where T: 'a;
//     fn f<'a, T>(i: usize, t: &'a T) -> Self::Ty<'a, T> {
//         if i == 0 {
//             sumtype!(std::iter::empty(), for<'a, T: 'a> std::iter::Empty<&'a T>)
//         } else {
//             sumtype!(
//                 std::iter::repeat(t).take(i),
//                 for<'a, T: 'a> std::iter::Take<std::iter::Repeat<&'a T>>
//             )
//         }
//     }
// }
#[allow(unused)]
trait MyTrait2<T> {
    type Ty<'a>
    where
        T: 'a;

    fn f<'a>(i: usize, _: &'a T) -> Self::Ty<'a>;
}

#[sumtype(sumtype::traits::Iterator)]
impl<T> MyTrait2<T> for () {
    type Ty<'a> = sumtype!['a] where T: 'a;
    fn f<'a>(i: usize, t: &'a T) -> Self::Ty<'a> {
        if i == 0 {
            sumtype!(std::iter::empty(), for<'a> std::iter::Empty<&'a T> where T: 'a)
        } else {
            sumtype!(
                std::iter::repeat(t).take(i),
                for<'a> std::iter::Take<std::iter::Repeat<&'a T>>
                where T: 'a
            )
        }
    }
}
