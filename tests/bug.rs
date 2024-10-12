use sumtype::sumtype;

struct Flatten<I, Iter> {
    slot: I,
    iter: Option<Iter>,
}

impl<I, OuterItem> Flatten<I, OuterItem> {
    #[allow(unused)]
    fn new(slot: I) -> Self {
        Flatten { slot, iter: None }
    }
}

impl<I, Iter, T> Iterator for Flatten<I, Iter>
where
    I: Iterator,
    <I as Iterator>::Item: IntoIterator<IntoIter = Iter, Item = T>,
    Iter: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(iter) = &mut self.iter {
            if let Some(item) = iter.next() {
                return Some(item);
            }
        }
        self.iter = Some(self.slot.next()?.into_iter());
        self.next()
    }
}
#[allow(unused)]
enum E<T> {
    E0(S<T>),
    E1,
}

trait Parametrized<const PARAM: usize> {
    type Item;
    type Iter<'a>: Iterator<Item = &'a Self::Item>
    where
        (Self, Self::Item): 'a;

    #[allow(unused)]
    fn param_iter<'a>(&'a self) -> Self::Iter<'a>
    where
        Self::Item: 'a;
}

struct S<T>(T);

impl<T> Parametrized<0> for S<T> {
    type Item = T;
    type Iter<'a> = std::iter::Once<&'a T> where T: 'a;
    fn param_iter<'a>(&'a self) -> Self::Iter<'a>
    where
        Self::Item: 'a {todo!()}

}

#[sumtype(sumtype::traits::Iterator)]
impl<T> Parametrized<0usize> for E<T> {
    type Item = T;
    type Iter <'__parametrized_lt > = sumtype! ['__parametrized_lt] where
    (Self, Self :: Item) : '__parametrized_lt;
    fn param_iter<'__parametrized_lt>(&'__parametrized_lt self) -> Self::Iter<'__parametrized_lt>
    where
        Self::Item: '__parametrized_lt,
    {
        #[allow(unused)]
        match self {
            E::E0(__parametric_type_id_0) => {
                sumtype!
                ({
                    let __parametrized_fn : fn(& '__parametrized_lt T) -> _ = |
                    __parametrized_arg |
                    { :: core :: iter :: once(__parametrized_arg) }; 
                    Flatten :: new(< S < T > as Parametrized < 0usize >
                    > ::
                    param_iter(__parametric_type_id_0).map(__parametrized_fn))
                }, for <'__parametrized_lt > Flatten < ::
                core :: iter :: Map < < S < T > as 
                Parametrized < 0usize > > :: Iter < '__parametrized_lt > ,
                fn(& '__parametrized_lt T) -> :: core :: iter :: Once < &
                '__parametrized_lt T > > , :: core :: iter :: Once < &
                '__parametrized_lt T > > where T : '__parametrized_lt, S < T >
                : Parametrized < 0usize > )
            }
            E::E1 => {
                sumtype!
                (:: core :: iter :: empty(), for <'__parametrized_lt > :: core
                :: iter :: Empty < & '__parametrized_lt T > where T :
                '__parametrized_lt, S < T > : Parametrized
                < 0usize > )
            }
        }
    }
}
