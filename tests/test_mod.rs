use sumtype::sumtype;
#[sumtype(sumtype::traits::Iterator)]
mod my_module {
    #[allow(unused)]
    pub struct MyStruct {
        iter: sumtype!(),
    }

    impl MyStruct {
        #[allow(unused)]
        pub fn new(flag: bool) -> Self {
            let iter = if flag {
                sumtype!(0..5, std::ops::Range<u32>) // Wraps a range iterator
            } else {
                sumtype!(vec![10, 20, 30].into_iter(), std::vec::IntoIter<u32>) // Wraps a vector iterator
            };
            MyStruct { iter }
        }

        #[allow(unused)]
        pub fn iterate(self) {
            for value in self.iter {
                println!("{}", value);
            }
        }
    }
}
