use sumtype::sumtype;
#[sumtype(::announce_std::iter::Iterator)]
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

        pub fn collect_vals(self) -> Vec<u32> {
            self.iter.collect()
        }
    }
}

// Runtime assertion for the `#[sumtype] mod ...` expansion path (is_module=true; generated items
// hoisted out of the module, `super::`-prefixed enum/typeref paths). Previously compile-only.
#[test]
fn module_expansion_dispatches_correctly() {
    assert_eq!(my_module::MyStruct::new(true).collect_vals(), vec![0, 1, 2, 3, 4]);
    assert_eq!(my_module::MyStruct::new(false).collect_vals(), vec![10, 20, 30]);
}
