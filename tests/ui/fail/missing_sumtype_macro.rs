use sumtype::sumtype;

#[sumtype(::announce_std::iter::Iterator)]
fn missing_macro(flag: bool) -> impl Iterator<Item = u32> {
    // Missing sumtype!() macro should cause compilation error
    if flag {
        std::iter::once(42)
    } else {
        vec![1, 2, 3].into_iter()
    }
}

fn main() {}