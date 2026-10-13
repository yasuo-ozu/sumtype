use sumtype::sumtype;

#[sumtype(::announce_std::iter::Iterator)]
fn invalid_syntax() -> impl Iterator<Item = u32> {
    // Invalid sumtype! syntax - missing expression
    sumtype!()
}

fn main() {}