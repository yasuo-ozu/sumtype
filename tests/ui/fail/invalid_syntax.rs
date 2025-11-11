use sumtype::sumtype;

#[sumtype(sumtype::traits::Iterator)]
fn invalid_syntax() -> impl Iterator<Item = u32> {
    // Invalid sumtype! syntax - missing expression
    sumtype!()
}

fn main() {}