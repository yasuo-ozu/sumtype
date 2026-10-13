use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait SelfTwice {
    fn f(&self, other: &Self);
}

fn main() {}
