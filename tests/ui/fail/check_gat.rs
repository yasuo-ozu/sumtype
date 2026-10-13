use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait Gat {
    type Item<'a>
    where
        Self: 'a;
}

fn main() {}
