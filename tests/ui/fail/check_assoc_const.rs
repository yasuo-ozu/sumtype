use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait HasConst {
    const N: usize;
}

fn main() {}
