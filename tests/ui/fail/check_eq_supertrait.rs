use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait MyEq: ::core::cmp::Eq {}

fn main() {}
