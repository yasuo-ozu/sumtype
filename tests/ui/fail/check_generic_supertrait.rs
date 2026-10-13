use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait GenericSuper: ::core::convert::AsRef<u8> {}

fn main() {}
