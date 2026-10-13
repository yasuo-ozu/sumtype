use sumtype::announce::announce;

pub trait Base {}

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait Sub: Base {}

fn main() {}
