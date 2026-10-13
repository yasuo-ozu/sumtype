use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait LifetimeSuper: 'static {}

fn main() {}
