use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait NoReceiver {
    fn new() -> u8;
}

fn main() {}
