use sumtype::announce::announce;

#[announce(crate = ::sumtype::announce, sumtype::check)]
pub trait SelfInOption {
    fn f(&self, v: ::core::option::Option<&Self>);
}

fn main() {}
