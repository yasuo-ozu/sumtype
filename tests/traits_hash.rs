//! `sumtype::traits::Hash` as a direct target trait. The generated sum type implements
//! `std::hash::Hash` by forwarding `hash` to the active variant.

use std::hash::{Hash, Hasher};
use sumtype::sumtype;

fn hash_of<T: Hash>(t: &T) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    t.hash(&mut h);
    h.finish()
}

#[sumtype(sumtype::traits::Hash)]
fn make(b: bool) -> impl Hash {
    if b {
        sumtype!(1u32)
    } else {
        sumtype!(2u64)
    }
}

#[test]
fn hash_forwards_to_active_variant() {
    assert_eq!(hash_of(&make(true)), hash_of(&1u32));
    assert_eq!(hash_of(&make(false)), hash_of(&2u64));
    assert_ne!(hash_of(&make(true)), hash_of(&make(false)));
}
