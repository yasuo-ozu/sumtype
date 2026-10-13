//! Runtime behavioral coverage added by the audit.
//!
//! Before this file, most integration tests were *compile-only*: they assert that macro output
//! compiles but never check that the generated trait dispatch is correct at runtime. There was no
//! assertion that the generated `Read` reads the right bytes from the right variant, that `Clone`
//! produces an independent equal value, that the `Iterator + Clone` enum clones into an
//! independent iterator, or that `Copy` actually makes the value `Copy`. These tests lock that in.

use std::io::Read;
use sumtype::sumtype;

#[sumtype(::announce_std::io::Read)]
fn make_reader(which: u8) -> impl Read {
    match which {
        0 => sumtype!(std::io::Cursor::new(vec![1u8, 2, 3, 4])),
        1 => sumtype!(std::io::Cursor::new(&b"hello"[..])),
        _ => sumtype!(std::io::empty()),
    }
}

#[sumtype(::announce_std::iter::Iterator + ::announce_std::clone::Clone)]
fn make_iter(a: usize) -> impl Iterator<Item = usize> + Clone {
    match a {
        0 => sumtype!(std::iter::empty::<usize>()),
        1 => sumtype!(std::iter::once(7usize)),
        _ => sumtype!(std::iter::repeat(9usize).take(3)),
    }
}

#[sumtype(::announce_std::marker::Copy)]
fn make_copy(b: bool) -> impl Copy {
    if b {
        sumtype!(1i8)
    } else {
        sumtype!(2i32)
    }
}

#[test]
fn read_dispatches_and_reads_correct_bytes() {
    // Three structurally different concrete types (Cursor<Vec<u8>>, Cursor<&[u8]>, Empty)
    // unified through the sum type; assert each reads exactly its own bytes.
    let mut buf = Vec::new();
    make_reader(0).read_to_end(&mut buf).unwrap();
    assert_eq!(buf, vec![1, 2, 3, 4]);

    let mut s = String::new();
    make_reader(1).read_to_string(&mut s).unwrap();
    assert_eq!(s, "hello");

    let mut e = Vec::new();
    make_reader(2).read_to_end(&mut e).unwrap();
    assert_eq!(e, Vec::<u8>::new());
}

#[test]
fn clone_yields_independent_equal_iterator() {
    // The whole point of the Iterator + Clone combo: cloning yields an independent iterator
    // producing the same sequence.
    let it = make_iter(2);
    let cloned: Vec<_> = it.clone().collect();
    let original: Vec<_> = it.collect();
    assert_eq!(cloned, vec![9, 9, 9]);
    assert_eq!(cloned, original);

    // Advancing the clone must not advance the original.
    let it2 = make_iter(1);
    let mut c = it2.clone();
    assert_eq!(c.next(), Some(7));
    assert_eq!(it2.collect::<Vec<_>>(), vec![7]);

    assert_eq!(make_iter(0).collect::<Vec<_>>(), Vec::<usize>::new());
}

#[test]
fn copy_value_is_usable_after_move() {
    // Compiles only if the generated enum is `Copy`: `x` is still usable after `let y = x`.
    let x = make_copy(true);
    let y = x;
    let z = x;
    let _ = (y, z);
}
