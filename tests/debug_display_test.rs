use sumtype::sumtype;
use std::fmt;

#[derive(Debug)]
struct TestStructA(i32);

impl fmt::Display for TestStructA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TestStructA({})", self.0)
    }
}

#[derive(Debug)]
struct TestStructB(String);

impl fmt::Display for TestStructB {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TestStructB({})", self.0)
    }
}

#[sumtype(::announce_std::fmt::Debug)]
fn get_debug(use_a: bool) -> impl std::fmt::Debug {
    if use_a {
        sumtype!(TestStructA(42))
    } else {
        sumtype!(TestStructB("hello".to_string()))
    }
}

#[sumtype(::announce_std::fmt::Display)]
fn get_display(use_a: bool) -> impl std::fmt::Display {
    if use_a {
        sumtype!(TestStructA(42))
    } else {
        sumtype!(TestStructB("hello".to_string()))
    }
}

#[test]
fn test_debug_trait() {
    let debug_a = get_debug(true);
    let debug_str = format!("{:?}", debug_a);
    assert!(debug_str.contains("TestStructA"));
    assert!(debug_str.contains("42"));

    let debug_b = get_debug(false);
    let debug_str = format!("{:?}", debug_b);
    assert!(debug_str.contains("TestStructB"));
    assert!(debug_str.contains("hello"));
}

#[test]
fn test_display_trait() {
    let display_a = get_display(true);
    assert_eq!(format!("{}", display_a), "TestStructA(42)");

    let display_b = get_display(false);
    assert_eq!(format!("{}", display_b), "TestStructB(hello)");
}

#[test]
fn test_mixed_primitives() {
    #[sumtype(::announce_std::fmt::Display)]
    fn get_mixed_display(use_int: bool) -> impl std::fmt::Display {
        if use_int {
            sumtype!(42i32)
        } else {
            sumtype!("hello world")
        }
    }

    let int_display = get_mixed_display(true);
    assert_eq!(format!("{}", int_display), "42");

    let str_display = get_mixed_display(false);
    assert_eq!(format!("{}", str_display), "hello world");
}