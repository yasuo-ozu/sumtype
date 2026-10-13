use sumtype::sumtype;
use std::fmt;
use std::error::Error;

// Define custom error types for testing
#[derive(Debug)]
struct IoError(String);

impl fmt::Display for IoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IO Error: {}", self.0)
    }
}

impl std::error::Error for IoError {}

#[derive(Debug)]
struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parse Error: {}", self.0)
    }
}

impl std::error::Error for ParseError {}

#[derive(Debug)]
struct NetworkError {
    code: u32,
    message: String,
}

impl fmt::Display for NetworkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Network Error {}: {}", self.code, self.message)
    }
}

impl std::error::Error for NetworkError {}

// Test function that returns different error types
#[sumtype(::announce_std::error::Error)]
fn get_error(error_type: &str) -> impl std::error::Error {
    match error_type {
        "io" => sumtype!(IoError("Failed to read file".to_string())),
        "parse" => sumtype!(ParseError("Invalid JSON format".to_string())),
        "network" => sumtype!(NetworkError { 
            code: 404, 
            message: "Not Found".to_string() 
        }),
        _ => sumtype!(IoError("Unknown error".to_string())),
    }
}

// Test function that can be used where std::error::Error is expected
fn handle_error(e: impl std::error::Error) -> String {
    format!("Handled: {}", e)
}

// Test function that works with Error + Send + Sync bounds
#[allow(dead_code)]
fn handle_error_threadsafe(e: impl std::error::Error + Send + Sync) -> String {
    format!("Thread-safe handled: {}", e)
}

#[test]
fn test_error_display() {
    let io_err = get_error("io");
    assert_eq!(format!("{}", io_err), "IO Error: Failed to read file");
    
    let parse_err = get_error("parse");
    assert_eq!(format!("{}", parse_err), "Parse Error: Invalid JSON format");
    
    let network_err = get_error("network");
    assert_eq!(format!("{}", network_err), "Network Error 404: Not Found");
}

#[test]
fn test_error_debug() {
    let io_err = get_error("io");
    let debug_str = format!("{:?}", io_err);
    assert!(debug_str.contains("IoError"));
    
    let parse_err = get_error("parse");
    let debug_str = format!("{:?}", parse_err);
    assert!(debug_str.contains("ParseError"));
}

#[test]
fn test_error_handling() {
    let io_err = get_error("io");
    let result = handle_error(io_err);
    assert_eq!(result, "Handled: IO Error: Failed to read file");
    
    let parse_err = get_error("parse");
    let result = handle_error(parse_err);
    assert_eq!(result, "Handled: Parse Error: Invalid JSON format");
}

#[test]
fn test_error_source() {
    let io_err = get_error("io");
    // Test that source() method is available
    assert!(io_err.source().is_none());
    
    let parse_err = get_error("parse");
    assert!(parse_err.source().is_none());
}

#[test]
fn test_error_as_trait_object() {
    let error: Box<dyn std::error::Error> = Box::new(get_error("io"));
    assert_eq!(format!("{}", error), "IO Error: Failed to read file");
}

// Test with generic function that accepts Error trait
fn process_result<T, E: std::error::Error>(result: Result<T, E>) -> String {
    match result {
        Ok(_) => "Success".to_string(),
        Err(e) => format!("Error occurred: {}", e),
    }
}

#[test]
fn test_error_in_result() {
    let err = get_error("network");
    let result: Result<(), _> = Err(err);
    let processed = process_result(result);
    assert_eq!(processed, "Error occurred: Network Error 404: Not Found");
}

// Test with explicit type annotation
#[sumtype(::announce_std::error::Error)]
fn get_typed_error() -> sumtype!() {
    sumtype!(IoError("Explicit type".to_string()), IoError)
}

#[test]
fn test_explicit_type() {
    let err = get_typed_error();
    assert_eq!(format!("{}", err), "IO Error: Explicit type");
}