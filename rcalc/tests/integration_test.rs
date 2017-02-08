extern crate rcalc;

use std::rc::Rc;
use rcalc::{ Calculator, RuntimeItem, Value };

#[test]
fn test_atom() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc("1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
}

#[test]
fn test_it() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc(r"1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
    assert_eq!(calculator.calc(r"it + 1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
}

#[test]
fn test_term() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc("1 + 1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
}

#[test]
fn test_float() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc("(1 + 1 + 0.5) / 0.5"),
               Result::Ok(&RuntimeItem::Value(Value::Float(5.0))));
}

#[test]
fn test_complex_1() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc(r"sin (pi/2) \ 1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
}

#[test]
fn test_complex_2() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc(r"(0b0000_0001 << 7) & 0x80"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(128))));
}

#[test]
fn test_function_decl() {
    let mut calculator = Calculator::new();
    let src = "let f(x) = x**2";
    match calculator.calc(src) {
        Result::Ok(&RuntimeItem::Function(..)) => (),
        _ => assert!(false),
    }
}

#[test]
fn test_function_call() {
    let mut calculator = Calculator::new();
    let src = "let f(x) = x**2";
    match calculator.calc(src) {
        Result::Ok(&RuntimeItem::Function(..)) => (),
        _ => assert!(false),
    }
    assert_eq!(calculator.calc(r"f(2)"),
               Result::Ok(&RuntimeItem::Value(Value::Float(4.0))));
}

#[test]
fn test_binding_ref() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc("let a = 1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
    assert_eq!(calculator.calc("a + 1"),
               Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
}

#[test]
fn test_vectors() {
    let mut calculator = Calculator::new();
    assert_eq!(calculator.calc(r"[1, 2] + 1"),
               Result::Ok(&RuntimeItem::Value(Value::Vector(Rc::new(vec![Value::Integer(2), Value::Integer(3)])))));
}
