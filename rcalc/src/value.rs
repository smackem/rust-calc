use std::ops::*;

/// Defines the possible types of values to process.
///
/// # Arithmetic Conversion Rules
///
/// * `Integer` *op* `Integer` = `Integer`
///   `Integer` *op* `Float` = `Float`
///   `Float` *op* `Float` = `Float`
///   for *op* in `+` `-` `*` `%`
/// * `Integer` / `Integer` = `Float`
///   `Integer` / `Float` = `Float`
///   `Float` / `Float` = `Float`
/// * `Integer` \ `Integer` = `Integer`
///   `Integer` \ `Float` = `Integer`
///   `Float` \ `Float` = `Integer`
#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
}

impl Value {
    /// Returns the contained value as `f64` with possible loss of precision for very large values.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(12).to_float(), Value::Float(12.0))
    /// ```
    pub fn to_float(&self) -> f64 {
        match *self {
            Value::Integer(n) => n as f64,
            Value::Float(f) => f,
        }
    }

    /// Returns the contained value as `i64`, truncating the fractional part if the contained
    /// value is a `Float`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Float(12.3).to_integer(), Value::Integer(12))
    /// ```
    pub fn to_integer(&self) -> i64 {
        match *self {
            Value::Integer(n) => n,
            Value::Float(f) => f as i64,
        }
    }

    /// Converts `self` to integer, divides it by `other` (also converted to integer) and
    /// returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Float(12.3).integer_divide_by(&Value::Integer(3)), Value::Integer(4))
    /// ```
    pub fn integer_divide_by(&self, other: &Value) -> Value {
        Value::Integer(self.to_integer() / other.to_integer())
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (left, right) => Value::Float(left.to_float() + right.to_float()),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (left, right) => Value::Float(left.to_float() - right.to_float()),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (left, right) => Value::Float(left.to_float() * right.to_float()),
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        Value::Float(self.to_float() / other.to_float())
    }
}

impl Rem for Value {
    type Output = Value;

    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
            (left, right) => Value::Float(left.to_float() % right.to_float()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(Value::Integer(2) + Value::Integer(3), Value::Integer(5));
        assert_eq!(Value::Integer(2) + Value::Float(3.0), Value::Float(5.0));
        assert_eq!(Value::Float(2.0) + Value::Integer(3), Value::Float(5.0));
        assert_eq!(Value::Float(2.0) + Value::Float(3.0), Value::Float(5.0));
    }

    #[test]
    fn test_sub() {
        assert_eq!(Value::Integer(2) - Value::Integer(3), Value::Integer(-1));
        assert_eq!(Value::Integer(2) - Value::Float(3.0), Value::Float(-1.0));
        assert_eq!(Value::Float(2.0) - Value::Integer(3), Value::Float(-1.0));
        assert_eq!(Value::Float(2.0) - Value::Float(3.0), Value::Float(-1.0));
    }

    #[test]
    fn test_mul() {
        assert_eq!(Value::Integer(2) * Value::Integer(3), Value::Integer(6));
        assert_eq!(Value::Integer(2) * Value::Float(3.0), Value::Float(6.0));
        assert_eq!(Value::Float(2.0) * Value::Integer(3), Value::Float(6.0));
        assert_eq!(Value::Float(2.0) * Value::Float(3.0), Value::Float(6.0));
    }

    #[test]
    fn test_div() {
        assert_eq!(Value::Integer(6) / Value::Integer(3), Value::Float(2.0));
        assert_eq!(Value::Integer(6) / Value::Float(3.0), Value::Float(2.0));
        assert_eq!(Value::Float(6.0) / Value::Integer(3), Value::Float(2.0));
        assert_eq!(Value::Float(6.0) / Value::Float(3.0), Value::Float(2.0));
    }

    #[test]
    fn test_integer_div() {
        assert_eq!(Value::Integer(7).integer_divide_by(&Value::Integer(3)), Value::Integer(2));
        assert_eq!(Value::Integer(7).integer_divide_by(&Value::Float(3.0)), Value::Integer(2));
        assert_eq!(Value::Float(7.0).integer_divide_by(&Value::Integer(3)), Value::Integer(2));
        assert_eq!(Value::Float(7.0).integer_divide_by(&Value::Float(3.0)), Value::Integer(2));
    }

    #[test]
    fn test_rem() {
        assert_eq!(Value::Integer(7) % Value::Integer(3), Value::Integer(1));
        assert_eq!(Value::Integer(7) % Value::Float(3.0), Value::Float(1.0));
        assert_eq!(Value::Float(7.0) % Value::Integer(3), Value::Float(1.0));
        assert_eq!(Value::Float(7.0) % Value::Float(3.0), Value::Float(1.0));
    }
}
