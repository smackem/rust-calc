use std::ops::*;

/// Defines the possible types of values to process.
/// Supports `Clone`, `Copy` and `PartialEq` semantics (*value type*).
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
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Vector(Box<Vec<Value>>),
}

impl Value {
    /// Converts `self` to integer, divides it by `other` (also converted to integer) and
    /// returns the result.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Float(12.3).integer_divide_by(&Value::Integer(3)), Value::Integer(4));
    /// ```
    pub fn integer_divide_by(&self, other: &Value) -> Value {
        match (self, other) {
            (&Value::Integer(l), &Value::Integer(r)) => Value::Integer(l / r),
            (ref lv, ref rv) => Value::Integer(lv.to_integer() / rv.to_integer()),
        }
    }

    pub fn pow(&self, exponent: &Value) -> Value {
        Value::Float(self.to_float().powf(exponent.to_float()))
    }

    pub fn log(&self, base: &Value) -> Value {
        Value::Float(self.to_float().log(base.to_float()))
    }

    pub fn sqrt(&self) -> Value {
        Value::Float(self.to_float().sqrt())
    }

    pub fn sin(&self) -> Value {
        Value::Float(self.to_float().sin())
    }

    pub fn cos(&self) -> Value {
        Value::Float(self.to_float().cos())
    }

    pub fn tan(&self) -> Value {
        Value::Float(self.to_float().tan())
    }

    pub fn asin(&self) -> Value {
        Value::Float(self.to_float().asin())
    }

    pub fn acos(&self) -> Value {
        Value::Float(self.to_float().acos())
    }

    pub fn atan(&self) -> Value {
        Value::Float(self.to_float().atan())
    }
}

impl Add for Value {
    type Output = Value;

    /// Adds to `Value`s.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) + Value::Integer(2), Value::Integer(3));
    /// ```
    fn add(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a + b),
            (left, right) => Value::Float(left.to_float() + right.to_float()),
        }
    }
}

impl Sub for Value {
    type Output = Value;

    /// Subtracts `Value` `other` from `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) - Value::Integer(2), Value::Integer(-1));
    /// ```
    fn sub(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a - b),
            (left, right) => Value::Float(left.to_float() - right.to_float()),
        }
    }
}

impl BitAnd for Value {
    type Output = Value;

    /// Caclulates `Value` `other` & `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) & Value::Integer(3), Value::Integer(1));
    /// ```
    fn bitand(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a & b),
            (left, right) => Value::Integer(left.to_integer() & right.to_integer()),
        }
    }
}

impl BitOr for Value {
    type Output = Value;

    /// Caclulates `Value` `other` | `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) | Value::Integer(2), Value::Integer(3));
    /// ```
    fn bitor(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a | b),
            (left, right) => Value::Integer(left.to_integer() | right.to_integer()),
        }
    }
}

impl BitXor for Value {
    type Output = Value;

    /// Caclulates `Value` `other` ^ `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) ^ Value::Integer(3), Value::Integer(2));
    /// ```
    fn bitxor(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a ^ b),
            (left, right) => Value::Integer(left.to_integer() ^ right.to_integer()),
        }
    }
}

impl Mul for Value {
    type Output = Value;

    /// Multiplies two `Value`s
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(2) * Value::Integer(3), Value::Integer(6));
    /// ```
    fn mul(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a * b),
            (left, right) => Value::Float(left.to_float() * right.to_float()),
        }
    }
}

impl Div for Value {
    type Output = Value;

    /// Divides `Value` `self` by `other`, always returning a `Float`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(6) / Value::Integer(2), Value::Float(3.0));
    /// ```
    fn div(self, other: Value) -> Value {
        Value::Float(self.to_float() / other.to_float())
    }
}

impl Rem for Value {
    type Output = Value;

    /// Divides `Value` `self` by `other`, returning the remainder.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(5) / Value::Integer(2), Value::Integer(1));
    /// ```
    fn rem(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a % b),
            (left, right) => Value::Float(left.to_float() % right.to_float()),
        }
    }
}

impl Shl<Value> for Value {
    type Output = Value;

    /// Shifts `Value` `self` left by `other` bits.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(1) << Value::Integer(2), Value::Integer(4));
    /// ```
    fn shl(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a << b),
            (left, right) => Value::Integer(left.to_integer() << right.to_integer()),
        }
    }
}

impl Shr<Value> for Value {
    type Output = Value;

    /// Shifts `Value` `self` right by `other` bits.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(8) >> Value::Integer(3), Value::Integer(1));
    /// ```
    fn shr(self, other: Value) -> Value {
        match (self, other) {
            (Value::Integer(a), Value::Integer(b)) => Value::Integer(a >> b),
            (left, right) => Value::Integer(left.to_integer() >> right.to_integer()),
        }
    }
}

impl Neg for Value {
    type Output = Value;

    /// The method for the unary `-` operator.
    /// Negates `Value` `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(-Value::Integer(10), Value::Integer(-10));
    /// ```
    fn neg(self) -> Value {
        match self {
            Value::Integer(n) => Value::Integer(-n),
            Value::Float(f) => Value::Float(-f),
        }
    }
}

// ===========================================================================

impl Value {
    /// Returns the contained value as `f64` with possible loss of precision for very large values.
    ///
    /// # Examples
    ///
    /// ```
    /// assert_eq!(Value::Integer(12).to_float(), Value::Float(12.0));
    /// ```
    fn to_float(&self) -> f64 {
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
    /// assert_eq!(Value::Float(12.3).to_integer(), Value::Integer(12));
    /// ```
    fn to_integer(&self) -> i64 {
        match *self {
            Value::Integer(n) => n,
            Value::Float(f) => f as i64,
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
    
    #[test]
    fn test_neg() {
        assert_eq!(-Value::Integer(10), Value::Integer(-10));
        assert_eq!(-Value::Float(-10.25), Value::Float(10.25));
    }
}
