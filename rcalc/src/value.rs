use std::ops::*;
use std::rc::Rc;
use std::cmp::Ordering;
use util::Boxable;

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
/// * `[a1,a2]` *op* `Integer` = `[a1*Integer, a2*Integer]
/// * `[a1,a2]` *op* `[b1,b2]` = `[a1*b1, a2*b2]`
#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Vector(Rc<Vec<Value>>),
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
        self.apply_binary_op(other, &|l, r| Value::Integer(l.to_integer() / r.to_integer()))
    }

    pub fn concat(&self, other: &Value) -> Value {
        let mut lv = (*self.to_vector()).clone();
        let mut rv = (*other.to_vector()).clone();
        lv.append(&mut rv);
        Value::Vector(lv.rc())
    }

    pub fn pow(&self, exponent: &Value) -> Value {
        self.apply_binary_op(exponent, &|l, r| Value::Float(l.to_float().powf(r.to_float()))) 
    }

    pub fn log(&self, base: &Value) -> Value {
        self.apply_binary_op(base, &|l, r| Value::Float(l.to_float().log(r.to_float()))) 
    }

    pub fn dot_product(&self, other: &Value) -> Value {
        let lv = self.to_vector();
        let rv = other.to_vector();
        let sum: f64 = (*lv).iter()
            .zip((*rv).iter())
            .map(|(l, r)| l.to_float() * r.to_float())
            .sum();
        Value::Float(sum)
    }

    pub fn sqrt(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().sqrt()))
    }

    pub fn sin(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().sin()))
    }

    pub fn cos(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().cos()))
    }

    pub fn tan(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().tan()))
    }

    pub fn asin(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().asin()))
    }

    pub fn acos(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().acos()))
    }

    pub fn atan(&self) -> Value {
        self.apply_unary_op(&|val| Value::Float(val.to_float().atan()))
    }

    pub fn count(&self) -> Value {
        if let &Value::Vector(ref v) = self {
            Value::Integer((*v).len() as i64)
        } else {
            Value::Integer(1)
        }
    }

    pub fn len(&self) -> Value {
        if let &Value::Vector(ref v) = self {
            let mut sum = 0.0;
            for val in (*v).iter() {
                let f_val = val.to_float();
                sum += f_val * f_val;
            }
            Value::Float(sum.sqrt())
        } else {
            Value::Float(self.to_float())
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn to_str(val: &Value, buf: &mut String) {
            match val {
                &Value::Float(fl) => buf.push_str(&format!("{}", fl)),
                &Value::Integer(n) => buf.push_str(&format!("{}", n)),
                &Value::Vector(ref v) => {
                    buf.push('[');
                    let mut first = true;
                    for vval in (*v).iter() {
                        if first == false {
                            buf.push_str(", ");
                        }
                        to_str(vval, buf);
                        first = false;
                    }
                    buf.push(']');
                }
            }
        };
        let mut buf = String::new();
        to_str(self, &mut buf);
        write!(f, "{}", &buf)
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a + b),
                (left, right) => Value::Float(left.to_float() + right.to_float()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a - b),
                (left, right) => Value::Float(left.to_float() - right.to_float()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a & b),
                (left, right) => Value::Integer(left.to_integer() & right.to_integer()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a | b),
                (left, right) => Value::Integer(left.to_integer() | right.to_integer()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a ^ b),
                (left, right) => Value::Integer(left.to_integer() ^ right.to_integer()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a * b),
                (left, right) => Value::Float(left.to_float() * right.to_float()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| Value::Float(l.to_float() / r.to_float()))
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a % b),
                (left, right) => Value::Float(left.to_float() % right.to_float()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a << b),
                (left, right) => Value::Integer(left.to_integer() << right.to_integer()),
            }
        })
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
        self.apply_binary_op(&other, &|l, r| {
            match (l, r) {
                (&Value::Integer(a), &Value::Integer(b)) => Value::Integer(a >> b),
                (left, right) => Value::Integer(left.to_integer() >> right.to_integer()),
            }
        })
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
            Value::Vector(ref v) => {
                let resv: Vec<Value> = (*v).iter()
                    .map(|val| val.clone().neg())
                    .collect();
                Value::Vector(resv.rc())
            },
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (&Value::Integer(l), &Value::Integer(r)) => l.partial_cmp(&r),
            (&Value::Float(l), &Value::Float(r)) => l.partial_cmp(&r),
            (&Value::Integer(l), &Value::Float(r)) => (l as f64).partial_cmp(&r),
            (&Value::Float(l), &Value::Integer(r)) => l.partial_cmp(&(r as f64)),
            _ => None,
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
        match self {
            &Value::Integer(n) => n as f64,
            &Value::Float(f) => f,
            &Value::Vector(ref v) => (*v).len() as f64,
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
        match self {
            &Value::Integer(n) => n,
            &Value::Float(f) => f as i64,
            &Value::Vector(ref v) => v.len() as i64,
        }
    }

    fn to_vector(&self) -> Rc<Vec<Value>> {
        match self {
            &Value::Integer(n) => vec![Value::Integer(n)].rc(),
            &Value::Float(f) => vec![Value::Float(f)].rc(),
            &Value::Vector(ref v) => v.clone(),
        }
    }

    fn apply_binary_op<F>(&self, r: &Value, f: &F) -> Value
        where F: Fn(&Value, &Value) -> Value {
        match (self, r) {
            (&Value::Vector(ref lv), &Value::Vector(ref rv)) => {
                let resv: Vec<Value> = (*lv).iter()
                    .zip((*rv).iter())
                    .map(|(l, r)| l.apply_binary_op(r, f))
                    .collect();
                Value::Vector(resv.rc())
            },
            (&Value::Vector(ref lv), r) => {
                let resv: Vec<Value> = (*lv).iter()
                    .map(|l| l.apply_binary_op(r, f))
                    .collect();
                Value::Vector(resv.rc())
            },
            (l, &Value::Vector(ref rv)) => {
                let resv: Vec<Value> = (*rv).iter()
                    .map(|r| l.apply_binary_op(r, f))
                    .collect();
                Value::Vector(resv.rc())
            },
            (l, r) => f(l, r),
        }
    }

    fn apply_unary_op<F>(&self, f: &F) -> Value
        where F: Fn(&Value) -> Value {
        match self {
            &Value::Vector(ref v) => {
                let resv: Vec<Value> = (*v).iter()
                    .map(|val| val.apply_unary_op(f))
                    .collect();
                Value::Vector(resv.rc())
            },
            v => f(v),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use util::Boxable;

    fn ivec(v: Vec<i32>) -> Value {
        let vout: Vec<Value> = v.iter().map(|i| Value::Integer(*i as i64)).collect();
        Value::Vector(vout.rc())
    }

    fn fvec(v: Vec<f64>) -> Value {
        let vout: Vec<Value> = v.iter().map(|f| Value::Float(*f)).collect();
        Value::Vector(vout.rc())
    }

    #[test]
    fn test_add() {
        assert_eq!(Value::Integer(2) + Value::Integer(3), Value::Integer(5));
        assert_eq!(Value::Integer(2) + Value::Float(3.0), Value::Float(5.0));
        assert_eq!(Value::Float(2.0) + Value::Integer(3), Value::Float(5.0));
        assert_eq!(Value::Float(2.0) + Value::Float(3.0), Value::Float(5.0));
    }

    #[test]
    fn test_add_vectors() {
        assert_eq!(ivec(vec![1, 2, 3]) + Value::Float(3.0),
                   fvec(vec![4.0, 5.0, 6.0]));
        assert_eq!(Value::Integer(1) + fvec(vec![1.0, 2.0, 3.0]),
                   fvec(vec![2.0, 3.0, 4.0]));
        assert_eq!(ivec(vec![1, 2, 3]) + ivec(vec![1, 2, 3, 4, 5]),
                   ivec(vec![2, 4, 6]));
        assert_eq!(ivec(vec![1, 2, 3, 4, 5]) + ivec(vec![1, 2, 3]),
                   ivec(vec![2, 4, 6]));
    }

    #[test]
    fn test_sub() {
        assert_eq!(Value::Integer(2) - Value::Integer(3), Value::Integer(-1));
        assert_eq!(Value::Integer(2) - Value::Float(3.0), Value::Float(-1.0));
        assert_eq!(Value::Float(2.0) - Value::Integer(3), Value::Float(-1.0));
        assert_eq!(Value::Float(2.0) - Value::Float(3.0), Value::Float(-1.0));
        assert_eq!(ivec(vec![1, 2, 3]) - Value::Integer(1), ivec(vec![0, 1, 2]));
    }

    #[test]
    fn test_mul() {
        assert_eq!(Value::Integer(2) * Value::Integer(3), Value::Integer(6));
        assert_eq!(Value::Integer(2) * Value::Float(3.0), Value::Float(6.0));
        assert_eq!(Value::Float(2.0) * Value::Integer(3), Value::Float(6.0));
        assert_eq!(Value::Float(2.0) * Value::Float(3.0), Value::Float(6.0));
        assert_eq!(ivec(vec![1, 2, 3]) * Value::Integer(2), ivec(vec![2, 4, 6]));
    }

    #[test]
    fn test_div() {
        assert_eq!(Value::Integer(6) / Value::Integer(3), Value::Float(2.0));
        assert_eq!(Value::Integer(6) / Value::Float(3.0), Value::Float(2.0));
        assert_eq!(Value::Float(6.0) / Value::Integer(3), Value::Float(2.0));
        assert_eq!(Value::Float(6.0) / Value::Float(3.0), Value::Float(2.0));
        assert_eq!(ivec(vec![1, 2, 3]) / Value::Integer(2), fvec(vec![0.5, 1.0, 1.5]));
    }

    #[test]
    fn test_integer_div() {
        assert_eq!(Value::Integer(7).integer_divide_by(&Value::Integer(3)), Value::Integer(2));
        assert_eq!(Value::Integer(7).integer_divide_by(&Value::Float(3.0)), Value::Integer(2));
        assert_eq!(Value::Float(7.0).integer_divide_by(&Value::Integer(3)), Value::Integer(2));
        assert_eq!(Value::Float(7.0).integer_divide_by(&Value::Float(3.0)), Value::Integer(2));
        assert_eq!(ivec(vec![3, 4, 5]).integer_divide_by(&Value::Integer(2)), ivec(vec![1, 2, 2]));
    }

    #[test]
    fn test_rem() {
        assert_eq!(Value::Integer(7) % Value::Integer(3), Value::Integer(1));
        assert_eq!(Value::Integer(7) % Value::Float(3.0), Value::Float(1.0));
        assert_eq!(Value::Float(7.0) % Value::Integer(3), Value::Float(1.0));
        assert_eq!(Value::Float(7.0) % Value::Float(3.0), Value::Float(1.0));
        assert_eq!(ivec(vec![3, 4, 5]) % Value::Integer(2), ivec(vec![1, 0, 1]));
    }

    #[test]
    fn test_neg() {
        assert_eq!(-Value::Integer(10), Value::Integer(-10));
        assert_eq!(-Value::Float(-10.25), Value::Float(10.25));
        assert_eq!(-ivec(vec![1, 2, 3]), ivec(vec![-1, -2, -3]));
    }

    #[test]
    fn test_vectors_unary_op() {
        let vin = Value::Vector(vec![Value::Float(4.0), Value::Integer(16), Value::Float(25.0)].rc());
        let vout = Value::Vector(vec![Value::Float(2.0), Value::Float(4.0), Value::Float(5.0)].rc());
        assert_eq!(vin.sqrt(), vout);
    }

    #[test]
    fn test_vectors_binary_op() {
        let vin1 = Value::Vector(vec![Value::Integer(2), Value::Integer(3), Value::Float(4.0)].rc());
        let vout = Value::Vector(vec![Value::Float(4.0), Value::Float(9.0), Value::Float(16.0)].rc());
        assert_eq!(vin1.pow(&Value::Integer(2)), vout);

        let vout = Value::Vector(vec![Value::Float(4.0), Value::Float(8.0), Value::Float(16.0)].rc());
        assert_eq!(Value::Integer(2).pow(&vin1), vout);

        let vin2 = Value::Vector(vec![Value::Integer(3), Value::Integer(2), Value::Float(1.0)].rc());
        let vout = Value::Vector(vec![Value::Float(8.0), Value::Float(9.0), Value::Float(4.0)].rc());
        assert_eq!(vin1.pow(&vin2), vout);

        let vin2 = Value::Vector(vec![Value::Integer(3), Value::Integer(2), Value::Float(1.0), Value::Float(0.0)].rc());
        assert_eq!(vin1.pow(&vin2), vout);
    }

    #[test]
    fn test_nested_vectors() {
        // [[[1]]] + 1 = [[[3]]]
        let v = Value::Vector(vec![Value::Vector(vec![Value::Vector(vec![Value::Integer(1)].rc())].rc())].rc());
        assert_eq!(v + Value::Integer(2),
                   Value::Vector(vec![Value::Vector(vec![Value::Vector(vec![Value::Integer(3)].rc())].rc())].rc()));
    }
}
