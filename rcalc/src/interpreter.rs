use std::collections::HashMap;
use parser::Expr;
use value::Value;

/// Evaluates the given expression and returns the result.
/// The given `Context` stores the variables that can be addressed
/// through identifiers.
///
/// # Examples
///
/// ```
/// let expr = parser::integer_expr(1);
/// let res = interpret(&expr, &*ctx());
/// assert_eq!(res, Value::Integer(1));
/// ```
pub fn interpret(expr: &Expr, ctx: &mut Context) -> Result<Value, String> {
    let val = match *expr {
        Expr::Plus(ref left, ref right) =>
            try!(interpret(&*left, ctx)) + try!(interpret(&*right, ctx)),
        Expr::Minus(ref left, ref right) =>
            try!(interpret(&*left, ctx)) - try!(interpret(&*right, ctx)),
        Expr::Times(ref left, ref right) =>
            try!(interpret(&*left, ctx)) * try!(interpret(&*right, ctx)),
        Expr::Div(ref left, ref right) =>
            try!(interpret(&*left, ctx)) / try!(interpret(&*right, ctx)),
        Expr::IntDiv(ref left, ref right) =>
            try!(interpret(&*left, ctx)).integer_divide_by(&try!(interpret(&*right, ctx))),
        Expr::Mod(ref left, ref right) =>
            try!(interpret(&*left, ctx)) % try!(interpret(&*right, ctx)),
        Expr::Ident(ref s) => {
            let res = match ctx.get(s) {
                Some(v) => Result::Ok(v),
                None => Result::Err(format!("Identifier '{}' not found", s)), 
            };
            try!(res)
        },
        Expr::Literal(ref v) => *v,
    };

    Result::Ok(val)
}

/// Client code can use this trait to provide context for the
/// `interpret` function, like variables.
pub trait Context {
    /// Gets the value of the variable with the given `ident`.
    fn get(&self, ident: &str) -> Option<Value>;

    /// Sets the value of the variable with the given `ident` to `value`.
    /// Adds the variable if it is not present.
    fn put(&mut self, ident: &str, value: Value);
}

/// Returns a boxed object that implements the `Context` trait based
/// on the given `map` that is used to keep variable values.
pub fn context_from_hashmap(map: HashMap<String, Value>) -> Box<Context> {
    Box::new(MapContext { map: map })
}

// ============================================================================

struct MapContext {
    map: HashMap<String, Value>,
}

impl Context for MapContext {
    fn get(&self, ident: &str) -> Option<Value> {
        match self.map.get(ident) {
            Some(v) => Some(*v),
            None => None,
        }
    }

    fn put(&mut self, ident: &str, value: Value) {
        self.map.insert(ident.to_string(), value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Expr;
    use parser::integer_expr;
    use parser::float_expr;
    use value::Value;
    use std::collections::HashMap;

    fn ctx() -> Box<Context> {
        context_from_hashmap(HashMap::new())
    }

    #[test]
    fn test_interpret_simple() {
        // 1
        let expr = integer_expr(1);
        let res = interpret(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_interpret_term() {
        // 1 + 2
        let expr = Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed());
        let res = interpret(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(3));
    }

    #[test]
    fn test_interpret_complex() {
        // (1 + 2) * (5 - 3.0)
        let expr =
            Expr::Times(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()).boxed(),
                        Expr::Minus(integer_expr(5).boxed(), float_expr(3.0).boxed()).boxed());
        let res = interpret(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Float(6.0));
    }

    #[test]
    fn test_interpret_lookup_ident() {
        let expr = Expr::Ident("X".to_string());
        let mut ctx = ctx();
        (*ctx).put("X", Value::Integer(1));
        let res = interpret(&expr, &mut *ctx).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_interpret_fail_unknown_ident() {
        let expr = Expr::Ident("X".to_string());
        let res = interpret(&expr, &mut *ctx());
        assert!(res.is_err());
    }
}
