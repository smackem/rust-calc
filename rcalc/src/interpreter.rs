use std::collections::HashMap;
use parser::Expr;

/// Evaluates the given expression and returns the result.
/// The given `Context` stores the variables that can be addressed
/// through identifiers.
///
/// # Examples
///
/// ```
/// let expr = Expr::Integer(1);
/// let res = interpret(&expr, &*ctx());
/// assert_eq!(res, 1);
/// ```
pub fn interpret(expr: &Expr, ctx: &Context) -> i32 {
    match *expr {
        Expr::Plus(ref left, ref right) => interpret(&*left, ctx) + interpret(&*right, ctx),
        Expr::Minus(ref left, ref right) => interpret(&*left, ctx) - interpret(&*right, ctx),
        Expr::Times(ref left, ref right) => interpret(&*left, ctx) * interpret(&*right, ctx),
        Expr::Div(ref left, ref right) => interpret(&*left, ctx) / interpret(&*right, ctx),
        Expr::Mod(ref left, ref right) => interpret(&*left, ctx) % interpret(&*right, ctx),
        Expr::Ident(ref s) => ctx.get(s),
        Expr::Integer(n) => n,
    }
}

/// Client code can use this trait to provide context for the
/// `interpret` function, like variables.
pub trait Context {

    /// Gets the value of the variable with the given `ident`.
    fn get(&self, ident: &str) -> i32;

    /// Sets the value of the variable with the given `ident` to `value`.
    /// Adds the variable if it is not present.
    fn put(&mut self, ident: &str, value: i32);
}

/// Returns a boxed object that implements the `Context` trait based
/// on the given `map` that is used to keep variable values.
pub fn context_from_hashmap(map: HashMap<String, i32>) -> Box<Context> {
    Box::new(MapContext { map: map })
}

// ============================================================================

struct MapContext {
    map: HashMap<String, i32>,
}

impl Context for MapContext {
    fn get(&self, ident: &str) -> i32 {
        *self.map.get(ident).unwrap()
    }

    fn put(&mut self, ident: &str, value: i32) {
        self.map.insert(ident.to_string(), value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::Expr;
    use std::collections::HashMap;

    fn ctx() -> Box<Context> {
        context_from_hashmap(HashMap::new())
    }

    #[test]
    fn test_interpret_simple() { // 1
        let expr = Expr::Integer(1);
        let res = interpret(&expr, &*ctx());
        assert_eq!(res, 1);
    }

    #[test]
    fn test_interpret_term() { // 1 + 2
        let expr = Expr::Plus(Expr::Integer(1).boxed(), Expr::Integer(2).boxed());
        let res = interpret(&expr, &*ctx());
        assert_eq!(res, 3);
    }

    #[test]
    fn test_interpret_complex() { // (1 + 2) * (5 - 3)
        let expr = Expr::Times(
            Expr::Plus(Expr::Integer(1).boxed(), Expr::Integer(2).boxed()).boxed(),
            Expr::Minus(Expr::Integer(5).boxed(), Expr::Integer(3).boxed()).boxed());
        let res = interpret(&expr, &*ctx());
        assert_eq!(res, 6);
    }
}
