use std::collections::HashMap;
use parser::{Expr, Stmt};
use value::Value;

pub enum RuntimeItem {
    Function(Vec<String>, Expr),
    Value(Value),
}

pub fn interpret(stmt: &Stmt, ctx: &mut Context) -> Result<Option<RuntimeItem>, String> {
    let item = match *stmt {
        Stmt::VarBind(ref ident, ref expr) => {
            let val = try!(eval_expr(expr, ctx));
            ctx.put(ident, RuntimeItem::Value(val));
            Some(RuntimeItem::Value(val))
        },
        Stmt::FunBind(ref ident, ref param_idents, ref expr) => {
            let item = RuntimeItem::Function(param_idents.clone(), expr.clone());
            ctx.put(ident, item);
            None
        },
        Stmt::Expr(ref expr) => {
            let val = try!(eval_expr(expr, ctx));
            Some(RuntimeItem::Value(val))
        },
    };

    Result::Ok(item)
}

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
pub fn eval_expr(expr: &Expr, ctx: &mut Context) -> Result<Value, String> {
    let val = match *expr {
        Expr::Plus(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) + try!(eval_expr(&*right, ctx)),
        Expr::Minus(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) - try!(eval_expr(&*right, ctx)),
        Expr::Times(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) * try!(eval_expr(&*right, ctx)),
        Expr::Div(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) / try!(eval_expr(&*right, ctx)),
        Expr::IntDiv(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).integer_divide_by(try!(eval_expr(&*right, ctx))),
        Expr::Mod(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) % try!(eval_expr(&*right, ctx)),
        Expr::BindingRef(ref s) => {
            let res = match ctx.get(s) {
                Some(item) => {
                    if let RuntimeItem::Value(ref v) = *item {
                        Result::Ok(*v)
                    } else {
                        Result::Err(format!("'{}' is not a value", s))
                    }
                },
                None => Result::Err(format!("Identifier '{}' not found", s)), 
            };
            try!(res)
        },
        Expr::FunCall(ref s, ref params) => try!(Result::Err("FunCall not implemented")),
        Expr::Literal(ref v) => *v,
    };

    Result::Ok(val)
}

/// Client code can use this trait to provide context for the
/// `interpret` function, like bindings.
pub trait Context {
    /// Gets the `RuntimeItem` with the given `ident` or `None` if not present.
    fn get(&self, ident: &str) -> Option<&RuntimeItem>;

    /// Puts the `RuntimeItem` with the given `ident` into the `Context`.
    /// Adds the item if it is not present.
    fn put(&mut self, ident: &str, item: RuntimeItem);
}

/// Returns a boxed object that implements the `Context` trait based
/// on the given `map` that is used to keep variable values.
pub fn context_from_hashmap(map: HashMap<String, RuntimeItem>) -> Box<Context> {
    Box::new(MapContext { map: map })
}

// ============================================================================

struct MapContext {
    map: HashMap<String, RuntimeItem>,
}

impl Context for MapContext {
    fn get(&self, ident: &str) -> Option<&RuntimeItem> {
        match self.map.get(ident) {
            Some(item) => Some(item),
            None => None,
        }
    }

    fn put(&mut self, ident: &str, item: RuntimeItem) {
        self.map.insert(ident.to_string(), item);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::{Expr, integer_expr, float_expr};
    use value::Value;
    use std::collections::HashMap;

    fn ctx() -> Box<Context> {
        context_from_hashmap(HashMap::new())
    }

    #[test]
    fn test_interpret_simple() {
        // 1
        let expr = integer_expr(1);
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_interpret_term() {
        // 1 + 2
        let expr = Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed());
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(3));
    }

    #[test]
    fn test_interpret_complex() {
        // (1 + 2) * (5 - 3.0)
        let expr =
            Expr::Times(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()).boxed(),
                        Expr::Minus(integer_expr(5).boxed(), float_expr(3.0).boxed()).boxed());
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Float(6.0));
    }

    #[test]
    fn test_interpret_lookup_ident() {
        let expr = Expr::BindingRef("X".to_string());
        let mut ctx = ctx();
        (*ctx).put("X", RuntimeItem::Value(Value::Integer(1)));
        let res = eval_expr(&expr, &mut *ctx).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_interpret_fail_unknown_ident() {
        let expr = Expr::BindingRef("X".to_string());
        let res = eval_expr(&expr, &mut *ctx());
        assert!(res.is_err());
    }
}
