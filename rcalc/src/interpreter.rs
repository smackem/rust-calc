use std::collections::HashMap;
use std::cmp::Ordering;
use parser::{Expr, Stmt};
use value::Value;
use util::Boxable;

/// The entities that result from interpretation and that are stored in a
/// `Context`.
#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeItem {
    /// A function definition with parameter identifiers and function body
    Function(Vec<String>, Expr),
    /// The result of an evaluation
    Value(Value),
}

/// Interprets the given `Stmt`, using the specified `Context` for binding
/// lookup and storage. Returns either the resulting `RuntimeItem` if successful
/// or an error message.
pub fn interpret(stmt: &Stmt, ctx: &mut Context) -> Result<RuntimeItem, String> {
    let item = match *stmt {
        Stmt::VarBind(ref ident, ref expr) => {
            let val = try!(eval_expr(expr, ctx));
            let item = RuntimeItem::Value(val);
            ctx.put(ident, item.clone());
            item
        },
        Stmt::FunBind(ref ident, ref param_idents, ref expr) => {
            let item = RuntimeItem::Function(param_idents.clone(), expr.clone());
            ctx.put(ident, item.clone());
            item
        },
        Stmt::Eval(ref expr) => {
            let val = try!(eval_expr(expr, ctx));
            RuntimeItem::Value(val)
        },
    };

    Result::Ok(item)
}

/// Evaluates the given expression and returns the result.
/// The given `Context` stores the variables that can be addressed
/// through identifiers.
pub fn eval_expr(expr: &Expr, ctx: &Context) -> Result<Value, String> {
    let val = match *expr {
        Expr::Plus(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) + try!(eval_expr(&*right, ctx)),
        Expr::Minus(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) - try!(eval_expr(&*right, ctx)),
        Expr::BitwiseAnd(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) & try!(eval_expr(&*right, ctx)),
        Expr::BitwiseOr(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) | try!(eval_expr(&*right, ctx)),
        Expr::BitwiseXor(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) ^ try!(eval_expr(&*right, ctx)),
        Expr::Times(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) * try!(eval_expr(&*right, ctx)),
        Expr::Div(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) / try!(eval_expr(&*right, ctx)),
        Expr::IntDiv(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).integer_divide_by(&try!(eval_expr(&*right, ctx))),
        Expr::Mod(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) % try!(eval_expr(&*right, ctx)),
        Expr::ShiftLeft(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) << try!(eval_expr(&*right, ctx)),
        Expr::ShiftRight(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)) >> try!(eval_expr(&*right, ctx)),
        Expr::DotProduct(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).dot_product(&try!(eval_expr(&*right, ctx))),
        Expr::Pow(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).pow(&try!(eval_expr(&*right, ctx))),
        Expr::Log(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).log(&try!(eval_expr(&*right, ctx))),
        Expr::Concat(ref left, ref right) =>
            try!(eval_expr(&*left, ctx)).concat(&try!(eval_expr(&*right, ctx))),
        Expr::Neg(ref inner) =>
            -try!(eval_expr(&*inner, ctx)),
        Expr::BitwiseNot(ref inner) =>
            !try!(eval_expr(&*inner, ctx)),
        Expr::Sqrt(ref inner) =>
            try!(eval_expr(&*inner, ctx)).sqrt(),
        Expr::Sin(ref inner) =>
            try!(eval_expr(&*inner, ctx)).sin(),
        Expr::Cos(ref inner) =>
            try!(eval_expr(&*inner, ctx)).cos(),
        Expr::Tan(ref inner) =>
            try!(eval_expr(&*inner, ctx)).tan(),
        Expr::Asin(ref inner) =>
            try!(eval_expr(&*inner, ctx)).asin(),
        Expr::Acos(ref inner) =>
            try!(eval_expr(&*inner, ctx)).acos(),
        Expr::Atan(ref inner) =>
            try!(eval_expr(&*inner, ctx)).atan(),
        Expr::Len(ref inner) =>
            try!(eval_expr(&*inner, ctx)).len(),
        Expr::Count(ref inner) =>
            try!(eval_expr(&*inner, ctx)).count(),
        Expr::BindingRef(ref s) => {
            let res = match ctx.get(s) {
                Some(item) => {
                    match *item {
                        RuntimeItem::Value(ref v) => Result::Ok(v.clone()),
                        _ => Result::Err(format!("'{}' is not a value", s)),
                    }
                },
                None => Result::Err(format!("Identifier '{}' not found", s)), 
            };
            try!(res)
        },
        Expr::FunCall(ref s, ref args) => {
            let res = match ctx.get(s) {
                Some(item) => {
                    match *item {
                        RuntimeItem::Function(ref param_idents, ref body_expr) =>
                            call_function(param_idents, args, body_expr, ctx),
                        _ => Result::Err(format!("'{}' is not a function", s)),
                    }
                },
                None => Result::Err(format!("Identifier '{}' not found", s)), 
            };
            try!(res)
        },
        Expr::Literal(ref v) => v.clone(),
        Expr::Vector(ref exprs) => {
            let mut values = vec![];
            for ref e in (*exprs).iter() {
                values.push(try!(eval_expr(e, ctx)));
            }
            Value::Vector(values.arc())
        },
        Expr::RangeVector(ref range) => {
            let mut values = vec![];
            let mut value = try!(eval_expr(&range.incl_lower_bound, ctx));
            let upper_val = try!(eval_expr(&range.excl_upper_bound, ctx));
            let step_val = try!(eval_expr(&range.step, ctx));
            loop {
                values.push(value.clone());
                value = value + step_val.clone();
                match value.partial_cmp(&upper_val) {
                    Some(Ordering::Less) => (),
                    None | Some(Ordering::Greater) | Some(Ordering::Equal) => break,
                }
            }

            Value::Vector(values.arc())
        }
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

    /// Lists all ident/value pairs present in the `Context`.
    fn list(&self) -> Vec<(&String, &RuntimeItem)>;
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

    fn list(&self) -> Vec<(&String, &RuntimeItem)> {
        self.map.iter().collect()
    }
}

struct StackedContext<'a> {
    head: MapContext,
    next: &'a Context,
}

impl<'a> StackedContext<'a> {
    fn new(next: &'a Context) -> StackedContext {
        let map = MapContext { map: HashMap::new() };
        StackedContext { head: map, next: next }
    }
}

impl<'a> Context for StackedContext<'a> {
    fn get(&self, ident: &str) -> Option<&RuntimeItem> {
        match self.head.get(ident) {
            Some(item) => Some(item),
            None => self.next.get(ident),
        }
    }

    fn put(&mut self, ident: &str, item: RuntimeItem) {
        self.head.put(ident, item);
    }

    fn list(&self) -> Vec<(&String, &RuntimeItem)> {
        let mut tail_list = self.next.list();
        let mut head_list = self.head.list();
        tail_list.append(&mut head_list);
        tail_list
    }
}

fn call_function(param_idents: &Vec<String>, args: &Vec<Expr>, body: &Expr, ctx: &Context) -> Result<Value, String> {
    let min_len = ::std::cmp::min(param_idents.len(), args.len());
    let mut head_ctx = StackedContext::new(ctx);
    for i in 0..min_len {
        let arg_val = try!(eval_expr(&args[i], ctx));
        head_ctx.put(&param_idents[i], RuntimeItem::Value(arg_val));
    }
    eval_expr(body, &head_ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::{Expr, Stmt, integer_expr, float_expr};
    use value::Value;
    use util::Boxable;
    use std::collections::HashMap;

    fn ctx() -> Box<Context> {
        context_from_hashmap(HashMap::new())
    }

    #[test]
    fn test_eval_simple() {
        // 1
        let expr = integer_expr(1);
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_eval_term() {
        // 1 + 2
        let expr = Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed());
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(3));
    }

    #[test]
    fn test_eval_complex() {
        // (1 + 2) * (5 - 3.0)
        let expr =
            Expr::Times(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()).boxed(),
                        Expr::Minus(integer_expr(5).boxed(), float_expr(3.0).boxed()).boxed());
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Float(6.0));
    }

    #[test]
    fn test_eval_negate() {
        // 1 - -2
        let expr = Expr::Minus(integer_expr(1).boxed(), Expr::Neg(integer_expr(2).boxed()).boxed());
        let res = eval_expr(&expr, &mut *ctx()).unwrap();
        assert_eq!(res, Value::Integer(3));
    }

    #[test]
    fn test_eval_lookup_ident() {
        let expr = Expr::BindingRef("X".to_string());
        let mut ctx = ctx();
        (*ctx).put("X", RuntimeItem::Value(Value::Integer(1)));
        let res = eval_expr(&expr, &mut *ctx).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_eval_fail_unknown_ident() {
        let expr = Expr::BindingRef("X".to_string());
        let res = eval_expr(&expr, &mut *ctx());
        assert!(res.is_err());
    }

    #[test]
    fn test_eval_fun_call_1() {
        let expr = Expr::FunCall("f".to_string(), Box::new(vec![]));
        let mut ctx = ctx();
        (*ctx).put("f", RuntimeItem::Function(vec![], integer_expr(1)));
        let res = eval_expr(&expr, &mut *ctx).unwrap();
        assert_eq!(res, Value::Integer(1));
    }

    #[test]
    fn test_eval_fun_call_2() {
        // let f(x) = x
        // f(123)
        let expr = Expr::FunCall("f".to_string(), Box::new(vec![integer_expr(123)]));
        let mut ctx = ctx();
        (*ctx).put("f", RuntimeItem::Function(vec!["x".to_string()], Expr::BindingRef("x".to_string())));
        let res = eval_expr(&expr, &mut *ctx).unwrap();
        assert_eq!(res, Value::Integer(123));
    }

    #[test]
    fn test_interpret_simple() {
        // 1
        let stmt = Stmt::Eval(integer_expr(1));
        let mut ctx = ctx();
        let res = interpret(&stmt, &mut *ctx).unwrap();
        assert_eq!(res, RuntimeItem::Value(Value::Integer(1)));
    }

    #[test]
    fn test_interpret_var_bind() {
        // let a = 1
        let stmt = Stmt::VarBind("a".to_string(), integer_expr(1));
        let mut ctx = ctx();
        let res = interpret(&stmt, &mut *ctx).unwrap();
        assert_eq!(res, RuntimeItem::Value(Value::Integer(1)));
        assert_eq!(ctx.get("a"), Some(&RuntimeItem::Value(Value::Integer(1))));
    }

    #[test]
    fn test_interpret_fun_bind_1() {
        // let f() = 1
        let stmt = Stmt::FunBind("f".to_string(), vec![], integer_expr(1));
        let mut ctx = ctx();
        let res = interpret(&stmt, &mut *ctx).unwrap();
        assert_eq!(res, RuntimeItem::Function(vec![], integer_expr(1)));
        assert_eq!(ctx.get("f"), Some(&RuntimeItem::Function(vec![], integer_expr(1))));
    }

    #[test]
    fn test_interpret_fun_bind_2() {
        // let f(x) = x
        let stmt = Stmt::FunBind("f".to_string(),
                                 vec!["x".to_string()],
                                 Expr::BindingRef("x".to_string()));
        let mut ctx = ctx();
        let res = interpret(&stmt, &mut *ctx).unwrap();
        assert_eq!(res, RuntimeItem::Function(vec!["x".to_string()], Expr::BindingRef("x".to_string())));
        assert_eq!(ctx.get("f"),
                   Some(&RuntimeItem::Function(vec!["x".to_string()], Expr::BindingRef("x".to_string()))));
    }
}
