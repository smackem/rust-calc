use std::collections::HashMap;
use parser::Expr;

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

pub trait Context {
    fn get(&self, ident: &str) -> i32;
    fn put(&mut self, ident: &str, value: i32);
}

pub fn context_from_hashmap(map: HashMap<String, i32>) -> Box<Context> {
    Box::new(MapContext { map: map })
}

///////////////////////////////////////////////////////////////////////////////

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
