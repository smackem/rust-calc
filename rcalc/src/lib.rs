extern crate regex;
#[macro_use]
extern crate lazy_static;
extern crate num;
#[macro_use]
extern crate log;

mod lexer;
mod parser;
mod value;
mod interpreter;
mod util;

use std::collections::HashMap;
use interpreter::Context;
use lexer::Lexer;
use parser::{ Stmt, Expr };

pub use value::Value;
pub use interpreter::RuntimeItem;

/// The identifier for the result of the last evaluation.
///
/// # Examples
/// ```
/// use rcalc::{ Calculator, RuntimeItem, Value };
/// let mut calculator = Calculator::new();
/// assert_eq!(calculator.calc(r"1"),
///            Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
/// assert_eq!(calculator.calc(r"it + 1"),
///            Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
/// ```
pub static IT_IDENT: &'static str = "it";

/// The top-level calculator class.
///
/// # Examples
///
/// ```
/// use rcalc::{ Calculator, RuntimeItem, Value };
/// let mut calculator = Calculator::new();
/// assert_eq!(calculator.calc("1 + 1"), Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
/// ```
pub struct Calculator {
    ctx: Box<Context>,
    lexer: Lexer,
    deferred_exprs: Vec<Expr>,
}

impl Calculator {
    /// Creates and initializes a new instance of `Calculator`.
    pub fn new() -> Calculator {
        let ctx = {
            let mut map: HashMap<String, RuntimeItem> = HashMap::new();
            map.insert(IT_IDENT.to_string(), RuntimeItem::Value(Value::Integer(0)));
            map.insert("pi".to_string(), RuntimeItem::Value(Value::Float(std::f64::consts::PI)));
            map.insert("e".to_string(), RuntimeItem::Value(Value::Float(std::f64::consts::E)));
            interpreter::context_from_hashmap(map)
        };
        Calculator { ctx: ctx, lexer: Lexer::new(), deferred_exprs: vec![] }
    }

    /// Parses and evaluates the specified source.
    ///
    /// # Examples
    ///
    /// ```
    /// use rcalc::{ Calculator, RuntimeItem, Value };
    /// let mut calculator = Calculator::new();
    /// assert_eq!(calculator.calc("1 + 1"), Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
    /// ```
    ///
    /// ```
    /// use rcalc::{ Calculator, RuntimeItem, Value };
    /// let mut calculator = Calculator::new();
    /// let src = "let f(x) = x**2";
    /// match calculator.calc(src) {
    ///     Result::Ok(&RuntimeItem::Function(..)) => (),
    ///     _ => assert!(false),
    /// }
    /// ```
    ///
    /// ```
    /// use rcalc::{ Calculator, RuntimeItem, Value };
    /// let mut calculator = Calculator::new();
    /// assert_eq!(calculator.calc("let a = 1"),
    ///            Result::Ok(&RuntimeItem::Value(Value::Integer(1))));
    /// assert_eq!(calculator.calc("a + 1"),
    ///            Result::Ok(&RuntimeItem::Value(Value::Integer(2))));
    /// ```
    pub fn calc(&mut self, src: &str) -> Result<&RuntimeItem, String> {
        let input = try!(self.lexer.lex(&src));
        info!("Tokens: {:?}", input);

        let stmt = try!(parser::parse(&input));
        info!("Ast: {:?}", stmt);

        let item = try!(interpreter::interpret(&stmt, &mut *self.ctx));
        self.ctx.put(IT_IDENT, item);
        Result::Ok(self.ctx.get(IT_IDENT).unwrap())
    }

    /// Parses the specified source and either (in the case of a binding), interprets
    /// the resulting statement or (in the case of an evaluation), defers evaluation
    /// until `calc_deferred` is called.
    ///
    /// # Returns
    ///
    /// * `true` if the source was parsed as an evaluation and the evaluation was deferred.
    /// & `false` if the source was parsed as a binding and interpreted instantly.
    ///
    /// # Examples
    ///
    /// ```
    /// use rcalc::{ Calculator, RuntimeItem, Value };
    /// let mut calculator = Calculator::new();
    /// assert_eq!(calculator.defer("1 + 1"),
    ///            Result::Ok(true));
    /// assert_eq!(calculator.defer("let a = 1"),
    ///            Result::Ok(false));
    /// ```
    pub fn defer(&mut self, src: &str) -> Result<bool, String> {
        let input = try!(self.lexer.lex(&src));
        info!("Tokens: {:?}", input);

        let stmt = try!(parser::parse(&input));
        info!("Ast: {:?}", stmt);

        match stmt {
            Stmt::Eval(expr) => {
                self.deferred_exprs.push(expr);
                Result::Ok(true)
            },
            _ => {
                try!(interpreter::interpret(&stmt, &mut *self.ctx));
                Result::Ok(false)
            },
        }
    }

    pub fn calc_deferred(&mut self) -> Result<&RuntimeItem, String> {
        let exprs = {
            let mut exprs = vec![];
            exprs.append(&mut self.deferred_exprs);
            exprs
        };
        unimplemented!()
    }
}

// ============================================================================

impl Calculator {
    fn eval(&self, src: &str) -> Result<Value, String> {
        let input = try!(self.lexer.lex(&src));
        info!("Tokens: {:?}", input);

        let stmt = try!(parser::parse(&input));
        info!("Ast: {:?}", stmt);

        let expr = match stmt {
            Stmt::Eval(expr) => expr,
            _ => try!(Result::Err("Statement is not an evaluation!")),
        };

        let val = try!(interpreter::eval_expr(&expr, &*self.ctx));
        Result::Ok(val)
    }
}
