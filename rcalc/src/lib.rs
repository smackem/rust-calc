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
use std::thread;
use std::io::{ BufWriter, Write };
use interpreter::Context;
use lexer::Lexer;
use util::Boxable;

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
        Calculator { ctx: ctx, lexer: Lexer::new() }
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

    /// Parses and evaluates the specified sources in parallel, merging the
    /// resulting `RuntimeItem`s into this `Calculator`'s context.
    /// 'it' will afterwards refer to a vector containing all the values
    /// that have been successfully calculated in parallel. This vector
    /// has the same order as the input vector of sources.
    ///
    /// # Return Value
    ///
    /// The 'it' value, which is always a vector.
    ///
    /// # Examples
    ///
    /// ```
    /// use rcalc::{ Calculator, RuntimeItem, Value };
    /// use std::sync::Arc;
    /// let mut calculator = Calculator::new();
    /// assert_eq!(calculator.calc_parallel(vec!["1 + 1".to_string(), "2 + 2".to_string()]),
    ///            &RuntimeItem::Value(Value::Vector(Arc::new(vec![Value::Integer(2), Value::Integer(4)]))));
    /// ```
    pub fn calc_parallel(&mut self, srcs: Vec<String>) -> &RuntimeItem {
        let mut threads = vec![];
        for src in srcs {
            let context_map = self.get_context();
            threads.push(thread::spawn(move || {
                let mut local_calc = Calculator::with_context(context_map);
                let _ = local_calc.calc(&src).unwrap();
                local_calc.get_context()
            }));
        };

        let mut its = vec![];
        for thread in threads {
            match thread.join() {
                Result::Ok(map) => {
                    for (ident, item) in map.iter() {
                        self.ctx.put(ident, item.clone());
                    };
                    if let &RuntimeItem::Value(ref val) = map.get(IT_IDENT).unwrap() {
                        its.push(val.clone());
                    };
                },
                Result::Err(x) => error!("{:?}", x),
            }
        }

        let it = RuntimeItem::Value(Value::Vector(its.arc()));
        self.ctx.put(IT_IDENT, it);
        self.ctx.get(IT_IDENT).unwrap()
    }

    /// Writes a JSON representation of the current calculator context into the specified
    /// `BufWriter`, e.g. `{ "a": 100, "b": 200 }`.
    /// This JSON representation only includes values, not functions.
    pub fn write_json<T: Write>(&self, writer: &mut BufWriter<T>) -> ::std::io::Result<()> {
        let item_tuples = {
            let mut items = (*self.ctx).list();
            items.sort_by_key(|&(ident, _)| ident);
            items
        };
        try!(writer.write_all(b"{\n"));
        for (ident, item) in item_tuples {
            if let &RuntimeItem::Value(ref val) = item {
                try!(write!(writer, "    \"{}\": ", &ident));
                try!(val.write_json(writer));
                try!(writer.write_all(b",\n"));
            }
        }
        writer.write_all(b"}")
    }
}

// ============================================================================

impl Calculator {
    fn with_context(map: HashMap<String, RuntimeItem>) -> Calculator {
        let ctx = interpreter::context_from_hashmap(map);
        Calculator { ctx: ctx, lexer: Lexer::new() }
    }

    fn get_context(&self) -> HashMap<String, RuntimeItem> {
        let mut map = HashMap::new();
        for (ident, item) in self.ctx.list() {
            map.insert(ident.clone(), item.clone());
        }
        map
    }
}
