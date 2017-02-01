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
use std::io;
use std::io::Write;
use value::Value;
use interpreter::{ RuntimeItem, Context };

static IT_IDENT: &'static str = "it";

struct Calculator {
    ctx: Box<Context>,
}

impl Calculator {
    pub fn new() -> Calculator {
        let ctx = {
            let mut map: HashMap<String, RuntimeItem> = HashMap::new();
            map.insert(IT_IDENT.to_string(), RuntimeItem::Value(Value::Integer(0)));
            map.insert("pi".to_string(), RuntimeItem::Value(Value::Float(3.1415926535897932384626433832795)));
            map.insert("e".to_string(), RuntimeItem::Value(Value::Float(2.7182818284590452353602874713527)));
            interpreter::context_from_hashmap(map)
        };
        Calculator { ctx: ctx }
    }

    pub fn calc(&mut self, src: &str) -> Result<&RuntimeItem, String> {
        let input = try!(lexer::lex(&src));
        info!("Tokens: {:?}", input);

        let stmt = try!(parser::parse(&input));
        info!("Ast: {:?}", stmt);

        let item = try!(interpreter::interpret(&stmt, &mut *self.ctx));
        self.ctx.put(IT_IDENT, item);
        Result::Ok(self.ctx.get(IT_IDENT).unwrap())
    }
}

fn main() {
    let mut calculator = Calculator::new();

    loop {
        print!("> ");
        io::stdout().flush().expect("stdout error");

        let line = {
            let mut buf = String::new();
            match io::stdin().read_line(&mut buf) {
                Ok(_) => buf,
                Err(e) => {
                    println!("{:?}", e);
                    "".to_string()
                },
            }
        };

        match line.trim() {
            "#q" => break,
            "" => (),
            line => {
                match calculator.calc(line) {
                    Ok(item) => {
                        if let &RuntimeItem::Value(ref v) = item {
                            print_value(v);
                        } else {
                            println!("Function OK");
                        };
                    },
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn print_value(v: &Value) {
    match v {
        &Value::Float(f) => println!("= {}", f),
        &Value::Integer(n) => {
            println!("= {}", n);
            println!("  {:#x}", n);
            println!("  {:#b}", n);
        },
        &Value::Vector(ref v) => println!("= {:?}", *v),
    }
}
