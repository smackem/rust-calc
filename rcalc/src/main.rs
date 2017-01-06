extern crate regex;
#[macro_use]
extern crate lazy_static;

mod lexer;
mod parser;
mod value;
mod interpreter;

use std::collections::HashMap;
use std::io;
use std::io::Write;
use value::Value;
use interpreter::RuntimeItem;

static IT_IDENT: &'static str = "it";

fn main() {
    let mut ctx = {
        let mut map: HashMap<String, RuntimeItem> = HashMap::new();
        map.insert(IT_IDENT.to_string(), RuntimeItem::Value(Value::Integer(0)));
        interpreter::context_from_hashmap(map)
    };

    loop {
        print!("> ");
        io::stdout().flush().expect("stdout error");

        let line = {
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).expect("stdin error");
            buf
        };

        match line.trim() {
            "#q" => break,
            "" => (),
            line => {
                match process_line(line, &mut *ctx) {
                    Ok(val) => {
                        println!("= {:?}", val);
                        ctx.put(IT_IDENT, RuntimeItem::Value(val));
                    },
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn process_line(line: &str, ctx: &mut interpreter::Context) -> Result<Value, String> {
    let input = try!(lexer::lex(&line));
    println!("Tokens: {:?}", input);

    let stmt = try!(parser::parse(&input));
    println!("Ast: {:?}", stmt);

    if let parser::Stmt::Expr(expr) = stmt {
        interpreter::eval_expr(&expr, ctx)
    }
    else {
        Result::Err("Stmt not implemented yet".to_string())
    }
}
