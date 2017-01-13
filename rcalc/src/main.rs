extern crate regex;
#[macro_use]
extern crate lazy_static;
extern crate num;

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
                    Ok(Some(item)) => {
                        println!("= {:?}", item);
                        ctx.put(IT_IDENT, item);
                    },
                    Ok(None) => println!("OK"),
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn process_line(line: &str, ctx: &mut interpreter::Context) -> Result<Option<RuntimeItem>, String> {
    let input = try!(lexer::lex(&line));
    println!("Tokens: {:?}", input);

    let stmt = try!(parser::parse(&input));
    println!("Ast: {:?}", stmt);

    interpreter::interpret(&stmt, ctx)
}
