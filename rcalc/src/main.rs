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

fn main() {
    let ctx = {
        let mut map: HashMap<String, Value> = HashMap::new();
        map.insert("a".to_string(), Value::Integer(1));
        map.insert("b".to_string(), Value::Integer(2));
        interpreter::context_from_hashmap(map)
    };

    loop {
        print!("> ");
        io::stdout().flush().expect("");

        let line = {
            let mut buf = String::new();
            io::stdin().read_line(&mut buf).expect("stdin failure");
            buf
        };

        match line.trim() {
            "#q" => break,
            "" => (),
            line => {
                match eval(line, &*ctx) {
                    Ok(res) => println!("= {:?}", res),
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn eval(line: &str, ctx: &interpreter::Context) -> Result<Value, String> {
    let input = try!(lexer::lex(&line));
    println!("Tokens: {:?}", input);

    let expr = parser::parse(&input);
    println!("Ast: {:?}", expr);

    Result::Ok(interpreter::interpret(&expr, ctx))
}
