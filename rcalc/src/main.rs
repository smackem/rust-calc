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
    let mut ctx = {
        let mut map: HashMap<String, Value> = HashMap::new();
        map.insert("it".to_string(), Value::Integer(0));
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
                match eval(line, &mut *ctx) {
                    Ok(res) => println!("= {:?}", res),
                    Err(msg) => println!("{}", msg),
                }
            }
        }
    }
}

fn eval(line: &str, ctx: &mut interpreter::Context) -> Result<Value, String> {
    let input = try!(lexer::lex(&line));
    println!("Tokens: {:?}", input);

    let expr = parser::parse(&input);
    println!("Ast: {:?}", expr);

    Result::Ok(interpreter::interpret(&expr, ctx))
}
