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
                let res = eval(line, &*ctx);
                println!("= {:?}", res);
            }
        }
    }
}

fn eval(line: &str, ctx: &interpreter::Context) -> Value {
    let input = lexer::lex(&line);
    println!("Tokens: {:?}", input);

    let expr = parser::parse(&input);
    println!("Ast: {:?}", expr);

    interpreter::interpret(&expr, ctx)
}
