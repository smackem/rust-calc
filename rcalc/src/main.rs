extern crate regex;
#[macro_use] extern crate lazy_static;

mod lexer;
mod parser;
mod interpreter;

use std::collections::HashMap;

fn main() {
    let input = lexer::lex("1 + 2 * (3 - 2) + a - b");
    println!("Tokens: {:?}", input);

    let expr = parser::parse(&input);
    println!("Ast: {:?}", expr);

    let ctx = {
        let mut map: HashMap<String, i32> = HashMap::new();
        map.insert("a".to_string(), 1);
        map.insert("b".to_string(), 2);
        interpreter::context_from_hashmap(map)
    };
    let res = interpreter::interpret(&expr, &*ctx);
    println!("Result: {}", res);
}
