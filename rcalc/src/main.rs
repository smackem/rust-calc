mod lexer;
mod parser;

use std::io;
use lexer::Token;

fn main() {
    let input = vec![Token::Integer(100), Token::Plus, Token::Integer(50)];

    let x = Token::Ident("abc".to_string());
    println!("Hello, world!");
}
