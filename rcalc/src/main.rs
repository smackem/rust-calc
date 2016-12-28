mod lexer;
mod parser;

use lexer::Token;

fn main() {
    let input = vec![Token::Integer(120), Token::Plus, Token::Integer(50)];
    let expr = parser::parse(&input);

    println!("Hello, world: {:?}", expr);
}
