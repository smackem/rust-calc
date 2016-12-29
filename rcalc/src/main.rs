mod lexer;
mod parser;

use lexer::Token;

fn main() {
    let input = vec![
        Token::LParen,
        Token::Integer(120),
        Token::Plus,
        Token::Integer(50),
        Token::RParen,
        Token::Star,
        Token::Integer(3)];
    let expr = parser::parse(&input);

    println!("Hello, world: {:?}", expr);
}
