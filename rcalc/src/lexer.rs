pub enum Token {
    LParen,
    RParen,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Integer(i32),
    Ident(String),
}
