use lexer::Token;

pub enum Expr {
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Ident(String),
    Integer(i32),
}

pub fn parse(input: &Vec<Token>) -> Expr {
    Expr::Plus(Expr::Integer(100).boxed(), Expr::Integer(50).boxed())
}

///////////////////////////////////////////////////////////////////////////////

impl Expr {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

struct Parser {
    input: Vec<Token>,
    index: usize,
}

impl Parser {
    fn next(&mut self) -> &Token {
        let token = &self.input[self.index];
        self.index = self.index + 1;
        token
    }

    fn current(&self) -> &Token {
        &self.input[self.index]
    }

    fn parseTerm(&mut self) -> Expr {
        Expr::Integer(100)
    }
}
