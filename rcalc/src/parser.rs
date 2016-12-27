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

impl Expr {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn plus(left: Expr, right: Expr) -> Expr {
        Expr::Plus(left.boxed(), right.boxed())
    }

    fn minus(left: Expr, right: Expr) -> Expr {
        Expr::Minus(left.boxed(), right.boxed())
    }

    fn times(left: Expr, right: Expr) -> Expr {
        Expr::Times(left.boxed(), right.boxed())
    }

    fn div(left: Expr, right: Expr) -> Expr {
        Expr::Div(left.boxed(), right.boxed())
    }
}

pub fn parse(input: &Vec<Token>) -> Expr {
    Expr::plus(Expr::Integer(100), Expr::Integer(50))
}
