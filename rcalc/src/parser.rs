use lexer::Token;

#[derive(Debug)]
#[derive(PartialEq)]
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
    let mut parser = Parser { input: input, index: 0 };
    parser.parse_term()
}

///////////////////////////////////////////////////////////////////////////////

impl Expr {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

static EOF: Token = Token::Eof;

struct Parser<'a> {
    input: &'a Vec<Token>,
    index: usize,
}

impl<'a> Parser<'a> {
    fn next(&mut self) -> &Token {
        if self.index >= self.input.len() {
            &EOF
        } else {
            let token = &self.input[self.index];
            self.index = self.index + 1;
            token
        }
    }

    fn current(&self) -> &Token {
        if self.index >= self.input.len() {
            &EOF
        } else {
            &self.input[self.index]
        }
    }

    fn expect(&mut self, token: Token) {
        if *self.next() != token {
            panic!("Expected {:?}", token);
        }
    }

    fn parse_term(&mut self) -> Expr {
        let left = self.parse_product();

        match *self.current() {
            Token::Plus => {
                self.next();
                Expr::Plus(left.boxed(), self.parse_term().boxed())
            },
            Token::Minus => {
                self.next();
                Expr::Minus(left.boxed(), self.parse_term().boxed())
            },
            _ => left,
        }
    }

    fn parse_product(&mut self) -> Expr {
        let left = self.parse_atom();

        match *self.current() {
            Token::Star => {
                self.next();
                Expr::Times(left.boxed(), self.parse_product().boxed())
            },
            Token::Slash => {
                self.next();
                Expr::Div(left.boxed(), self.parse_product().boxed())
            },
            Token::Percent => {
                self.next();
                Expr::Mod(left.boxed(), self.parse_product().boxed())
            },
            _ => left,
        }
    }

    fn parse_atom(&mut self) -> Expr {
        let expr = match *self.current() {
            Token::Integer(n) => Expr::Integer(n),
            Token::Ident(ref s) => Expr::Ident(s.clone()),
            _ => panic!("Expected atom"),
        };

        self.next();
        expr
    }
}
