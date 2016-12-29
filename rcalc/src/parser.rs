use lexer::Token;

#[derive(Debug, PartialEq)]
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
    fn next<'s>(&'s mut self) -> &'a Token {
        if self.index >= self.input.len() {
            &EOF
        } else {
            let token = &self.input[self.index];
            self.index = self.index + 1;
            token
        }
    }

    fn current<'s>(&'s self) -> &'a Token {
        let token =
            if self.index >= self.input.len() {
                &EOF
            } else {
                &self.input[self.index]
            };
        println!("current: {:?}", token);
        token
    }

    fn assert_current<'s>(&'s mut self, token: Token) {
        if token != *self.current() {
            panic!("Expected {:?}", token);
        }
    }

    fn parse_term<'s>(&'s mut self) -> Expr {
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

    fn parse_product<'s>(&'s mut self) -> Expr {
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

    fn parse_atom<'s>(&'s mut self) -> Expr {
        let expr = match *self.current() {
            Token::Integer(n) => Expr::Integer(n),
            Token::Ident(ref s) => Expr::Ident(s.clone()),
            Token::LParen => {
                self.next();
                let inner = self.parse_term();
                self.assert_current(Token::RParen);
                inner
            },
            _ => panic!("Expected atom"),
        };

        self.next();
        expr
    }
}
