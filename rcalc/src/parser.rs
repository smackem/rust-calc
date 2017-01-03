use lexer::Token;
use value::Value;

/// Defines the syntax elements that make up the AST.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IntDiv(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Ident(String),
    Literal(Value),
}

impl Expr {
    /// Convenience method that returns a `Box` containing the `Expr`.
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

/// Parses the given input and returns the AST.
///
/// # Examples
///
/// ```
/// let input = vec![Token::Integer(1)];
/// let expr = parse(&input).unwrap();
/// assert_eq!(expr, Expr::Literal(Value::Integer(1)));
/// ```
pub fn parse(input: &Vec<Token>) -> Result<Expr, String> {
    let mut parser = Parser {
        input: input,
        index: 0,
    };
    parser.parse_expr()
}

/// Returns the passed `i64` packaged in a `Expr::Literal` expression.
/// Useful for testing.
pub fn integer_expr(n: i64) -> Expr {
    Expr::Literal(Value::Integer(n))
}

/// Returns the passed `f64` packaged in a `Expr::Literal` expression.
/// Useful for testing.
pub fn float_expr(f: f64) -> Expr {
    Expr::Literal(Value::Float(f))
}

// ============================================================================

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
            self.index += 1;
            token
        }
    }

    fn current<'s>(&'s self) -> &'a Token {
        let token = if self.index >= self.input.len() {
            &EOF
        } else {
            &self.input[self.index]
        };
        println!("current: {:?}", token);
        token
    }

    fn assert_current<'s>(&'s mut self, token: Token) -> Result<(), String> {
        if token != *self.current() {
            Result::Err(format!("Expected {:?}", token))
        } else {
            Ok(())
        }
    }

    fn parse_expr<'s>(&'s mut self) -> Result<Expr, String> {
        let expr = try!(self.parse_term());
        try!(self.assert_current(Token::Eof));
        Result::Ok(expr)
    }

    fn parse_term<'s>(&'s mut self) -> Result<Expr, String> {
        let left = try!(self.parse_product());

        let expr = match *self.current() {
            Token::Plus => {
                self.next();
                Expr::Plus(left.boxed(), try!(self.parse_term()).boxed())
            }
            Token::Minus => {
                self.next();
                Expr::Minus(left.boxed(), try!(self.parse_term()).boxed())
            }
            _ => left,
        };

        Result::Ok(expr)
    }

    fn parse_product<'s>(&'s mut self) -> Result<Expr, String> {
        let left = try!(self.parse_atom());

        let expr = match *self.current() {
            Token::Star => {
                self.next();
                Expr::Times(left.boxed(), try!(self.parse_product()).boxed())
            }
            Token::Slash => {
                self.next();
                Expr::Div(left.boxed(), try!(self.parse_product()).boxed())
            }
            Token::Backslash => {
                self.next();
                Expr::IntDiv(left.boxed(), try!(self.parse_product()).boxed())
            }
            Token::Percent => {
                self.next();
                Expr::Mod(left.boxed(), try!(self.parse_product()).boxed())
            }
            _ => left,
        };

        Result::Ok(expr)
    }

    fn parse_atom<'s>(&'s mut self) -> Result<Expr, String> {
        let expr_opt = match *self.current() {
            Token::Integer(n) => Some(integer_expr(n)),
            Token::Float(f) => Some(float_expr(f)),
            Token::Ident(ref s) => Some(Expr::Ident(s.clone())),
            Token::LParen => {
                self.next();
                let inner = try!(self.parse_term());
                try!(self.assert_current(Token::RParen));
                Some(inner)
            }
            _ => None,
        };

        if let Some(expr) = expr_opt {
            self.next();
            Result::Ok(expr)
        } else {
            Result::Err("Expected atom".to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Token;

    #[test]
    fn test_parse_atom() {
        // 1
        let input = vec![Token::Integer(1)];
        let expr = parse(&input).unwrap();
        assert_eq!(expr, integer_expr(1));
    }

    #[test]
    fn test_parse_simple() {
        // 1 + 2
        let input = vec![Token::Integer(1), Token::Plus, Token::Integer(2)];
        let expr = parse(&input).unwrap();
        assert_eq!(expr,
                   Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()));
    }

    #[test]
    fn test_parse_complex() {
        // (1 + 2) * (5 - x)
        let input = vec![Token::LParen,
                         Token::Integer(1),
                         Token::Plus,
                         Token::Integer(2),
                         Token::RParen,
                         Token::Star,
                         Token::LParen,
                         Token::Integer(5),
                         Token::Minus,
                         Token::Ident("x".to_string()),
                         Token::RParen];
        let expr = parse(&input).unwrap();
        assert_eq!(expr,
                   Expr::Times(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed())
                                   .boxed(),
                               Expr::Minus(integer_expr(5).boxed(),
                                           Expr::Ident("x".to_string()).boxed())
                                   .boxed()));
    }

    #[test]
    fn test_parse_multi_parens() {
        // (((1)) + (2))
        let input = vec![Token::LParen,
                         Token::LParen,
                         Token::LParen,
                         Token::Integer(1),
                         Token::RParen,
                         Token::RParen,
                         Token::Plus,
                         Token::LParen,
                         Token::Integer(2),
                         Token::RParen,
                         Token::RParen];
        let expr = parse(&input).unwrap();
        assert_eq!(expr,
                   Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()));
    }

    #[test]
    fn test_parse_fail_1() {
        // *
        let input = vec![Token::Star];
        assert!(parse(&input).is_err());
    }

    #[test]
    fn test_parse_fail_2() {
        // + 1
        let input = vec![Token::Plus, Token::Integer(1)];
        assert!(parse(&input).is_err());
    }

    #[test]
    fn test_parse_fail_3() {
        // 1 + -
        let input = vec![Token::Integer(1), Token::Plus, Token::Minus];
        assert!(parse(&input).is_err());
    }

    #[test]
    fn test_parse_fail_4() {
        // 1 + )
        let input = vec![Token::Integer(1), Token::Plus, Token::RParen];
        assert!(parse(&input).is_err());
    }

    #[test]
    fn test_parse_fail_5() {
        // (1 + 2
        let input = vec![Token::LParen, Token::Integer(1), Token::Plus, Token::Integer(2)];
        assert!(parse(&input).is_err());
    }
}
