use lexer::Token;
use value::Value;

/// Defines the syntax elements that can be the AST root.
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// A variable binding with the lhs identifier and the rhs expression
    VarBind(String, Expr),

    /// A function binding with the identifier of the function, the identifiers
    /// of the parameters and the rhs expression
    FunBind(String, Vec<String>, Expr),

    /// An evaluation statement that consists of an expression
    Eval(Expr),
}

/// Defines the syntax elements that make up the expression part of the AST.
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    BitwiseAnd(Box<Expr>, Box<Expr>),
    BitwiseOr(Box<Expr>, Box<Expr>),
    BitwiseXor(Box<Expr>, Box<Expr>),
    Times(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    IntDiv(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    ShiftLeft(Box<Expr>, Box<Expr>),
    ShiftRight(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
    Log(Box<Expr>, Box<Expr>),
    Sqrt(Box<Expr>),
    Sin(Box<Expr>),
    Cos(Box<Expr>),
    Tan(Box<Expr>),
    Asin(Box<Expr>),
    Acos(Box<Expr>),
    Atan(Box<Expr>),
    BindingRef(String),
    FunCall(String, Box<Vec<Expr>>),
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
/// # Return value
///
/// Either `Result::Ok` with the parsed AST root or `Err` with an error message.
///
/// # Examples
///
/// ```
/// let input = vec![Token::Integer(1)];
/// let stmt = parse(&input).unwrap();
/// assert_eq!(stmt, Stmt::Eval(Expr::Literal(Value::Integer(1))));
/// ```
pub fn parse(input: &Vec<Token>) -> Result<Stmt, String> {
    let mut parser = Parser {
        input: input,
        index: 0,
    };
    parser.parse_stmt()
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

        trace!("current: {:?}", token);
        token
    }

    fn assert_current<'s>(&'s mut self, token: Token) -> Result<(), String> {
        if token != *self.current() {
            Result::Err(format!("Expected {:?}", token))
        } else {
            Ok(())
        }
    }

    // let f(a, b) = a + b
    // let x = 1
    // 1 + 1
    // ^
    fn parse_stmt<'s>(&'s mut self) -> Result<Stmt, String> {
        let stmt = match *self.current() {
            Token::Let => {
                self.next();
                try!(self.parse_binding())
            },
            _ => Stmt::Eval(try!(self.parse_expr())),
        };

        try!(self.assert_current(Token::Eof));
        Result::Ok(stmt)
    }

    // let f(a, b) = a + b
    // let x = 1
    //     ^
    fn parse_binding<'s>(&'s mut self) -> Result<Stmt, String> {
        let ident = match *self.next() {
            Token::Ident(ref s) => s,
            _ => try!(Result::Err("Expected identifier")),
        };

        match *self.current() {
            Token::LParen => {
                self.next();
                let (param_idents, expr) = try!(self.parse_fun_binding());
                Result::Ok(Stmt::FunBind(ident.clone(), param_idents, expr))
            },
            Token::Eq => {
                self.next();
                let expr = try!(self.parse_var_binding());
                Result::Ok(Stmt::VarBind(ident.clone(), expr))
            },
            _ => Err(format!("Expected {:?} or {:?}, found {:?}", Token::LParen, Token::Eq, self.current())),
        }
    }

    // let f() = 1
    // let f(a, b) = a + b
    //       ^
    fn parse_fun_binding<'s>(&'s mut self) -> Result<(Vec<String>, Expr), String> {
        let param_idents = match *self.current() {
            Token::RParen => {
                self.next();
                vec![]
            },
            Token::Ident(_) => {
                let idents = try!(self.parse_ident_list());
                try!(self.assert_current(Token::RParen));
                self.next();
                idents
            },
            _ => try!(Result::Err(format!("Expected identifier or {:?}", Token::RParen))),
        };

        try!(self.assert_current(Token::Eq));
        self.next();

        let expr = try!(self.parse_expr());
        Result::Ok((param_idents, expr))
    }

    // (a, b, xyz)
    //  ^
    fn parse_ident_list<'s>(&'s mut self) -> Result<Vec<String>, String> {
        let mut idents = Vec::new();
        loop {
            if let Token::Ident(ref ident) = *self.current() {
                idents.push(ident.clone());
                self.next();
            } else {
                try!(Result::Err("Expected identifier"));
            }

            match *self.current() {
                Token::Comma => self.next(),
                _ => break,
            };
        }

        Result::Ok(idents)
    }

    // let x = 1
    //         ^
    fn parse_var_binding<'s>(&'s mut self) -> Result<Expr, String> {
        self.parse_expr()
    }

    fn parse_expr<'s>(&'s mut self) -> Result<Expr, String> {
        self.parse_term()
    }

    fn parse_term<'s>(&'s mut self) -> Result<Expr, String> {
        let mut left = try!(self.parse_product());

        loop {
            match *self.current() {
                Token::Plus => {
                    self.next();
                    left = Expr::Plus(left.boxed(), try!(self.parse_product()).boxed())
                },
                Token::Minus => {
                    self.next();
                    left = Expr::Minus(left.boxed(), try!(self.parse_product()).boxed())
                },
                Token::Ampersand => {
                    self.next();
                    left = Expr::BitwiseAnd(left.boxed(), try!(self.parse_product()).boxed())
                },
                Token::VBar => {
                    self.next();
                    left = Expr::BitwiseOr(left.boxed(), try!(self.parse_product()).boxed())
                },
                Token::Caret => {
                    self.next();
                    left = Expr::BitwiseXor(left.boxed(), try!(self.parse_product()).boxed())
                },
                _ => break,
            };
        }

        Result::Ok(left)
    }

    fn parse_product<'s>(&'s mut self) -> Result<Expr, String> {
        let mut left = try!(self.parse_molecule());

        loop {
            match *self.current() {
                Token::Star => {
                    self.next();
                    left = Expr::Times(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                Token::Slash => {
                    self.next();
                    left = Expr::Div(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                Token::Backslash => {
                    self.next();
                    left = Expr::IntDiv(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                Token::Percent => {
                    self.next();
                    left = Expr::Mod(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                Token::LtLt => {
                    self.next();
                    left = Expr::ShiftLeft(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                Token::GtGt => {
                    self.next();
                    left = Expr::ShiftRight(left.boxed(), try!(self.parse_molecule()).boxed())
                },
                _ => break,
            };
        }

        Result::Ok(left)
    }

    fn parse_molecule<'s>(&'s mut self) -> Result<Expr, String> {
        let mut left = try!(self.parse_atom());

        loop {
            match *self.current() {
                Token::StarStar => {
                    self.next();
                    left = Expr::Pow(left.boxed(), try!(self.parse_atom()).boxed())
                },
                Token::Log => {
                    self.next();
                    left = Expr::Log(left.boxed(), try!(self.parse_atom()).boxed())
                },
                _ => break,
            };
        }

        Result::Ok(left)
    }

    // 1
    // f()
    // f(1)
    // ^
    fn parse_atom<'s>(&'s mut self) -> Result<Expr, String> {
        let expr = match *self.current() {
            Token::Integer(n) => {
                self.next();
                integer_expr(n)
            },
            Token::Float(f) => {
                self.next();
                float_expr(f)
            },
            Token::Ident(ref s) => {
                self.next();
                let ident = s.clone();
                match *self.current() {
                    Token::LParen => {
                        self.next();
                        let args = try!(self.parse_fun_call());
                        Expr::FunCall(ident, Box::new(args))
                    },
                    _ => Expr::BindingRef(ident),
                }
            },
            Token::Sqrt => {
                self.next();
                Expr::Sqrt(try!(self.parse_atom()).boxed())
            },
            Token::Sin => {
                self.next();
                Expr::Sin(try!(self.parse_atom()).boxed())
            },
            Token::Cos => {
                self.next();
                Expr::Cos(try!(self.parse_atom()).boxed())
            },
            Token::Tan => {
                self.next();
                Expr::Tan(try!(self.parse_atom()).boxed())
            },
            Token::Asin => {
                self.next();
                Expr::Asin(try!(self.parse_atom()).boxed())
            },
            Token::Acos => {
                self.next();
                Expr::Acos(try!(self.parse_atom()).boxed())
            },
            Token::Atan => {
                self.next();
                Expr::Atan(try!(self.parse_atom()).boxed())
            },
            Token::LParen => {
                self.next();
                let inner = try!(self.parse_term());
                try!(self.assert_current(Token::RParen));
                self.next();
                inner
            },
            _ => try!(Result::Err(format!("Expected atom, found {:?}", self.current()))),
        };

        Result::Ok(expr)
    }

    // 1
    // f()
    // f(1)
    //   ^
    fn parse_fun_call<'s>(&'s mut self) -> Result<Vec<Expr>, String> {
        let expr_list = match *self.current() {
            Token::RParen => {
                self.next();
                vec![]
            },
            _ => {
                let exprs = try!(self.parse_expr_list());
                try!(self.assert_current(Token::RParen));
                self.next();
                exprs
            },
        };

        Result::Ok(expr_list)
    }

    // 1 + 2
    // 1 + 2, a, 5.0 / x
    // ^
    fn parse_expr_list<'s>(&'s mut self) -> Result<Vec<Expr>, String> {
        let mut exprs = Vec::new();
        loop {
            exprs.push(try!(self.parse_expr()));

            match *self.current() {
                Token::Comma => self.next(),
                _ => break,
            };
        }

        Result::Ok(exprs)
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
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt, Stmt::Eval(integer_expr(1)));
    }

    #[test]
    fn test_parse_simple() {
        // 1 + 2
        let input = vec![Token::Integer(1), Token::Plus, Token::Integer(2)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed())));
    }

    #[test]
    fn test_parse_lassoc_term() {
        // 2 - 1 - 1 = (2 - 1) - 1
        let input = vec![Token::Integer(2), Token::Minus, Token::Integer(1), Token::Minus, Token::Integer(1)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(
                       Expr::Minus(
                           Expr::Minus(integer_expr(2).boxed(), integer_expr(1).boxed()).boxed(),
                           integer_expr(1).boxed())));
    }

    #[test]
    fn test_parse_lassoc_product() {
        // 12 / 2 / 2 = (12 / 2) / 2
        let input = vec![Token::Integer(12), Token::Slash, Token::Integer(2), Token::Slash, Token::Integer(2)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(
                       Expr::Div(
                           Expr::Div(integer_expr(12).boxed(), integer_expr(2).boxed()).boxed(),
                           integer_expr(2).boxed())));
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
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(
                       Expr::Times(
                           Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed()).boxed(),
                           Expr::Minus(integer_expr(5).boxed(), Expr::BindingRef("x".to_string()).boxed()).boxed())));
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
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::Plus(integer_expr(1).boxed(), integer_expr(2).boxed())));
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

    #[test]
    fn test_parse_fail_6() {
        // 1 2
        let input = vec![Token::Integer(1), Token::Integer(2)];
        assert!(parse(&input).is_err());
    }

    #[test]
    fn test_parse_variable_binding() {
        // let x = 1
        let input = vec![Token::Let, Token::Ident("x".to_string()), Token::Eq, Token::Integer(1)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::VarBind("x".to_string(), integer_expr(1)));
    }

    #[test]
    fn test_parse_function_binding_1() {
        // let f() = 1
        let input = vec![Token::Let,
                         Token::Ident("f".to_string()),
                         Token::LParen,
                         Token::RParen,
                         Token::Eq,
                         Token::Integer(1)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::FunBind("f".to_string(), vec![], integer_expr(1)));
    }

    #[test]
    fn test_parse_function_binding_2() {
        // let f(a) = 1
        let input = vec![Token::Let,
                         Token::Ident("f".to_string()),
                         Token::LParen,
                         Token::Ident("a".to_string()),
                         Token::RParen,
                         Token::Eq,
                         Token::Integer(1)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::FunBind("f".to_string(), vec!["a".to_string()], integer_expr(1)));
    }

    #[test]
    fn test_parse_function_binding_3() {
        // let f(a, b) = 1
        let input = vec![Token::Let,
                         Token::Ident("f".to_string()),
                         Token::LParen,
                         Token::Ident("a".to_string()),
                         Token::Comma,
                         Token::Ident("b".to_string()),
                         Token::RParen,
                         Token::Eq,
                         Token::Integer(1)];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::FunBind("f".to_string(), vec!["a".to_string(), "b".to_string()], integer_expr(1)));
    }
    
    #[test]
    fn test_parse_function_call_1() {
        // f()
        let input = vec![Token::Ident("f".to_string()), Token::LParen, Token::RParen];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::FunCall("f".to_string(), Box::new(vec![]))));
    }

    #[test]
    fn test_parse_function_call_2() {
        // f(1)
        let input = vec![Token::Ident("f".to_string()), Token::LParen, Token::Integer(1), Token::RParen];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::FunCall("f".to_string(), Box::new(vec![integer_expr(1)]))));
    }

    #[test]
    fn test_parse_function_call_3() {
        // f(1, 2)
        let input = vec![Token::Ident("f".to_string()),
                         Token::LParen,
                         Token::Integer(1),
                         Token::Comma,
                         Token::Integer(2),
                         Token::RParen];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::FunCall("f".to_string(),
                              Box::new(vec![integer_expr(1), integer_expr(2)]))));
    }

    #[test]
    fn test_parse_function_call_4() {
        // f(1, (2 + 3))
        let input = vec![Token::Ident("f".to_string()),
                         Token::LParen,
                         Token::Integer(1),
                         Token::Comma,
                         Token::LParen,
                         Token::Integer(2),
                         Token::Plus,
                         Token::Integer(3),
                         Token::RParen,
                         Token::RParen];
        let stmt = parse(&input).unwrap();
        assert_eq!(stmt,
                   Stmt::Eval(Expr::FunCall("f".to_string(),
                              Box::new(vec![integer_expr(1),
                                            Expr::Plus(integer_expr(2).boxed(), integer_expr(3).boxed())]))));
    }
}
