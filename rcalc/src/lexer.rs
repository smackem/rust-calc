use regex::Regex;

/// Defines the terminals that can be lexed.
#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    Plus,
    Minus,
    Star,
    StarStar,
    Slash,
    Backslash,
    Percent,
    Comma,
    Eq,
    Ampersand,
    VBar,
    Caret,
    LtLt,
    GtGt,
    Log,
    Sqrt,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Integer(i64),
    Float(f64),
    Ident(String),
    Let,
    Eof,
}

/// Tokenizes the given input string and returns a vector of `Token`s.
///
/// # Examples
///
/// ```
/// let tokens = lex("1 + 2");
/// assert_eq!(tokens, vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
/// ```
pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    let map = token_map();
    let mut tokens = Vec::new();
    let mut index: usize = 0;
    let input = input.trim();

    'outer: while index < input.len() {
        let input_slice = &input[index..];

        for &(ref regex, ref f) in &map {
            if let Some((0, len)) = regex.find(input_slice) {
                let token = f(&input_slice[0..len]);
                tokens.push(token);
                index += len;
                continue 'outer;
            }
        }

        return Result::Err(format!("Could not lex symbol: {}", input_slice));
    }

    Result::Ok(tokens)
}

// ============================================================================

fn token_map() -> Vec<(Regex, Box<Fn(&str) -> Token>)> {
    let create_regex = |s| Regex::new(s).unwrap();
    vec![(create_regex(r"^\s*-?\d+\.\d+"), Box::new(|s| Token::Float(s.trim().parse::<f64>().unwrap()))),
         (create_regex(r"^\s*-?\d+"), Box::new(|s| Token::Integer(s.trim().parse::<i64>().unwrap()))),
         (create_regex(r"^\s*\("), Box::new(|_| Token::LParen)),
         (create_regex(r"^\s*\)"), Box::new(|_| Token::RParen)),
         (create_regex(r"^\s*\+"), Box::new(|_| Token::Plus)),
         (create_regex(r"^\s*-"), Box::new(|_| Token::Minus)),
         (create_regex(r"^\s*\*\*"), Box::new(|_| Token::StarStar)),
         (create_regex(r"^\s*\*"), Box::new(|_| Token::Star)),
         (create_regex(r"^\s*/"), Box::new(|_| Token::Slash)),
         (create_regex(r"^\s*\\"), Box::new(|_| Token::Backslash)),
         (create_regex(r"^\s*%"), Box::new(|_| Token::Percent)),
         (create_regex(r"^\s*,"), Box::new(|_| Token::Comma)),
         (create_regex(r"^\s*="), Box::new(|_| Token::Eq)),
         (create_regex(r"^\s*&"), Box::new(|_| Token::Ampersand)),
         (create_regex(r"^\s*\|"), Box::new(|_| Token::VBar)),
         (create_regex(r"^\s*\^"), Box::new(|_| Token::Caret)),
         (create_regex(r"^\s*<<"), Box::new(|_| Token::LtLt)),
         (create_regex(r"^\s*>>"), Box::new(|_| Token::GtGt)),
         (create_regex(r"^\s*log\b"), Box::new(|_| Token::Log)),
         (create_regex(r"^\s*sqrt\b"), Box::new(|_| Token::Sqrt)),
         (create_regex(r"^\s*sin\b"), Box::new(|_| Token::Sin)),
         (create_regex(r"^\s*cos\b"), Box::new(|_| Token::Cos)),
         (create_regex(r"^\s*tan\b"), Box::new(|_| Token::Tan)),
         (create_regex(r"^\s*asin\b"), Box::new(|_| Token::Asin)),
         (create_regex(r"^\s*acos\b"), Box::new(|_| Token::Acos)),
         (create_regex(r"^\s*atan\b"), Box::new(|_| Token::Atan)),
         (create_regex(r"^\s*let\b"), Box::new(|_| Token::Let)),
         (create_regex(r"^\s*[a-zA-z]+"), Box::new(|s| Token::Ident(s.trim().to_string())))]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_simple() {
        let tokens = lex("1+2").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_whitespace() {
        let tokens = lex(" \t 1 + \t\r\n    2 \r\n ").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_big_nums() {
        let tokens = lex("1002 + 123456789").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1002), Token::Plus, Token::Integer(123456789)]);
    }

    #[test]
    fn test_lex_with_ident() {
        let tokens = lex("abc 100").unwrap();
        assert_eq!(tokens,
                   vec![Token::Ident("abc".to_string()), Token::Integer(100)]);
    }

    #[test]
    fn test_lex_with_negative_integer() {
        let tokens = lex("- -123").unwrap();
        assert_eq!(tokens,
                   vec![Token::Minus, Token::Integer(-123)]);
    }

    #[test]
    fn test_lex_float() {
        let tokens = lex("1.0 -123.5 1003.125").unwrap();
        assert_eq!(tokens,
                   vec![Token::Float(1.0), Token::Float(-123.5), Token::Float(1003.125)]);
    }

    #[test]
    fn test_lex_all_tokens() {
        let tokens = lex(r"( ) + - * / \ % , = 100 1.25 abc let ** & | ^ << >> log sqrt sin cos tan asin acos atan").unwrap();
        assert_eq!(tokens,
                   vec![Token::LParen,
                        Token::RParen,
                        Token::Plus,
                        Token::Minus,
                        Token::Star,
                        Token::Slash,
                        Token::Backslash,
                        Token::Percent,
                        Token::Comma,
                        Token::Eq,
                        Token::Integer(100),
                        Token::Float(1.25),
                        Token::Ident("abc".to_string()),
                        Token::Let,
                        Token::StarStar,
                        Token::Ampersand,
                        Token::VBar,
                        Token::Caret,
                        Token::LtLt,
                        Token::GtGt,
                        Token::Log,
                        Token::Sqrt,
                        Token::Sin,
                        Token::Cos,
                        Token::Tan,
                        Token::Asin,
                        Token::Acos,
                        Token::Atan,
                        ]);
    }

    #[test]
    fn test_lex_unknown_token() {
        assert!(lex("123 < 234").is_err()); // '<' is unknown
    }
}
