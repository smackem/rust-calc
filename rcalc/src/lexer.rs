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
    At,
    DotDot,
    LBracket,
    RBracket,
    Colon,
    DotWord,
    Len,
    Count,
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

pub struct Lexer {
    token_map: Vec<(Regex, Box<Fn(&str) -> Token>)>,
}

impl Lexer {
    pub fn new() -> Lexer {
        Lexer { token_map: create_token_map() }
    }

    /// Tokenizes the given input string and returns a vector of `Token`s.
    pub fn lex(&self, input: &str) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let mut index: usize = 0;
        let input = input.trim();

        'outer: while index < input.len() {
            let input_slice = &input[index..];

            for &(ref regex, ref f) in &self.token_map {
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
}

// ============================================================================

fn create_token_map() -> Vec<(Regex, Box<Fn(&str) -> Token>)> {
    let create_regex = |s| Regex::new(s).unwrap();
    vec![(create_regex(r"^\s*\d+\.\d+"), Box::new(|s| Token::Float(s.trim().parse::<f64>().unwrap()))),
         (create_regex(r"^\s*0x[0-9a-fA-F][_0-9a-fA-F]*"), Box::new(|s| {
                     let mut buf = String::new();
                     strip_string_from_index(s.trim(), '_', 2, &mut buf);
                     Token::Integer(i64::from_str_radix(&buf, 16).unwrap())
                 })),
         (create_regex(r"^\s*0b[01][_01]*"), Box::new(|s| {
                     let mut buf = String::new();
                     strip_string_from_index(s.trim(), '_', 2, &mut buf);
                     Token::Integer(i64::from_str_radix(&buf, 2).unwrap())
                 })),
         (create_regex(r"^\s*\d[_\d]*"), Box::new(|s| {
                     let mut buf = String::new();
                     strip_string_from_index(s.trim(), '_', 0, &mut buf);
                     Token::Integer(buf.parse::<i64>().unwrap())
                 })),
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
         (create_regex(r"^\s*\.\."), Box::new(|_| Token::DotDot)),
         (create_regex(r"^\s*:"), Box::new(|_| Token::Colon)),
         (create_regex(r"^\s*@"), Box::new(|_| Token::At)),
         (create_regex(r"^\s*\["), Box::new(|_| Token::LBracket)),
         (create_regex(r"^\s*\]"), Box::new(|_| Token::RBracket)),
         (create_regex(r"^\s*log\b"), Box::new(|_| Token::Log)),
         (create_regex(r"^\s*sqrt\b"), Box::new(|_| Token::Sqrt)),
         (create_regex(r"^\s*sin\b"), Box::new(|_| Token::Sin)),
         (create_regex(r"^\s*cos\b"), Box::new(|_| Token::Cos)),
         (create_regex(r"^\s*tan\b"), Box::new(|_| Token::Tan)),
         (create_regex(r"^\s*asin\b"), Box::new(|_| Token::Asin)),
         (create_regex(r"^\s*acos\b"), Box::new(|_| Token::Acos)),
         (create_regex(r"^\s*atan\b"), Box::new(|_| Token::Atan)),
         (create_regex(r"^\s*let\b"), Box::new(|_| Token::Let)),
         (create_regex(r"^\s*dot\b"), Box::new(|_| Token::DotWord)),
         (create_regex(r"^\s*len\b"), Box::new(|_| Token::Len)),
         (create_regex(r"^\s*count\b"), Box::new(|_| Token::Count)),
         (create_regex(r"^\s*[a-zA-Z]+"), Box::new(|s| Token::Ident(s.trim().to_string())))]
}

fn strip_string_from_index(s: &str, ch_to_remove: char, start_index: usize, buffer: &mut String) {
    for ch in s.chars().skip(start_index) {
        if ch != ch_to_remove {
            buffer.push(ch);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_lex_simple() {
        let tokens = Lexer::new().lex("1+2").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_whitespace() {
        let tokens = Lexer::new().lex(" \t 1 + \t\r\n    2 \r\n ").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_big_nums() {
        let tokens = Lexer::new().lex("1_002 + 123_456_789").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(1002), Token::Plus, Token::Integer(123_456_789)]);
    }

    #[test]
    fn test_lex_hex() {
        let tokens = Lexer::new().lex("0xAB12_FFD4_CE01").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(0xAB12_FFD4_CE01)]);
    }

    #[test]
    fn test_lex_bin() {
        let tokens = Lexer::new().lex("0b1001_1100_0101_0011").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(0b1001_1100_0101_0011)]);
    }

    #[test]
    fn test_lex_with_ident() {
        let tokens = Lexer::new().lex("abc 100").unwrap();
        assert_eq!(tokens,
                   vec![Token::Ident("abc".to_string()), Token::Integer(100)]);
    }

    #[test]
    fn test_lex_with_negative_integer() {
        let tokens = Lexer::new().lex("- -123").unwrap();
        assert_eq!(tokens,
                   vec![Token::Minus, Token::Minus, Token::Integer(123)]);
    }

    #[test]
    fn test_lex_with_negative_float() {
        let tokens = Lexer::new().lex("-123.25").unwrap();
        assert_eq!(tokens,
                   vec![Token::Minus, Token::Float(123.25)]);
    }

    #[test]
    fn test_lex_float() {
        let tokens = Lexer::new().lex("1.0 123.5 1003.125").unwrap();
        assert_eq!(tokens,
                   vec![Token::Float(1.0), Token::Float(123.5), Token::Float(1003.125)]);
    }

    #[test]
    fn test_lex_all_tokens() {
        let lexer = Lexer::new();
        let tokens = lexer.lex(r"( ) + - * / \ % , =").unwrap();
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
                        ]);
        let tokens = lexer.lex(r"100 1.25 abc let ** & | ^ << >>").unwrap();
        assert_eq!(tokens,
                   vec![Token::Integer(100),
                        Token::Float(1.25),
                        Token::Ident("abc".to_string()),
                        Token::Let,
                        Token::StarStar,
                        Token::Ampersand,
                        Token::VBar,
                        Token::Caret,
                        Token::LtLt,
                        Token::GtGt,
                        ]);
        let tokens = lexer.lex(r"log sqrt sin cos tan asin acos atan").unwrap();
        assert_eq!(tokens,
                   vec![Token::Log,
                        Token::Sqrt,
                        Token::Sin,
                        Token::Cos,
                        Token::Tan,
                        Token::Asin,
                        Token::Acos,
                        Token::Atan,
                        ]);
        let tokens = lexer.lex(r"[ ] .. : dot len count").unwrap();
        assert_eq!(tokens,
                   vec![Token::LBracket,
                        Token::RBracket,
                        Token::DotDot,
                        Token::Colon,
                        Token::DotWord,
                        Token::Len,
                        Token::Count,
                        ]);
    }

    #[test]
    fn test_lex_unknown_token() {
        assert!(Lexer::new().lex("123 < 234").is_err()); // '<' is unknown
    }
}
