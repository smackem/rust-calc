use regex::Regex;

#[derive(Debug, PartialEq)]
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
    Eof,
}

pub fn lex(input: &str) -> Vec<Token> {
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
                index = index + len;
                continue 'outer;
            }
        }

        panic!("Could not lex symbol: {}", input_slice);
    }
    tokens
}

///////////////////////////////////////////////////////////////////////////////

fn token_map() -> Vec<(Regex, Box<Fn(&str) -> Token>)> {
    let create_regex = |s| Regex::new(s).unwrap();
    vec![
        (create_regex(r"^\s*\("), Box::new(|_| Token::LParen)),
        (create_regex(r"^\s*\)"), Box::new(|_| Token::RParen)),
        (create_regex(r"^\s*\+"), Box::new(|_| Token::Plus)),
        (create_regex(r"^\s*-"), Box::new(|_| Token::Minus)),
        (create_regex(r"^\s*\*"), Box::new(|_| Token::Star)),
        (create_regex(r"^\s*/"), Box::new(|_| Token::Slash)),
        (create_regex(r"^\s*%"), Box::new(|_| Token::Percent)),
        (create_regex(r"^\s*\d+"), Box::new(|s| Token::Integer(s.trim().parse::<i32>().unwrap()))),
        (create_regex(r"^\s*[a-zA-z]+"), Box::new(|s| Token::Ident(s.trim().to_string())))]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_simple() {
        let tokens = lex("1+2");
        assert_eq!(tokens, vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_whitespace() {
        let tokens = lex(" \t 1 + \t\r\n    2 \r\n ");
        assert_eq!(tokens, vec![Token::Integer(1), Token::Plus, Token::Integer(2)]);
    }

    #[test]
    fn test_lex_with_big_nums() {
        let tokens = lex("1002 + 123456789");
        assert_eq!(tokens, vec![Token::Integer(1002), Token::Plus, Token::Integer(123456789)]);
    }

    #[test]
    fn test_lex_with_ident() {
        let tokens = lex("abc 100");
        assert_eq!(tokens, vec![Token::Ident("abc".to_string()), Token::Integer(100)]);
    }

    #[test]
    fn test_lex_all_tokens() {
        let tokens = lex("( ) + - * / % 100 abc");
        assert_eq!(tokens, vec![
            Token::LParen, Token::RParen, Token::Plus, Token::Minus, Token::Star, Token::Slash,
            Token::Percent, Token::Integer(100), Token::Ident("abc".to_string())]);
    }

    #[test]
    #[should_panic]
    fn test_lex_unknown_token() {
        let tokens = lex("123 < 234"); // '<' is unknown
        assert_eq!(tokens, vec![Token::Ident("abc".to_string()), Token::Integer(100)]);
    }
}
