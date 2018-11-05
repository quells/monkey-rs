#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Meta
    Illegal,
    EOF,

    // Identifiers and literals
    Identifier, // [A-z][A-z0-9_]*
    Integer,    // [0-9]+

    // Operators
    Assign,             // =
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Exclamation,        // !
    EqualTo,            // ==
    NotEqualTo,         // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=
    
    // Delimiters
    Comma,     // ,
    Semicolon, // ;

    // Brackets
    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Clone)]
pub struct Token {
    pub literal: String,
    pub kind: TokenKind,
    pub line: usize,
    pub character: usize,
}

impl Token {
    fn new(literal: &str, kind: TokenKind, line: usize, character: usize) -> Token {
        Token {
            literal: literal.to_owned(),
            kind: kind,
            line: line,
            character: character,
        }
    }

    #[allow(dead_code)]
    fn basic(literal: &str, kind: TokenKind) -> Token {
        Token {
            literal: literal.to_owned(),
            kind: kind,
            line: 0,
            character: 0,
        }
    }

    fn is_equivalent_to(&self, other: &Token) -> bool {
        self.literal == other.literal && self.kind == other.kind
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let display_text = match self.kind {
            TokenKind::Identifier => format!("ID `{}`", self.literal),
            TokenKind::Integer => format!("{}", self.literal),
            TokenKind::Illegal => format!("Illegal {:?}", self.literal),
            _ => format!("{:?}", self.kind),
        };

        match self.kind {
            TokenKind::EOF => write!(f, "EOF"),
            _ => write!(f, "[ {} {}:{} ]", display_text, self.line, self.character),
        }
    }
}

use std::collections::HashMap;

struct Lexer {
    src: Vec<char>,
    len: usize,
    cursor: usize,
    line_number: usize,
    char_number: usize,
    special_char_tokens: HashMap<char, Vec<(Option<char>, TokenKind)>>,
    keywords: HashMap<String, TokenKind>,
}

fn option_eq<T>(a: Option<T>, b: Option<T>) -> bool
where
    T: PartialEq
{
    match (a, b) {
        (Some(l), Some(r)) => l == r,
        (None, None) => true,
        _ => false,
    }
}

impl Lexer {
    fn new(src: &str) -> Lexer {
        let chars: Vec<char> = src.chars().collect();
        let len = (&chars).len();

        let mut single_chars = HashMap::new();
        for (c, t) in vec![
            ('=', vec![(None, TokenKind::Assign), (Some('='), TokenKind::EqualTo)]),
            ('+', vec![(None, TokenKind::Plus)]),
            ('-', vec![(None, TokenKind::Minus)]),
            ('*', vec![(None, TokenKind::Asterisk)]),
            ('/', vec![(None, TokenKind::Slash)]),
            ('!', vec![(None, TokenKind::Exclamation), (Some('='), TokenKind::NotEqualTo)]),
            ('<', vec![(None, TokenKind::LessThan), (Some('='), TokenKind::LessThanOrEqual)]),
            ('>', vec![(None, TokenKind::GreaterThan), (Some('='), TokenKind::GreaterThanOrEqual)]),

            (',', vec![(None, TokenKind::Comma)]),
            (';', vec![(None, TokenKind::Semicolon)]),
            
            ('(', vec![(None, TokenKind::LParen)]),
            (')', vec![(None, TokenKind::RParen)]),
            ('{', vec![(None, TokenKind::LBrace)]),
            ('}', vec![(None, TokenKind::RBrace)]),
        ] {
            single_chars.insert(c, t);
        }

        let mut keywords = HashMap::new();
        for (s, t) in vec![
            ("let", TokenKind::Let),
            ("fn", TokenKind::Function),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("return", TokenKind::Return),
            ("true", TokenKind::True),
            ("false", TokenKind::False),
        ] {
            keywords.insert(s.to_owned(), t);
        }
        
        Lexer {
            src: chars,
            len: len,
            cursor: 0,
            line_number: 1,
            char_number: 0,
            special_char_tokens: single_chars,
            keywords: keywords,
        }
    }

    fn read(&mut self) -> Option<char> {
        if self.cursor < self.len {
            let c = self.src[self.cursor];
            self.cursor += 1;

            if c == '\n' {
                self.line_number += 1;
                self.char_number = 0;
            }
            self.char_number += 1;
            
            Some(c)
        } else {
            None
        }
    }

    fn peek(&self) -> Option<char> {
        if self.cursor >= self.len {
            None
        } else {
            Some(self.src[self.cursor])
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        let mut literal = String::new();
        let mut kind = TokenKind::EOF;
        let mut start_line = self.line_number;
        let mut start_char = self.char_number;
        
        loop {
            let c = match self.read() {
                Some(c) => c,
                None => break,
            };
            if c.is_whitespace() {
                if literal.len() == 0 {
                    continue;
                }
                break;
            }

            if literal.len() == 0 {
                start_line = self.line_number;
                start_char = self.char_number;
            }

            literal += &c.to_string();

            let temp_char_map = self.special_char_tokens.clone();
            let single_char_t = temp_char_map.get(&c);
            if single_char_t.is_some() {
                let possible = single_char_t.unwrap();
                let next_c = self.peek();
                let default = possible.into_iter()
                    .filter(|(pc, _)| pc.is_none())
                    .map(|(_, t)| t)
                    .next()
                    .unwrap_or(&TokenKind::Illegal);
                
                match possible.into_iter()
                .filter(|(pc, _)| option_eq(*pc, next_c))
                .next() {
                    Some((pc, t)) => {
                        if pc.is_some() {
                            literal += &self.read().unwrap().to_string();
                        }
                        kind = *t;
                    },
                    None => {
                        kind = *default;
                    },
                }
                break;
            }

            if c.is_ascii_digit() {
                if kind == TokenKind::EOF {
                    kind = TokenKind::Integer;
                }
                if !self.peek().unwrap_or('\0').is_ascii_digit() {
                    break
                }
            }

            if c.is_alphabetic() {
                if kind == TokenKind::EOF {
                    kind = TokenKind::Identifier;
                }
                if !self.peek().unwrap_or('\0').is_alphanumeric() {
                    break
                }
            }

            match kind {
                TokenKind::EOF => {
                    kind = TokenKind::Illegal;
                    break;
                },
                _ => (),
            }
        }

        let default = Token::new(&literal, kind, start_line, start_char);

        match kind {
            TokenKind::EOF => None,
            TokenKind::Identifier => {
                match self.keywords.get(&literal) {
                    Some(t) => Some(Token::new(&literal, *t, start_line, start_char)),
                    None => Some(default),
                }
            },
            _ => Some(default),
        }
    }
}

pub fn lex(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut lexer = Lexer::new(&src);
    loop {
        match lexer.next_token() {
            Some(t) => tokens.push(t),
            None => break,
        }
    }
    tokens.push(Token::new("", TokenKind::EOF, lexer.line_number, lexer.char_number));
    tokens
}

mod test {
    use crate::lex::{option_eq, lex, Token, TokenKind};

#[test]
fn test_option_eq() {
    let test_vectors = vec![
        (Some('a'), Some('a'), true),
        (Some('a'), None, false),
        (None, Some('a'), false),
        (None, None, true),
    ];
    for test_vector in test_vectors {
        let (a, b, expected) = test_vector;
        let actual = option_eq(a, b);
        let op = match expected {
            true => "==",
            false => "!=",
        };
        let msg = format!("Expected {:?} {} {:?}", a, op, b);
        assert_eq!(expected, actual, "{}", msg);
    }
}

#[allow(dead_code)]
fn test_comparisons(src: &str, expected: &[Token], actual: &[Token]) {
    let err_msg = format!("Unexpected number of tokens emitted from `{}`: expected {:?} found {:?}", src, expected, actual);
    assert_eq!(expected.len(), actual.len(), "{}", err_msg);
    for (e, a) in expected.into_iter().zip(actual) {
        let err_msg = format!("Expected {:?} found {:?} in `{}`", e, a, src);
        assert!(e.is_equivalent_to(&a), err_msg);
    }
}

#[test]
fn test_lex_specials() {
    let test_vectors: Vec<(String, Vec<Token>)> = vec![
        ("".to_owned(), vec![
            Token::basic("", TokenKind::EOF),
        ]),
        ("=+-*/!,;(){}<> == != <= >=".to_owned(), vec![
            ("=", TokenKind::Assign),
            ("+", TokenKind::Plus),
            ("-", TokenKind::Minus),
            ("*", TokenKind::Asterisk),
            ("/", TokenKind::Slash),
            ("!", TokenKind::Exclamation),
            (",", TokenKind::Comma),
            (";", TokenKind::Semicolon),
            ("(", TokenKind::LParen),
            (")", TokenKind::RParen),
            ("{", TokenKind::LBrace),
            ("}", TokenKind::RBrace),
            ("<", TokenKind::LessThan),
            (">", TokenKind::GreaterThan),
            ("==", TokenKind::EqualTo),
            ("!=", TokenKind::NotEqualTo),
            ("<=", TokenKind::LessThanOrEqual),
            (">=", TokenKind::GreaterThanOrEqual),
            ("", TokenKind::EOF),
        ].into_iter().map(|(c, t)| Token::basic(c, t)).collect()),
        // Possible two-character token is last character
        ("!".to_owned(), vec![
            Token::basic("!", TokenKind::Exclamation),
            Token::basic("", TokenKind::EOF),
        ]),
        ("!=".to_owned(), vec![
            Token::basic("!=", TokenKind::NotEqualTo),
            Token::basic("", TokenKind::EOF),
        ]),
    ];

    for test_vector in test_vectors {
        let (src, expected) = test_vector;
        let actual = lex(&src);
        test_comparisons(&src, &expected, &actual);
    }
}


#[test]
fn test_lex_keywords() {
    let test_vectors: Vec<(String, Vec<Token>)> = vec![
        ("let fn if else return true false".to_owned(), vec![
            ("let", TokenKind::Let),
            ("fn", TokenKind::Function),
            ("if", TokenKind::If),
            ("else", TokenKind::Else),
            ("return", TokenKind::Return),
            ("true", TokenKind::True),
            ("false", TokenKind::False),
            ("", TokenKind::EOF),
        ].into_iter().map(|(c, t)| Token::basic(c, t)).collect()),
    ];

    for test_vector in test_vectors {
        let (src, expected) = test_vector;
        let actual = lex(&src);
        test_comparisons(&src, &expected, &actual);
    }
}

#[test]
fn test_lex_integer() {
    let test_vectors: Vec<(String, Vec<Token>)> = vec![
        ("5 10 123".to_owned(), vec![
            ("5", TokenKind::Integer),
            ("10", TokenKind::Integer),
            ("123", TokenKind::Integer),
            ("", TokenKind::EOF),
        ].into_iter().map(|(c, t)| Token::basic(c, t)).collect()),
    ];

    for test_vector in test_vectors {
        let (src, expected) = test_vector;
        let actual = lex(&src);
        test_comparisons(&src, &expected, &actual);
    }
}

#[test]
fn test_lex_func() {
    let src = r#"
let add = fn(x, y) {
    x + y;
};"#;
    let expected: Vec<Token> = vec![
        ("let", TokenKind::Let),
        ("add", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("fn", TokenKind::Function),
        ("(", TokenKind::LParen),
        ("x", TokenKind::Identifier),
        (",", TokenKind::Comma),
        ("y", TokenKind::Identifier),
        (")", TokenKind::RParen),
        ("{", TokenKind::LBrace),
        ("x", TokenKind::Identifier),
        ("+", TokenKind::Plus),
        ("y", TokenKind::Identifier),
        (";", TokenKind::Semicolon),
        ("}", TokenKind::RBrace),
        (";", TokenKind::Semicolon),
        ("", TokenKind::EOF),
    ].into_iter().map(|(c, t)| Token::basic(c, t)).collect();
    let actual = lex(&src);
    test_comparisons(&src, &expected, &actual);
}

#[test]
fn test_lex_func_call() {
    let src = "let result = add(five, ten);";
    let expected: Vec<Token> = vec![
        ("let", TokenKind::Let),
        ("result", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("add", TokenKind::Identifier),
        ("(", TokenKind::LParen),
        ("five", TokenKind::Identifier),
        (",", TokenKind::Comma),
        ("ten", TokenKind::Identifier),
        (")", TokenKind::RParen),
        (";", TokenKind::Semicolon),
        ("", TokenKind::EOF),
    ].into_iter().map(|(c, t)| Token::basic(c, t)).collect();
    let actual = lex(&src);
    test_comparisons(&src, &expected, &actual);
}

#[test]
fn test_lex() {
    let src = r#"
let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);

10 == 10;
10!=9;

if (5 < 10) {
    return true;
} else {
    return false;
}
"#;
    let expected: Vec<Token> = vec![
        ("let", TokenKind::Let),
        ("five", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("5", TokenKind::Integer),
        (";", TokenKind::Semicolon),

        ("let", TokenKind::Let),
        ("ten", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("10", TokenKind::Integer),
        (";", TokenKind::Semicolon),

        ("let", TokenKind::Let),
        ("add", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("fn", TokenKind::Function),
        ("(", TokenKind::LParen),
        ("x", TokenKind::Identifier),
        (",", TokenKind::Comma),
        ("y", TokenKind::Identifier),
        (")", TokenKind::RParen),
        ("{", TokenKind::LBrace),
        ("x", TokenKind::Identifier),
        ("+", TokenKind::Plus),
        ("y", TokenKind::Identifier),
        (";", TokenKind::Semicolon),
        ("}", TokenKind::RBrace),
        (";", TokenKind::Semicolon),

        ("let", TokenKind::Let),
        ("result", TokenKind::Identifier),
        ("=", TokenKind::Assign),
        ("add", TokenKind::Identifier),
        ("(", TokenKind::LParen),
        ("five", TokenKind::Identifier),
        (",", TokenKind::Comma),
        ("ten", TokenKind::Identifier),
        (")", TokenKind::RParen),
        (";", TokenKind::Semicolon),

        ("10", TokenKind::Integer),
        ("==", TokenKind::EqualTo),
        ("10", TokenKind::Integer),
        (";", TokenKind::Semicolon),
        ("10", TokenKind::Integer),
        ("!=", TokenKind::NotEqualTo),
        ("9", TokenKind::Integer),
        (";", TokenKind::Semicolon),

        ("if", TokenKind::If),
        ("(", TokenKind::LParen),
        ("5", TokenKind::Integer),
        ("<", TokenKind::LessThan),
        ("10", TokenKind::Integer),
        (")", TokenKind::RParen),
        ("{", TokenKind::LBrace),
        ("return", TokenKind::Return),
        ("true", TokenKind::True),
        (";", TokenKind::Semicolon),
        ("}", TokenKind::RBrace),
        ("else", TokenKind::Else),
        ("{", TokenKind::LBrace),
        ("return", TokenKind::Return),
        ("false", TokenKind::False),
        (";", TokenKind::Semicolon),
        ("}", TokenKind::RBrace),
        
        ("", TokenKind::EOF),
    ].into_iter().map(|(c, t)| Token::basic(c, t)).collect();
    let actual = lex(&src);
    test_comparisons(&src, &expected, &actual);
}

}